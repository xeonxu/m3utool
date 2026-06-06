(defpackage :m3u-check
  (:use :cl :m3u-data)
  (:export :filter-alive-items
           :check-uri-alive-p)) ;; Exported for unit testing

(in-package :m3u-check)

(defun check-uri-alive-p (uri timeout &key (stream-check-p t) verbose)
  "Check if a URI carries a live stream.

   For HTTP/HTTPS URIs:
     Opens a raw TCP socket, sends a minimal HTTP GET request, and reads
     the response in one shot. This bypasses HTTP client libraries that
     reject non-2xx status codes. Many multicast proxies return 400 but
     still deliver stream data in the response body.

     The response buffer is searched for the \\r\\n\\r\\n header/body
     boundary. If body data exists beyond the boundary, the stream is
     considered alive.

   For non-HTTP URIs (rtp://, udp://): always returns T (assumed alive).

   VERBOSE: when T, prints result for each URI."
  (labels ((parse-url (url)
             "Extract host, port, and path from an HTTP URL."
             (let* ((no-scheme (if (uiop:string-prefix-p "https://" url)
                                  (subseq url 8)
                                  (subseq url 7)))
                    (slash-pos (position #\/ no-scheme))
                    (host-port (if slash-pos (subseq no-scheme 0 slash-pos) no-scheme))
                    (path (if slash-pos (subseq no-scheme slash-pos) "/"))
                    (colon-pos (position #\: host-port))
                    (host (if colon-pos (subseq host-port 0 colon-pos) host-port))
                    (default-port (if (uiop:string-prefix-p "https://" url) 443 80))
                    (port (if colon-pos
                             (parse-integer (subseq host-port (1+ colon-pos)))
                             default-port)))
               (values host port path)))
           (read-response (stream timeout)
             "Read available response data from STREAM into a buffer.
              Returns (values buffer bytes-read) or (values nil 0) on error."
             (let ((buf (make-array 65536 :element-type '(unsigned-byte 8)))
                   (total 0))
               (handler-case
                   ;; Read in a loop: get whatever is available, up to buffer size
                   (loop
                     (let ((n (read-sequence buf stream :start total)))
                       (when (zerop n) (return))
                       (incf total n)
                       (when (>= total (length buf)) (return))))
                 (end-of-file ()
                   ;; EOF is expected — proxy closed connection
                   )
                 (error ()
                   ;; Other errors
                   (setf total 0)))
               (values buf total)))
           (find-header-end (buf total)
             "Find the position of \\r\\n\\r\\n (double CRLF) marking end of headers.
              Returns the byte position of the first body byte, or NIL if not found."
             (loop for i from 0 to (- total 4)
                   when (and (= (aref buf i) 13)      ; CR
                            (= (aref buf (1+ i)) 10)  ; LF
                            (= (aref buf (+ i 2)) 13) ; CR
                            (= (aref buf (+ i 3)) 10)); LF
                     return (+ i 4))
             nil)
           (extract-status-line (buf total)
             "Extract the HTTP status line from the response buffer."
             (loop for i from 0 below (min total 256)
                   when (and (= (aref buf i) 13) (= (aref buf (1+ i)) 10)) ; CRLF
                     return (handler-case
                                (babel:octets-to-string buf :start 0 :end i :encoding :ascii)
                              (error () "")))
             ""))
    (unless (or (uiop:string-prefix-p "http://" uri)
                (uiop:string-prefix-p "https://" uri))
      (return-from check-uri-alive-p t))

    (multiple-value-bind (host port path) (parse-url uri)
      (handler-case
          (let* ((socket (usocket:socket-connect host port
                                                 :timeout timeout
                                                 :element-type '(unsigned-byte 8)))
                 (stream (usocket:socket-stream socket))
                 (request (format nil
                                  "GET ~a HTTP/1.0\r\nHost: ~a\r\nUser-Agent: m3utool\r\nConnection: close\r\n\r\n"
                                  path host))
                 (alive nil))
            (unwind-protect
                 (progn
                   ;; Send HTTP request
                   (write-sequence (babel:string-to-octets request :encoding :ascii) stream)
                   (force-output stream)
                   ;; Read response in one shot: headers + any available body data
                   (multiple-value-bind (buf total) (read-response stream timeout)
                     (let ((body-start (and (> total 0) (find-header-end buf total)))
                           (status-line (if (> total 0) (extract-status-line buf total) "")))
                       (if stream-check-p
                           ;; Stream check: alive if body data exists after headers
                           (let ((body-size (if body-start (- total body-start) 0)))
                             (setf alive (> body-size 0))
                             (when verbose
                               (format t "[CHECK] ~a -> ~a, ~d bytes headers+body, ~d bytes body -> ~a~%"
                                       uri status-line total body-size
                                       (if alive "ALIVE" "DEAD (no body data)"))))
                           ;; Header-only check
                           (progn
                             (setf alive (and body-start t))
                             (when verbose
                               (format t "[CHECK] ~a -> ~a -> ~a~%"
                                       uri status-line
                                       (if alive "ALIVE (header-only)" "DEAD (no response)")))))))
              (ignore-errors (usocket:socket-close socket)))
            alive)
        (error (e)
          (when verbose
            (format t "[CHECK] ~a -> ERROR: ~a~%" uri e))
          nil))))))

(defun filter-alive-items (items &key (threads 20) (timeout 3)
                                         (stream-check-p t) verbose)
  "Filter playlist-items concurrently using a thread pool, returning only the alive ones.

   STREAM-CHECK-P: when T (default), reads up to 8KB from each HTTP stream
     to confirm data is flowing. Pass an integer to specify bytes to read.
     Pass NIL for header-only check.
   VERBOSE: when T, prints per-URI HTTP status details."
  (format t "[CHECK] Initializing thread pool with ~d workers (timeout: ~ds)~%" threads timeout)
  (format t "[CHECK] Stream check: ~a~%"
          (if stream-check-p
              (format nil "enabled (~d bytes)" (if (integerp stream-check-p) stream-check-p 8192))
              "disabled (header-only)"))
  (setf lparallel:*kernel* (lparallel:make-kernel threads))

  (unwind-protect
       (let* ((total (length items))
              (checked-count 0)
              (alive-count 0)
              (lock (bordeaux-threads:make-lock "progress-lock"))
              (results (lparallel:pmap 'vector
                                       (lambda (item)
                                         (let ((alive (check-uri-alive-p (m3u-data::item-uri item) timeout
                                                       :stream-check-p stream-check-p
                                                       :verbose verbose)))
                                           ;; Thread-safe progress printing
                                           (bordeaux-threads:with-lock-held (lock)
                                             (incf checked-count)
                                             (when alive (incf alive-count))
                                             (when (zerop (mod checked-count (max 1 (min 50 (floor total 10)))))
                                               (format t "[CHECK] ~d/~d (~d alive)~%"
                                                       checked-count total alive-count)))
                                           (if alive item nil)))
                                       items)))
         ;; Filter out NILs and convert vector back to list
         (prog1 (remove nil (coerce results 'list))
           (format t "[CHECK] Done: ~d/~d alive~%" alive-count total)))

    ;; Cleanup: Ensure the thread pool is terminated even if errors occur
    (lparallel:end-kernel :wait t)))
