(defpackage :m3u-check
  (:use :cl :m3u-data)
  (:export :filter-alive-items
           :check-uri-alive-p)) ;; Exported for unit testing

(in-package :m3u-check)

(defun check-uri-alive-p (uri timeout &key (stream-check-p t) verbose)
  "Check if a URI carries a live stream.

   For HTTP/HTTPS URIs:
     Opens a raw TCP socket, sends a minimal HTTP/1.0 GET request with
     proper CRLF line endings, and reads the response directly as binary.
     This bypasses HTTP client libraries that would reject non-2xx status
     codes — many multicast proxies return 400 but still stream data.

     Uses usocket:wait-for-input with timeout before every read to avoid
     hanging on unresponsive proxies.

   For non-HTTP URIs (rtp://, udp://): always returns T (assumed alive).

   VERBOSE: when T, prints result for each URI."
  (unless (or (uiop:string-prefix-p "http://" uri)
              (uiop:string-prefix-p "https://" uri))
    (return-from check-uri-alive-p t))

  (flet ((parse-url (url)
           "Extract host, port, and path from an HTTP URL."
           (let* ((no-scheme (if (uiop:string-prefix-p "https://" url)
                                (subseq url 8)
                                (subseq url 7)))
                  (slash-pos (position #\/ no-scheme))
                  (host-port (if slash-pos
                                (subseq no-scheme 0 slash-pos)
                                no-scheme))
                  (path (if slash-pos (subseq no-scheme slash-pos) "/"))
                  (colon-pos (position #\: host-port))
                  (host (if colon-pos (subseq host-port 0 colon-pos) host-port))
                  (port (if colon-pos
                           (parse-integer (subseq host-port (1+ colon-pos)))
                           80)))
             (values host port path)))
         (read-crlf-line (socket)
           "Read one CRLF-terminated line from socket as string.
            Uses wait-for-input with timeout. Returns NIL on timeout/EOF."
           (let ((bytes (make-array 64 :element-type '(unsigned-byte 8)
                                       :fill-pointer 0 :adjustable t)))
             (loop
               (unless (usocket:wait-for-input socket :timeout timeout)
                 (return-from read-crlf-line nil))
               (let ((b (handler-case (read-byte socket)
                          (end-of-file () (return-from read-crlf-line nil))
                          (error () (return-from read-crlf-line nil)))))
                 (vector-push-extend b bytes)
                 (when (and (>= (length bytes) 2)
                            (= (aref bytes (- (length bytes) 2)) 13) ; CR
                            (= (aref bytes (- (length bytes) 1)) 10)) ; LF
                   ;; Remove trailing CRLF and return as string
                   (setf (fill-pointer bytes) (- (length bytes) 2))
                   (return-from read-crlf-line
                     (handler-case
                         (babel:octets-to-string bytes :encoding :ascii)
                       (error () "")))))))))
    (multiple-value-bind (host port path) (parse-url uri)
      (handler-case
          (let* ((socket (usocket:socket-connect host port
                                                 :timeout timeout
                                                 :element-type '(unsigned-byte 8)))
                 ;; Build HTTP/1.0 request with proper CRLF (\r\n) endings
                 (request (format nil
                                  "GET ~a HTTP/1.0\r\nHost: ~a\r\nUser-Agent: m3utool\r\nConnection: close\r\n\r\n"
                                  path host))
                 (request-bytes (babel:string-to-octets request :encoding :ascii))
                 (alive nil)
                 (status-line ""))
            (unwind-protect
                 (progn
                   ;; Send HTTP request as binary
                   (write-sequence request-bytes socket)
                   (force-output socket)
                   ;; Read headers line by line (CRLF-terminated)
                   (loop for line = (read-crlf-line socket)
                         while (and line (not (string= line "")))
                         do (when (and (> (length line) 5)
                                      (string= (subseq line 0 5) "HTTP/"))
                              (setf status-line line)))
                   ;; Read body data to confirm stream liveness
                   (if stream-check-p
                       (let* ((buf-size (if (integerp stream-check-p) stream-check-p 8192))
                              (buf (make-array buf-size :element-type '(unsigned-byte 8)))
                              (bytes-read 0))
                         (when (usocket:wait-for-input socket :timeout timeout)
                           (handler-case
                               (setf bytes-read (read-sequence buf socket))
                             (end-of-file () (setf bytes-read 0))
                             (error () (setf bytes-read 0))))
                         (setf alive (> bytes-read 0))
                         (when verbose
                           (format t "[CHECK] ~a -> ~a, read ~d bytes -> ~a~%"
                                   uri status-line bytes-read
                                   (if alive "ALIVE" "DEAD (no data)"))))
                       ;; Header-only: connection + headers received = alive
                       (progn
                         (setf alive t)
                         (when verbose
                           (format t "[CHECK] ~a -> ~a -> ALIVE (header-only)~%"
                                   uri status-line)))))
              (ignore-errors (usocket:socket-close socket)))
            alive)
        ;; Connection/timeout/other errors
        (error (e)
          (when verbose
            (format t "[CHECK] ~a -> ERROR: ~a~%" uri e))
          nil)))))

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
