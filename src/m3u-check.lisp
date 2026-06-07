(defpackage :m3u-check
  (:use :cl :m3u-data)
  (:export :filter-alive-items
           :check-uri-alive-p)) ;; Exported for unit testing

(in-package :m3u-check)

(defun check-uri-alive-p (uri timeout &key (stream-check-p t) verbose)
  "Check if a URI carries a live stream.

   For HTTP/HTTPS URIs:
     Opens a raw TCP socket, sends an HTTP GET request, then reads
     response data with a deadline-based timeout. This bypasses HTTP
     client libraries that reject non-2xx status codes — many multicast
     proxies return 400 but still deliver stream data.

     On SBCL, sb-sys:with-deadline is used to enforce the read timeout.

   For non-HTTP URIs (rtp://, udp://): always returns T (assumed alive).

   VERBOSE: when T, prints result for each URI."
  (flet ((parse-url (url)
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
             (values host port path))))
    (unless (or (uiop:string-prefix-p "http://" uri)
                (uiop:string-prefix-p "https://" uri))
      (return-from check-uri-alive-p t))

    (multiple-value-bind (host port path) (parse-url uri)
      (handler-case
          (let* ((socket (usocket:socket-connect host port
                                                 :timeout timeout
                                                 :element-type '(unsigned-byte 8)))
                 (stream (usocket:socket-stream socket))
                 (crlf (format nil "~c~c" (code-char 13) (code-char 10)))
                 (request (concatenate 'string
                           "GET " path " HTTP/1.1" crlf
                           "Host: " host crlf
                           "User-Agent: m3utool" crlf
                           "Accept: */*" crlf
                           "Connection: close" crlf
                           crlf))
                 (alive nil)
                 (buf (make-array 65536 :element-type '(unsigned-byte 8)))
                 (total 0))
            (unwind-protect
                 (progn
                   ;; Send HTTP request
                   (write-sequence (babel:string-to-octets request :encoding :ascii) stream)
                   (force-output stream)
                   ;; Read response with deadline-based timeout (SBCL)
                   (handler-case
                       #+sbcl
                       (sb-sys:with-deadline (:seconds timeout)
                         (loop
                           (let ((n (read-sequence buf stream :start total)))
                             (when (zerop n) (return))
                             (incf total n)
                             (when (>= total (length buf)) (return)))))
                       #-sbcl
                       ;; Non-SBCL: poll with wait-for-input as fallback
                       (progn
                         (when (usocket:wait-for-input socket :timeout timeout)
                           (loop
                             (let ((n (read-sequence buf stream :start total)))
                               (when (zerop n) (return))
                               (incf total n)
                               (when (>= total (length buf)) (return))))))
                     (sb-sys:deadline-timeout ()
                       ;; Timeout: keep what we've read so far
                       nil)
                     (end-of-file ()
                       ;; EOF: expected after proxy finishes
                       nil)
                     (t ()
                       ;; Other errors: keep partial data
                       nil))
                   ;; Parse response: find header/body boundary
                   (let ((body-start (and (> total 0)
                                         (loop for i from 0 to (- total 4)
                                               when (and (= (aref buf i) 13)
                                                        (= (aref buf (1+ i)) 10)
                                                        (= (aref buf (+ i 2)) 13)
                                                        (= (aref buf (+ i 3)) 10))
                                               return (+ i 4)))))
                     ;; Extract status line from headers
                     (let ((status-line
                             (if (> total 0)
                                 (loop for i from 0 below (min total 256)
                                       when (and (= (aref buf i) 13)
                                                (= (aref buf (1+ i)) 10))
                                       return (handler-case
                                                  (babel:octets-to-string buf :start 0 :end i
                                                                          :encoding :ascii)
                                                (t () "")))
                                 "")))
                       ;; Determine liveness
                       (if stream-check-p
                           (let ((body-size (if body-start (- total body-start) 0)))
                             (setf alive (> body-size 0))
                             (when verbose
                               (format t "[CHECK] ~a -> ~a, ~d bytes total, ~d bytes body -> ~a~%"
                                       uri status-line total body-size
                                       (if alive "ALIVE" "DEAD (no body data)"))))
                           (progn
                             (setf alive (and body-start t))
                             (when verbose
                               (format t "[CHECK] ~a -> ~a -> ~a~%"
                                       uri status-line
                                       (if alive "ALIVE (header-only)" "DEAD (no response)"))))))))
              (ignore-errors (usocket:socket-close socket)))
            alive)
        ;; Connection refused / DNS failure / etc
        (t (err)
          (when verbose
            (format t "[CHECK] ~a -> ERROR: ~a~%" uri err))
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
                                           (bordeaux-threads:with-lock-held (lock)
                                             (incf checked-count)
                                             (when alive (incf alive-count))
                                             (when (zerop (mod checked-count (max 1 (min 50 (floor total 10)))))
                                               (format t "[CHECK] ~d/~d (~d alive)~%"
                                                       checked-count total alive-count)))
                                           (if alive item nil)))
                                       items)))
         (prog1 (remove nil (coerce results 'list))
           (format t "[CHECK] Done: ~d/~d alive~%" alive-count total)))

    (lparallel:end-kernel :wait t)))
