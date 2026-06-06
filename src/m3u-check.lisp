(defpackage :m3u-check
  (:use :cl :m3u-data)
  (:export :filter-alive-items
           :check-uri-alive-p)) ;; Exported for unit testing

(in-package :m3u-check)

(defun check-uri-alive-p (uri timeout &key (stream-check-p t) verbose)
  "Check if a URI carries a live stream.

   For HTTP/HTTPS URIs:
     Opens a raw TCP socket, sends a minimal HTTP GET request, and reads
     the response directly. This bypasses HTTP client libraries that would
     reject non-2xx status codes — many multicast proxies return 400 or
     other non-2xx codes but still deliver stream data.

     If STREAM-CHECK-P is T (default), reads up to 8KB of response body
     after the HTTP headers to confirm actual data is flowing.

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
                  (path (if slash-pos
                           (subseq no-scheme slash-pos)
                           "/"))
                  (colon-pos (position #\: host-port))
                  (host (if colon-pos
                           (subseq host-port 0 colon-pos)
                           host-port))
                  (port (if colon-pos
                           (parse-integer (subseq host-port (1+ colon-pos)))
                           80)))
             (values host port path))))
    (multiple-value-bind (host port path) (parse-url uri)
      (handler-case
          (let* ((socket (usocket:socket-connect host port
                                                 :timeout timeout
                                                 :element-type 'character))
                 (stream (usocket:socket-stream socket))
                 (request (format nil
                                  "GET ~a HTTP/1.0~%Host: ~a~%~%"
                                  path host))
                 (alive nil)
                 (status-line ""))
            (unwind-protect
                 (progn
                   ;; Send minimal HTTP request
                   (write-string request stream)
                   (force-output stream)
                   ;; Skip headers: read until empty line (CRLF alone)
                   (loop for line = (read-line stream nil nil)
                         while (and line (not (string= line "")))
                         do (when (uiop:string-prefix-p "HTTP/" line)
                              (setf status-line line)))
                   ;; Read body data to confirm stream liveness
                   (if stream-check-p
                       (let* ((buf-size (if (integerp stream-check-p) stream-check-p 8192))
                              (buf (make-array buf-size :element-type '(unsigned-byte 8)))
                              (bytes-read 0))
                         ;; Switch to binary for body reading
                         (let ((binary-stream (flexi-streams:make-flexi-stream
                                               stream :element-type '(unsigned-byte 8))))
                           (handler-case
                               (setf bytes-read (read-sequence buf binary-stream))
                             (end-of-file () (setf bytes-read 0))
                             (error () (setf bytes-read 0))))
                         (setf alive (> bytes-read 0))
                         (when verbose
                           (format t "[CHECK] ~a -> ~a, read ~d bytes -> ~a~%"
                                   uri (string-trim '(#\Return #\Newline) status-line)
                                   bytes-read (if alive "ALIVE" "DEAD (no data)"))))
                       ;; Header-only check: connection succeeded
                       (progn
                         (setf alive t)
                         (when verbose
                           (format t "[CHECK] ~a -> ~a -> ALIVE (header-only)~%"
                                   uri (string-trim '(#\Return #\Newline) status-line))))))
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
