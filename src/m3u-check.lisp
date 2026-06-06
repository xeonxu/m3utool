(defpackage :m3u-check
  (:use :cl :m3u-data)
  (:export :filter-alive-items
           :check-uri-alive-p)) ;; Exported for unit testing

(in-package :m3u-check)

(defun check-uri-alive-p (uri timeout &key (stream-check-p t) verbose)
  "Check if a URI carries a live stream.

   For HTTP/HTTPS URIs:
     Sends a GET request. If STREAM-CHECK-P is T (default), reads up to
     8KB of response body to confirm the proxy is actually sending stream
     data, not just returning an empty HTTP 200 wrapper. This is equivalent
     to 'curl -v' behavior for verifying multicast streams.

     Set STREAM-CHECK-P to NIL for header-only check (fast but proxy may
     return 200 with no actual stream data).

   For non-HTTP URIs (rtp://, udp://): always returns T (assumed alive).

   VERBOSE: when T, prints result for each URI."
  (if (or (uiop:string-prefix-p "http://" uri)
          (uiop:string-prefix-p "https://" uri))
      (handler-case
          (let* ((stream (dex:request uri
                                      :method :get
                                      :want-stream t
                                      :connect-timeout timeout
                                      :read-timeout timeout
                                      :keep-alive nil))
                 (alive nil))
            (unwind-protect
                 (if stream-check-p
                     ;; Read a small buffer to confirm stream data is flowing.
                     ;; A proxy may return HTTP 200 with no body for dead
                     ;; streams; reading actual bytes proves real liveness.
                     (let* ((buf-size (if (integerp stream-check-p)
                                          stream-check-p
                                          8192))
                            (buf (make-array buf-size
                                            :element-type '(unsigned-byte 8)))
                            (bytes-read 0))
                       (handler-case
                           (setf bytes-read (read-sequence buf stream))
                         (end-of-file ()
                           (setf bytes-read 0))
                         (error ()
                           (setf bytes-read 0)))
                       (setf alive (> bytes-read 0))
                       (when verbose
                         (format t "[CHECK] ~a -> read ~d bytes -> ~a~%"
                                 uri bytes-read
                                 (if alive "ALIVE" "DEAD (no data)"))))
                     ;; Header-only: connection success = alive
                     (progn
                       (setf alive t)
                       (when verbose
                         (format t "[CHECK] ~a -> connected -> ALIVE (header-only)~%"
                                 uri)))))
              (when stream (ignore-errors (close stream))))
            alive)
        (error (e)
          (when verbose
            (format t "[CHECK] ~a -> ERROR: ~a~%" uri e))
          nil))
      ;; Non-HTTP protocols: cannot verify via HTTP, assume alive
      t))

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
