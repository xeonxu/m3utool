(defpackage :m3u-check
  (:use :cl :m3u-data)
  (:export :filter-alive-items
           :check-uri-alive-p)) ;; Exported for unit testing

(in-package :m3u-check)

(defun check-uri-alive-p (uri timeout)
  "Check if a URI is alive. 
   Sends a GET request and closes the stream immediately upon receiving headers 
   to avoid downloading large media files."
  (if (or (uiop:string-prefix-p "http://" uri)
          (uiop:string-prefix-p "https://" uri))
      (handler-case
          (let ((stream (dex:request uri
                                     :method :get
                                     :want-stream t
                                     :connect-timeout timeout
                                     :read-timeout timeout
                                     :keep-alive nil)))
            (when stream
              (close stream))
            t) ;; Return T if HTTP 2xx/3xx is received
        ;; Return NIL on any network error or HTTP 4xx/5xx
        (error () nil)) 
      ;; Default to T for non-HTTP protocols (e.g., rtp://, udp://) 
      ;; as they cannot be verified via simple HTTP GET requests.
      t))

(defun filter-alive-items (items &key (threads 20) (timeout 3))
  "Filter playlist-items concurrently using a thread pool, returning only the alive ones."
  (format t "[CHECK] Initializing thread pool with ~d workers (timeout: ~ds)...~%" threads timeout)
  (setf lparallel:*kernel* (lparallel:make-kernel threads))

  (unwind-protect
       (let* ((total (length items))
              (checked-count 0)
              (alive-count 0)
              (lock (bordeaux-threads:make-lock "progress-lock"))
              (results (lparallel:pmap 'vector
                                       (lambda (item)
                                         (let ((alive (check-uri-alive-p (m3u-data::item-uri item) timeout)))
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
