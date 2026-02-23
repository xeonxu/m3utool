(defpackage :m3u-server
  (:use :cl)
  (:export :start-server))

(in-package :m3u-server)

(defun items-to-m3u-string (items)
  "Convert a list of playlist-item objects to an M3U format string in memory."
  (with-output-to-string (stream)
    (format stream "#EXTM3U~%")
    (dolist (item items)
      (let ((attr-str ""))
        ;; Reconstruct the attributes string
        (maphash (lambda (k v)
                   (when (and k v)
                     (setf attr-str (concatenate 'string attr-str (format nil " ~a=\"~a\"" k v)))))
                 (m3u-data::item-attributes item))
        ;; Write the EXTINF line and the URI
        (format stream "#EXTINF:~d~a,~a~%" 
                (m3u-data::item-duration item) 
                attr-str 
                (m3u-data::item-title item))
        (format stream "~a~%" (m3u-data::item-uri item))))))

(defun start-server (input-xlsx bind-address port server-url strip-proxy)
  "Start the Hunchentoot HTTP server and serve the dynamic playlist."
  (format t "Starting M3U server on ~a:~d...~%" bind-address port)
  (format t "Serving dynamic M3U from: ~a~%" input-xlsx)
  
  ;; Define the HTTP route GET /playlist.m3u
  (hunchentoot:define-easy-handler (playlist :uri "/playlist.m3u") ()
    ;; Set the correct MIME type for M3U playlists
    (setf (hunchentoot:content-type*) "audio/x-mpegurl; charset=utf-8")
    
    (handler-case
        (let ((items (m3u-xlsx:load-xlsx input-xlsx)))
          ;; Apply URI transformations dynamically per request
          (dolist (item items)
            (setf (m3u-data::item-uri item)
                  (m3u-data:transform-uri (m3u-data::item-uri item)
                                          :strip-proxy strip-proxy
                                          :server-url server-url)))
          ;; Generate and return the M3U string
          (items-to-m3u-string items))
      
      ;; Handle file reading or parsing errors gracefully
      (error (e)
        (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
        (format nil "Error generating playlist: ~a" e))))
  
  ;; Initialize and start the server
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor 
                                 :address bind-address 
                                 :port port)))
    (hunchentoot:start acceptor)
    (format t "~%[SUCCESS] Server is running!~%")
    (format t "Add this URL to your player: http://~a:~d/playlist.m3u~%" 
            (if (string= bind-address "0.0.0.0") "127.0.0.1" bind-address) 
            port)
    (format t "Note: The file '~a' is read dynamically. Any changes saved to it will reflect immediately on the next request.~%" input-xlsx)
    (format t "Press Ctrl+C to stop the server.~%")
    
    ;; Keep the main thread alive, otherwise the CLI app will exit immediately
    (handler-case
        (loop (sleep 60))
      ;; Handle Ctrl+C (SIGINT) gracefully in SBCL
      #+sbcl (sb-sys:interactive-interrupt () 
               (format t "~%Shutting down server...~%")
               (hunchentoot:stop acceptor)
               (uiop:quit 0)))))
