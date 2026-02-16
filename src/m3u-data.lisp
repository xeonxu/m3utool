(eval-when (:load-toplevel :compile-toplevel :execute)
  (ql:quickload '(:cl-ppcre :alexandria)))

(defpackage :m3u-data
  (:use :cl)
  (:export :transform-uri))

(in-package :m3u-data)

;; Define playlist item
(defclass playlist-item ()
  ((duration
    :initarg :duration
    :initform 0
    :accessor item-duration)
   (title
    :initarg :title
    :initform ""
    :accessor item-title)
   (uri
    :initarg :uri
    :initform ""
    :accessor item-uri)
   (attributes
    :initarg :attributes
    :initform (make-hash-table :test 'equal)
    :accessor item-attributes
    :documentation "Stores all key-value pairs like tvg-id, group-title, etc.")))

(defun parse-extinf-line (line item)
  "Ultimate enhanced version: supports unquoted attributes, mixed separators, and unified lowercase Keys"
  
  ;; 1. Extract duration (remains unchanged)
  (cl-ppcre:register-groups-bind (dur)
      ("^#EXTINF:([-0-9\\.]+)" line)
    (setf (item-duration item) (parse-integer dur :junk-allowed t)))

  ;; 2. Extract title (remains unchanged, takes content after the last comma)
  (let ((last-comma-pos (position #\, line :from-end t)))
    (when last-comma-pos
      (setf (item-title item) 
            (string-trim '(#\Space #\Tab #\Return #\Newline) 
                         (subseq line (1+ last-comma-pos))))))

  ;; 3. Extract all attributes (core modification)
  ;; Regex explanation:
  ;;    ([a-zA-Z0-9\-_]+)    -> Group 1: Match Key (supports letters, numbers, hyphens, underscores)
  ;;    =                    -> Equals sign
  ;;    (?:                  -> Non-capturing group start
  ;;      "([^"]*)"          -> Group 2: Match content inside double quotes (value)
  ;;      |                  -> Or
  ;;      ([^, ]+)           -> Group 3: Match unquoted content (until space or comma is encountered)
  ;;    )
  (cl-ppcre:do-register-groups (key val-quoted val-unquoted)
      ("([a-zA-Z0-9\\-_]+)=(?:\"([^\"]*)\"|([^, ]+))" line)
    
    (let ((final-key (string-downcase key)) ;; Force Key to lowercase
          (final-val (or val-quoted val-unquoted))) ;; Take the matched value
      
      (when final-val
        (setf (gethash final-key (item-attributes item)) final-val)))))

(defun parse-m3u-file (filepath)
  "Read M3U file and return a list of playlist-item objects"
  (let ((items '())
        (current-item nil))
    (with-open-file (stream filepath :direction :input)
      (loop for line = (read-line stream nil nil)
            while line do
              (let ((clean-line (string-trim '(#\Space #\Return #\Newline) line)))
                (cond
                  ;; Case A: It is a metadata line
                  ((uiop:string-prefix-p "#EXTINF:" clean-line)
                   (setf current-item (make-instance 'playlist-item))
                   (parse-extinf-line clean-line current-item))
                  
                  ;; Case B: It is a URL line (and not other comments)
                  ((and current-item 
                        (> (length clean-line) 0) 
                        (not (uiop:string-prefix-p "#" clean-line)))
                   (setf (item-uri current-item) clean-line)
                   (push current-item items)
                   (setf current-item nil)))))) ;; Reset, prepare for the next item
    (nreverse items)))

(defun transform-uri (original-uri &key strip-proxy server-url)
  "Core URI processing function: strips proxy first, then appends new server."
  (let ((result original-uri))
    
    ;; 1. Handle stripping logic
    (when strip-proxy
      ;; Regex logic: Find the IP:Port pattern at the end of the string
      ;; Group 1: IP address, Group 2: Port number
      ;; Example: http://.../udp/233.18.1.1:5000 -> 233.18.1.1:5000
      (cl-ppcre:register-groups-bind (ip port)
          ("https?:\\/\\/.*\\/(?:udp\\/|rtp\\/)(\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}):(\\d+)$" result)
        (setf result (format nil "~a:~a" ip port))))

    ;; 2. Handle server prefix appending
    (when (and server-url (> (length server-url) 0))
      (setf result (concatenate 'string server-url result)))
    
    result))

(defun collect-all-attribute-keys (items)
  "Scan all items, collect all attribute names that have appeared"
  (let ((keys (make-hash-table :test 'equal)))
    (dolist (item items)
      (maphash (lambda (k v) (declare (ignore v)) (setf (gethash k keys) t))
               (item-attributes item)))
    (alexandria:hash-table-keys keys))) ;; Requires loading the alexandria library

(defun items-to-m3u (items output-path)
  "Write object list back to M3U file"
  (with-open-file (stream output-path :direction :output :if-exists :supersede)
    (format stream "#EXTM3U~%") ;; Write file header
    (dolist (item items)
      ;; Build attribute string: key="val" key2="val2"
      (let ((attr-str ""))
        (maphash (lambda (k v)
                   (setf attr-str (concatenate 'string attr-str (format nil " ~a=\"~a\"" k v))))
                 (item-attributes item))
        
        ;; Write #EXTINF line
        (format stream "#EXTINF:~d~a,~a~%" 
                (item-duration item) 
                attr-str 
                (item-title item))
        ;; Write URI line
        (format stream "~a~%" (item-uri item))))))

(defun flexible-csv-row-to-item (row headers)
  "Convert a CSV row to a playlist-item object based on headers.
   Expected format: Duration, Title, URI, [additional attributes...]"
  (let ((item (make-instance 'playlist-item)))
    ;; Process each cell in the row according to the headers
    (loop for cell in row
          for header in headers
          for header-lower = (string-downcase (string-trim '(#\Space #\Tab) header))
          do (cond
               ;; Duration field
               ((string= header-lower "duration")
                (setf (item-duration item) 
                      (or (parse-integer (format nil "~a" cell) :junk-allowed t) -1)))
               ;; Title field
               ((string= header-lower "title")
                (setf (item-title item) (format nil "~a" cell)))
               ;; URI field
               ((string= header-lower "uri")
                (setf (item-uri item) (format nil "~a" cell)))
               ;; All other fields become attributes
               (t
                (let ((cell-str (format nil "~a" cell)))
                  (when (and cell-str (> (length cell-str) 0))
                    (setf (gethash header-lower (item-attributes item)) cell-str))))))
    item))

(defun save-items-to-m3u (items output-path)
  "Write the object list to the final M3U file"
  (with-open-file (stream output-path 
                          :direction :output 
                          :if-exists :supersede
                          :external-format :utf-8) ;; Explicitly specify UTF-8
    (format stream "#EXTM3U~%")
    (dolist (item items)
      ;; Build attribute string: key="value"
      (let ((attr-str ""))
        (maphash (lambda (k v)
                   ;; Only write when both key and value have content
                   (when (and k v)
                     (setf attr-str 
                           (concatenate 'string attr-str 
                                        (format nil " ~a=\"~a\"" k v)))))
                 (item-attributes item))
        
        ;; Write #EXTINF line
        ;; Format: #EXTINF:duration [attributes...],title
        (format stream "#EXTINF:~d~a,~a~%" 
                (item-duration item) 
                attr-str 
                (item-title item))
        ;; Write URI
        (format stream "~a~%" (item-uri item))))))
