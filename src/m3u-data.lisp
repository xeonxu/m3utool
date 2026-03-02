(eval-when (:load-toplevel :compile-toplevel :execute)
  (ql:quickload '(:cl-ppcre :alexandria)))

(defpackage :m3u-data
  (:use :cl)
  (:export :transform-uri
           :playlist-item
           :item-duration
           :item-title
           :item-uri
           :item-attributes
           :parse-m3u-file
           :items-to-m3u
           :save-items-to-m3u
           :flexible-csv-row-to-item
           :merge-items))

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
  "Core URI processing function:
   1. Extracts IP:PORT and protocol from HTTP proxies or raw RTP/UDP URIs.
   2. Reconstructs the URI depending on whether a new server-url is provided."
  (let ((result original-uri)
        extracted-ip
        extracted-port
        extracted-proto)

    ;; 1. Regex parsing: match both HTTP proxy format and raw RTP/UDP format simultaneously
    ;; Group 1: Match protocol in HTTP proxy (udp|rtp)
    ;; Group 2: Match protocol in raw multicast (udp|rtp)
    ;; Group 3: IP address
    ;; Group 4: Port number
    (cl-ppcre:register-groups-bind (http-proto raw-proto ip port)
        ("^(?:https?:\\/\\/.*\\/(udp|rtp)\\/|(udp|rtp):\\/\\/)(\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}):(\\d+)$" result)
      (setf extracted-proto (or http-proto raw-proto "rtp")) ;; Fallback to rtp if extraction fails
      (setf extracted-ip ip)
      (setf extracted-port port))

    ;; 2. Determine transformation logic based on parameters
    (cond
      ;; Case A: Convert to unicast address (user provided server-url)
      ;; Whether it's rtp://239... or http://old-proxy..., if extracted successfully,
      ;; strip it and append it to the new server-url.
      ((and server-url (> (length server-url) 0))
       (if extracted-ip
           (setf result (format nil "~a~a:~a" server-url extracted-ip extracted-port))
           ;; If regex doesn't match (e.g., standard http://domain.com/1.m3u8), append directly
           (setf result (concatenate 'string server-url original-uri))))

      ;; Case B: Restore to multicast address (no server-url provided, but strip-proxy requested)
      ;; Example: http://192.168.1.1:8080/rtp/239.1.1.1:5000 -> rtp://239.1.1.1:5000
      (strip-proxy
       (if extracted-ip
           (setf result (format nil "~a://~a:~a" extracted-proto extracted-ip extracted-port))
           ;; If not matched, keep the original URI
           result))

      ;; Case C: Do nothing
      (t result))))

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

(defun flexible-csv-row-to-item (row headers)
  "Convert a row from CSV/XLSX to a playlist-item object.
   row: list of cell values
   headers: list of column names (Duration, Title, URI, ...)

   Note: Duration defaults to 0 if parsing fails (e.g., empty or non-numeric values)."
  (let ((item (make-instance 'playlist-item)))
    ;; Map headers to row values
    (loop for header in headers
          for value in row
          for i from 0
          do
            (let ((clean-value (if value (format nil "~a" value) ""))
                  (clean-header (string-trim '(#\Space)
                                             (if header (format nil "~a" header) ""))))
              (cond
                ;; Duration column
                ((string-equal clean-header "Duration")
                 (setf (item-duration item) (or (parse-integer clean-value :junk-allowed t) 0)))
                ;; Title column
                ((string-equal clean-header "Title")
                 (setf (item-title item) clean-value))
                ;; URI column
                ((string-equal clean-header "URI")
                 (setf (item-uri item) clean-value))
                ;; Everything else is an attribute
                ((> (length clean-header) 0)
                 (setf (gethash (string-downcase clean-header) (item-attributes item))
                       clean-value)))))
    item))

(defun merge-items (existing-items new-items)
  "Merge NEW-ITEMS into EXISTING-ITEMS using 'Title' as the primary key.
   If a channel exists but has a different URI, append the new URI into
   the attributes (e.g., as 'uri-2', 'uri-3').
   If the channel and URI are completely identical, ignore the duplicate."
  (let ((item-map (make-hash-table :test 'equal))
        (result '()))

    ;; 1. Index existing items and preserve their original order
    (dolist (item existing-items)
      (setf (gethash (item-title item) item-map) item)
      (push item result))
    (setf result (nreverse result))

    ;; 2. Process new items for merging
    (dolist (new-item new-items)
      (let ((existing (gethash (item-title new-item) item-map)))
        (if existing
            ;; Case A: Channel exists, check if URI is a new alternative source
            (let ((new-uri (item-uri new-item))
                  (is-duplicate nil))

              ;; Check if the primary URI is identical
              (when (string= (item-uri existing) new-uri)
                (setf is-duplicate t))

              ;; Check if the URI already exists in the backup slots (uri-2 to uri-20)
              (unless is-duplicate
                (loop for i from 2 to 20
                      for key = (format nil "uri-~d" i)
                      for val = (gethash key (item-attributes existing))
                      do (when (and val (string= val new-uri))
                           (setf is-duplicate t)
                           (return))))

              ;; If it's not a duplicate, find an empty slot and insert it
              (unless is-duplicate
                (loop for i from 2 to 20
                      for key = (format nil "uri-~d" i)
                      for val = (gethash key (item-attributes existing))
                      do (cond ((null val) ; Found an empty slot
                                (setf (gethash key (item-attributes existing)) new-uri)
                                (return))))))

            ;; Case B: Completely new channel, append to the end and update the index
            (progn
              (setf (gethash (item-title new-item) item-map) new-item)
              (setf result (append result (list new-item)))))))

    result))
