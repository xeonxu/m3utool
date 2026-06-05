(eval-when (:load-toplevel :compile-toplevel :execute)
  (ql:quickload '(:cl-ppcre :alexandria :str)))

(defpackage :m3u-scan
  (:use :cl)
  (:export :ip-string-to-integer
           :ip-integer-to-string
           :parse-ip-range
           :generate-ip-list
           :parse-port-range
           :generate-scan-uris
           :scan-network
           :scan-and-save))

(in-package :m3u-scan)

;; ==========================================
;; IP ADDRESS UTILITIES
;; ==========================================

(defun ip-string-to-integer (ip-str)
  "Convert '239.1.1.1' to a 32-bit integer."
  (let ((octets (mapcar #'parse-integer (str:split "." ip-str))))
    (unless (= (length octets) 4)
      (error "Invalid IP address: ~a" ip-str))
    (+ (ash (first octets) 24)
       (ash (second octets) 16)
       (ash (third octets) 8)
       (fourth octets))))

(defun ip-integer-to-string (ip-int)
  "Convert a 32-bit integer back to dotted-decimal string."
  (format nil "~d.~d.~d.~d"
          (ldb (byte 8 24) ip-int)
          (ldb (byte 8 16) ip-int)
          (ldb (byte 8 8) ip-int)
          (ldb (byte 8 0) ip-int)))

;; ==========================================
;; IP RANGE PARSING
;; ==========================================

(defun parse-ip-range (range-str)
  "Parse an IP range specification into (start-ip-int . end-ip-int).
   Supports three formats:
     CIDR:       '239.1.1.0/24'
     Range:      '239.1.1.1-239.1.1.254'
     Single IP:  '239.1.1.1'

   Returns (values start-ip-integer end-ip-integer count)."
  (block nil
    ;; CIDR notation: 239.1.1.0/24
    (cl-ppcre:register-groups-bind (ip-str prefix-str)
        ("^([0-9\\.]+)/([0-9]+)$" range-str)
      (let* ((prefix (parse-integer prefix-str))
             (ip-int (ip-string-to-integer ip-str))
             (mask (1- (ash 1 (- 32 prefix))))
             (network (logand ip-int (lognot mask)))
             (start network)
             (end (logior network mask)))
        (return (values start end (1+ (- end start))))))

    ;; Range notation: 239.1.1.1-239.1.1.254
    (cl-ppcre:register-groups-bind (start-str end-str)
        ("^([0-9\\.]+)-([0-9\\.]+)$" range-str)
      (let ((start (ip-string-to-integer start-str))
            (end (ip-string-to-integer end-str)))
        (when (> start end)
          (error "Start IP (~a) is greater than end IP (~a)" start-str end-str))
        (return (values start end (1+ (- end start))))))

    ;; Single IP: 239.1.1.1
    (cl-ppcre:register-groups-bind (ip-str)
        ("^([0-9\\.]+)$" range-str)
      (let ((ip (ip-string-to-integer ip-str)))
        (return (values ip ip 1))))

    ;; No match
    (error "Invalid IP range format: ~a. Use CIDR (239.1.0.0/16), range (239.1.1.1-239.1.1.254), or single IP." range-str)))

(defun generate-ip-list (range-str &key (max-count 65536))
  "Generate a list of IP address strings from a range specification.
   MAX-COUNT caps the output to prevent accidental huge scans."
  (multiple-value-bind (start end count) (parse-ip-range range-str)
    (when (> count max-count)
      (error "IP range contains ~d addresses, exceeding the safety limit of ~d.~
              ~%If you really want to scan this many, increase :max-count."
             count max-count))
    (loop for ip from start to end
          collect (ip-integer-to-string ip))))

;; ==========================================
;; PORT RANGE PARSING
;; ==========================================

(defun parse-port-range (port-str)
  "Parse a port specification into a list of port numbers.
   Supports:
     Single:   '5000'
     Range:    '5000-5010'
     Comma:    '5000,5002,5008'
     Mixed:    '5000-5002,8000'"
  (let ((ports nil))
    (dolist (segment (str:split "," port-str))
      (setf segment (string-trim " " segment))
      (if (cl-ppcre:register-groups-bind (start-str end-str)
              ("^([0-9]+)-([0-9]+)$" segment)
            (let ((start (parse-integer start-str))
                  (end (parse-integer end-str)))
              (when (> start end)
                (error "Start port (~d) is greater than end port (~d)" start end))
              (loop for p from start to end do (push p ports))))
          (push (parse-integer segment) ports)))
    (nreverse (remove-duplicates ports :from-end t))))

;; ==========================================
;; URI GENERATION
;; ==========================================

(defun generate-scan-uris (ip-spec port-spec &key (proto "rtp") template)
  "Generate a list of URIs by combining IPs and ports.

   ip-spec:    IP range string (CIDR, range, or single)
   port-spec:  Port range string (single, range, comma-separated)
   proto:      Protocol: 'rtp', 'udp', or 'http'
   template:   Custom format string. Use ~a for IP, ~a for port.
               Example: 'http://~a:8080/udp/~a:5000'
               When NIL, uses '{proto}://{ip}:{port}'

   Returns a list of URI strings."
  (let* ((ips (generate-ip-list ip-spec))
         (ports (parse-port-range port-spec))
         (uris nil))
    (dolist (ip ips)
      (dolist (port ports)
        (let ((uri (if template
                       (format nil template ip port)
                       (format nil "~a://~a:~d" proto ip port))))
          (push uri uris))))
    (format t "[INFO] Generated ~d scan URIs (~d IPs x ~d ports).~%"
            (length uris) (length ips) (length ports))
    (nreverse uris)))

;; ==========================================
;; SCAN ENGINE
;; ==========================================

(defun scan-network (uris &key (threads 20) (timeout 3))
  "Scan a list of URIs for alive streams using concurrent HTTP checks.
   Reuses the battle-tested m3u-check:filter-alive-items engine.

   Returns two values:
     alive-items: list of playlist-item objects for alive streams
     dead-uris:   list of dead URI strings"
  (format t "[SCAN] Starting network scan of ~d URIs...~%" (length uris))
  (format t "[SCAN] Workers: ~d | Timeout: ~ds~%" threads timeout)

  ;; Build playlist-item objects for each URI
  (let* ((items (loop for uri in uris
                      collect (let ((item (make-instance 'm3u-data:playlist-item)))
                                (setf (m3u-data:item-uri item) uri
                                      (m3u-data:item-title item)
                                      (format nil "Scanned ~a" uri))
                                item)))
         (total (length items))
         ;; Use the battle-tested concurrent filter
         (alive-items (m3u-check:filter-alive-items items :threads threads :timeout timeout))
         (dead-uris (loop for item in items
                          unless (find (m3u-data:item-uri item) alive-items
                                       :key #'m3u-data:item-uri :test #'string=)
                            collect (m3u-data:item-uri item))))

    (format t "[SCAN] Complete: ~d alive / ~d dead / ~d total~%"
            (length alive-items) (length dead-uris) total)
    (values alive-items dead-uris)))

;; ==========================================
;; SCAN-AND-SAVE CONVENIENCE
;; ==========================================

(defun scan-and-save (ip-spec port-spec output-path
                      &key (proto "rtp") template
                           (threads 20) (timeout 3)
                           database-path sheet-name)
  "Scan a network range and save alive items to a file, optionally merging into a database.

   Parameters:
     ip-spec, port-spec:  IP range and port range strings
     output-path:         Path for output M3U or XLSX file
     proto, template:     URI generation parameters
     threads, timeout:    Scan parameters
     database-path:       If provided, merge results into this XLSX database
     sheet-name:          Target sheet name for merge"
  ;; 1. Generate URIs
  (let ((uris (generate-scan-uris ip-spec port-spec
                                   :proto proto :template template)))
    ;; 2. Scan
    (multiple-value-bind (alive-items dead-uris)
        (scan-network uris :threads threads :timeout timeout)

      (declare (ignore dead-uris))

      ;; 3. Save alive items
      (if (or (string= (pathname-type (pathname output-path)) "xlsx")
              (string= (pathname-type (pathname output-path)) "xls"))
          (m3u-xlsx:write-xlsx-items alive-items output-path :sheet-name sheet-name)
          (m3u-data:save-items-to-m3u alive-items output-path))
      (format t "[SCAN] Saved ~d alive items to ~a~%" (length alive-items) output-path)

      ;; 4. Optionally merge into database
      (when database-path
        (format t "[SCAN] Merging into database: ~a~%" database-path)
        (let ((existing-items (if (probe-file database-path)
                                  (m3u-xlsx:read-xlsx-items database-path :sheet-name sheet-name)
                                  nil)))
          (let ((merged (if existing-items
                            (m3u-data:merge-items existing-items alive-items)
                            alive-items)))
            (m3u-xlsx:write-xlsx-items merged database-path :sheet-name sheet-name)
            (format t "[SCAN] Database updated with ~d new items.~%"
                    (- (length merged) (length existing-items))))))
      alive-items)))
