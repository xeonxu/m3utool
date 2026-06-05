(eval-when (:load-toplevel :compile-toplevel :execute)
  (ql:quickload '(:clingon :slynk :swank))
  ;; Pre-load all common SLY contribs into the memory image so it won't
  ;;    attempt to find the source files dynamically on the deployment server.
  (let ((slynk-modules '(:slynk/mrepl
                         :slynk/arglists
                         :slynk/stickers
                         :slynk/trace-dialog
                         :slynk/profiler
                         :slynk/indentation
                         :slynk/package-fu
                         :slynk/fancy-inspector)))
    (dolist (module slynk-modules)
      (format t "[BUILD] Pre-loading Slynk contrib: ~A~%" module)
      (ql:quickload module))))

(defpackage :m3u-cli
  (:use :cl)
  (:import-from :clingon)
  (:export :main))

(in-package :m3u-cli)

(defun default-output-path (input-path new-ext)
  "E.g., input.m3u -> input.xlsx"
  (let ((p (pathname input-path)))
    (make-pathname :type new-ext :defaults p)))

;; ==========================================
;; UNIVERSAL I/O HELPERS
;; ==========================================
(defun m3u-file-p (path)
  "Check if the file path has a valid M3U extension."
  (let ((ext (string-downcase (pathname-type (pathname path)))))
    (or (string= ext "m3u") (string= ext "m3u8"))))

(defun excel-file-p (path)
  "Check if the file path has a valid Excel extension."
  (let ((ext (string-downcase (pathname-type (pathname path)))))
    (or (string= ext "xlsx") (string= ext "xls"))))

(defun load-items (path &key sheet-name)
  "Universally load items based on file extension."
  (cond ((m3u-file-p path)
         (m3u-data:parse-m3u-file path))
        ((excel-file-p path)
         (m3u-xlsx:read-xlsx-items path :sheet-name sheet-name))
        (t (error "Unsupported input format. Must be .m3u, .m3u8, .xls, or .xlsx: ~a" path))))

(defun save-items (items path &key sheet-name)
  "Universally save items based on file extension."
  (cond ((m3u-file-p path)
         (m3u-data:save-items-to-m3u items path))
        ((excel-file-p path)
         (m3u-xlsx:write-xlsx-items items path :sheet-name sheet-name))
        (t (error "Unsupported output format. Must be .m3u, .m3u8, .xls, or .xlsx: ~a" path))))

;; --- Define convert options ---
(defun convert/options ()
  (list
   (clingon:make-option :string :description "Input file path (.m3u/.xlsx)" :short-name #\i :long-name "input" :key :input :required t)
   (clingon:make-option :string :description "Output file path (.m3u/.xlsx)" :short-name #\o :long-name "output" :key :output :required t)
   (clingon:make-option :boolean/true :description "Strip existing proxy prefix from URIs" :short-name #\s :long-name "strip-proxy" :key :strip-proxy)
   (clingon:make-option :string :description "Change proxy address to the new server address (e.g., http://192.168.1.1:8080/udp/). It also implemented the strip function." :short-name #\S :long-name "server-url" :key :server-url)
   (clingon:make-option :string :description "Sheet name to read from input XLSX (default: first sheet)" :long-name "sheet" :key :sheet)
   (clingon:make-option :string :description "Sheet name to write to output XLSX (default: Channel List)" :long-name "output-sheet" :key :output-sheet)
   (clingon:make-option :boolean/true :description "List all sheet names in the input XLSX and exit" :long-name "list-sheets" :key :list-sheets)))

;; --- Define convert handler ---
(defun convert/handler (cmd)
  (let ((input-path (clingon:getopt cmd :input))
        (output-path (clingon:getopt cmd :output))
        (strip-proxy (clingon:getopt cmd :strip-proxy))
        (server-url (clingon:getopt cmd :server-url))
        (sheet-name (clingon:getopt cmd :sheet))
        (output-sheet (clingon:getopt cmd :output-sheet))
        (list-sheets (clingon:getopt cmd :list-sheets)))

    ;; --list-sheets flag: print sheet names and exit
    (when list-sheets
      (if (excel-file-p input-path)
          (progn
            (format t "Sheets in ~a:~%" input-path)
            (dolist (name (m3u-xlsx:list-sheet-names input-path))
              (format t "  - ~a~%" name)))
          (format t "Error: --list-sheets only works with XLSX input files.~%"))
      (return-from convert/handler))

    (format t "Converting: ~a -> ~a~%" input-path output-path)

    (let ((items (load-items input-path :sheet-name sheet-name)))
      ;; Apply URI transformations if proxy options are provided
      (when (or strip-proxy server-url)
        (format t "[INFO] Applying URI transformations (strip-proxy: ~a, server-url: ~a)~%" strip-proxy server-url)
        (dolist (item items)
          (setf (m3u-data:item-uri item)
                (m3u-data:transform-uri (m3u-data:item-uri item)
                                        :strip-proxy strip-proxy
                                        :server-url server-url))))

      (save-items items output-path :sheet-name output-sheet))))

;; --- Define convert command ---
(defun convert/command ()
  (clingon:make-command
   :name "convert"
   :description "Convert formats and modify URI proxies"
   :usage "-i <input> -o <output> [options]"
   :examples '(("Convert M3U to XLSX" . "m3utool convert -i in.m3u -o out.xlsx")
               ("Add proxy to M3U" . "m3utool convert -i in.m3u -o out.m3u -S http://192.168.1.1:8080/rtp/")
               ("Strip proxy from DB" . "m3utool convert -i db.xlsx -o clean.m3u -s"))
   :options (convert/options)
   :handler #'convert/handler))

;; --- Define update option ---
(defun update/options ()
  (list
   (clingon:make-option :string :description "Input file with new channels (.m3u or .xlsx)" :short-name #\i :long-name "input" :key :input :required t)
   (clingon:make-option :string :description "Target Core Database (.xlsx)" :short-name #\d :long-name "database" :key :db :required t)
   (clingon:make-option :string :description "Target Sheet Name (Optional)" :short-name #\s :long-name "sheet" :key :sheet)
   (clingon:make-option :string :description "Sheet name to read from input file (default: first sheet)" :long-name "input-sheet" :key :input-sheet)))

;; --- Define update handler ---
(defun update/handler (cmd)
  (let ((input-path (clingon:getopt cmd :input))
        (db-path (clingon:getopt cmd :db))
        (sheet-name (clingon:getopt cmd :sheet))
        (input-sheet (clingon:getopt cmd :input-sheet)))

    (format t "Updating database ~a with items from ~a...~%" db-path input-path)

    (let* ((new-items (load-items input-path :sheet-name input-sheet))
           ;; If the database exists, load it; otherwise, start with an empty list
           (existing-items (if (probe-file db-path)
                               (load-items db-path :sheet-name sheet-name)
                               nil))
           ;; Perform the core merge algorithm
           (merged-items (if existing-items
                             (m3u-data:merge-items existing-items new-items)
                             new-items)))

      ;; Force saving as XLSX since it's an update operation on the core DB
      (m3u-xlsx:write-xlsx-items merged-items db-path :sheet-name sheet-name))))

;; --- Define update command ---
(defun update/command ()
  (clingon:make-command
   :name "update"
   :description "Merge new channels into the core Excel database"
   :usage "-i <new_channels> -d <core_database.xlsx> [options]"
   :examples '(("Merge new scan into DB" . "m3utool update -i scan.m3u -d core.xlsx"))
   :options (update/options)
   :handler #'update/handler))

;; --- Define check option ---
(defun check/options ()
  (list
   (clingon:make-option :string
                        :description "Input M3U file"
                        :short-name #\i :long-name "input"
                        :key :input)
   (clingon:make-option :string
                        :description "Comma-separated list of URLs to check"
                        :short-name #\u :long-name "url"
                        :key :url)
   (clingon:make-option :string
                        :description "Output M3U file for alive items (optional if using --url)"
                        :short-name #\o :long-name "output"
                        :key :output)
   (clingon:make-option :integer
                        :description "Connection timeout in seconds"
                        :short-name #\t :long-name "timeout"
                        :key :timeout :initial-value 3)
   (clingon:make-option :integer
                        :description "Number of concurrent threads"
                        :short-name #\w :long-name "workers"
                        :key :workers :initial-value 20)))

;; --- Define check handler ---
(defun check/handler (cmd)
  (let ((input (clingon:getopt cmd :input))
        (url-str (clingon:getopt cmd :url))
        (output (clingon:getopt cmd :output))
        (timeout (clingon:getopt cmd :timeout))
        (workers (clingon:getopt cmd :workers)))

    ;; 1. Mutual exclusion validation
    (when (and input url-str)
      (format t "Error: --input and --url are mutually exclusive. Please provide only one.~%")
      (clingon:exit 1))
    (unless (or input url-str)
      (format t "Error: You must provide either an input file (-i) or a URL list (-u).~%")
      (clingon:exit 1))

    ;; 2. Parse or construct items
    (let ((items nil))
      (if input
          (progn
            (format t "Parsing M3U file: ~a~%" input)
            (setf items (m3u-data::parse-m3u-file input)))
          (progn
            (format t "Using manually provided URLs...~%")
            (let ((urls (str:split "," url-str :omit-nulls t)))
              (setf items (loop for u in urls
                                collect (let ((item (make-instance 'm3u-data::playlist-item)))
                                          (setf (m3u-data::item-uri item) (string-trim " " u))
                                          (setf (m3u-data::item-title item) "Manual URL")
                                          item))))))

      ;; 3. Execute concurrent check
      (let* ((total (length items))
             (alive-items (m3u-check:filter-alive-items items :threads workers :timeout timeout)))
        (format t "~%--- Check Complete ---~%")
        (format t "Total: ~d | Alive: ~d | Dead: ~d~%" total (length alive-items) (- total (length alive-items)))

        ;; 4. Output results based on context
        (if output
            (progn
              (m3u-data::items-to-m3u alive-items output)
              (format t "Saved alive items to ~a.~%" output))
            (progn
              (format t "~%[Alive URLs]~%")
              (dolist (item alive-items)
                (format t "~a~%" (m3u-data::item-uri item)))))))))

;; --- Define check command ---
(defun check/command ()
  (clingon:make-command
   :name "check"
   :description "Scan URIs and remove dead links"
   :options (check/options)
   :handler #'check/handler))

;; Define Server Options
(defun server/options ()
  (list
   (clingon:make-option :string
                        :description "Input XLSX file to serve"
                        :short-name #\i :long-name "input"
                        :key :input)
   (clingon:make-option :string
                        :description "Bind address (default: 0.0.0.0 for all interfaces)"
                        :short-name #\b :long-name "bind"
                        :key :bind :initial-value "0.0.0.0")
   (clingon:make-option :integer
                        :description "Specify the HTTP server port (e.g., 8080)"
                        :short-name #\p :long-name "port"
                        :long-name "port"
                        :key :port)
   (clingon:make-option :integer
                        :description "Specify the HTTPS server port (e.g., 8443)"
                        :short-name #\P :long-name "https-port"
                        :key :https-port)
   (clingon:make-option :string
                        :description "Specify the new server address"
                        :short-name #\S :long-name "server"
                        :key :server)
   (clingon:make-option :boolean/true
                        :description "Strip the original proxy address"
                        :short-name #\s
                        :long-name "strip-proxy"
                        :key :strip-proxy)
   (clingon:make-option :string
                        :description "Serve only the specified sheet from the XLSX database"
                        :long-name "sheet"
                        :key :sheet)))

;; Define Server Handler
(defun server/handler (cmd)
  (let ((input (clingon:getopt cmd :input))
        (bind (clingon:getopt cmd :bind))
        (port (clingon:getopt cmd :port))
        (https-port (clingon:getopt cmd :https-port))
        (server (clingon:getopt cmd :server))
        (strip (clingon:getopt cmd :strip-proxy))
        (sheet-name (clingon:getopt cmd :sheet)))

    (unless input
      (format t "Error: You must provide an input XLSX file using -i or --input.~%")
      (clingon:exit 1))

    (when server
      (setf strip 't))

    ;; Delegate to the m3u-server module
    (m3u-server:start-server input bind port https-port server strip :sheet-name sheet-name)))

;; Build the Command Object
(defun server/command ()
  (clingon:make-command
   :name "server"
   :description "Start an HTTP server to serve a dynamic M3U playlist from an XLSX file"
   :options (server/options)
   :handler #'server/handler))

;; --- Define scan options ---
(defun scan/options ()
  (list
   (clingon:make-option :string
                        :description "CIDR range to scan (e.g., 239.1.1.0/24)"
                        :short-name #\r :long-name "range"
                        :key :range)
   (clingon:make-option :string
                        :description "Start IP of range"
                        :long-name "start-ip"
                        :key :start-ip)
   (clingon:make-option :string
                        :description "End IP of range"
                        :long-name "end-ip"
                        :key :end-ip)
   (clingon:make-option :string
                        :description "Port or port range (e.g., 5000 or 5000-5010)"
                        :short-name #\p :long-name "port"
                        :key :port :initial-value "5000")
   (clingon:make-option :string
                        :description "Protocol: rtp, udp, or http (used when --proxy not set)"
                        :long-name "proto"
                        :key :proto :initial-value "rtp")
   (clingon:make-option :string
                        :description "Proxy server URL for multicast-to-unicast conversion (e.g., http://192.168.0.5:7088/udp/). The IP:port is appended to this URL."
                        :short-name #\P :long-name "proxy"
                        :key :proxy)
   (clingon:make-option :string
                        :description "Custom URI template (use ~a for IP, ~a for port). Takes precedence over --proxy."
                        :long-name "template"
                        :key :template)
   (clingon:make-option :string
                        :description "Output file path (.m3u or .xlsx)"
                        :short-name #\o :long-name "output"
                        :key :output :required t)
   (clingon:make-option :string
                        :description "Merge results into existing XLSX database"
                        :short-name #\d :long-name "database"
                        :key :db)
   (clingon:make-option :string
                        :description "Target sheet name for merge"
                        :short-name #\s :long-name "sheet"
                        :key :sheet)
   (clingon:make-option :integer
                        :description "Connection timeout in seconds (default: 5)"
                        :short-name #\t :long-name "timeout"
                        :key :timeout :initial-value 5)
   (clingon:make-option :integer
                        :description "Number of concurrent workers (default: 2)"
                        :short-name #\w :long-name "workers"
                        :key :workers :initial-value 2)))

;; --- Define scan handler ---
(defun scan/handler (cmd)
  (let ((range (clingon:getopt cmd :range))
        (start-ip (clingon:getopt cmd :start-ip))
        (end-ip (clingon:getopt cmd :end-ip))
        (port (clingon:getopt cmd :port))
        (proto (clingon:getopt cmd :proto))
        (proxy-url (clingon:getopt cmd :proxy))
        (template (clingon:getopt cmd :template))
        (output (clingon:getopt cmd :output))
        (db (clingon:getopt cmd :db))
        (sheet (clingon:getopt cmd :sheet))
        (timeout (clingon:getopt cmd :timeout))
        (workers (clingon:getopt cmd :workers)))

    ;; 1. Construct IP range spec
    (let ((ip-spec
            (cond (range range)
                  ((and start-ip end-ip)
                   (format nil "~a-~a" start-ip end-ip))
                  (start-ip start-ip)
                  (t (progn
                       (format t "Error: You must provide --range, --start-ip/--end-ip, or --start-ip alone.~%")
                       (clingon:exit 1))))))
      (format t "Scan Configuration:~%")
      (format t "  IP range:  ~a~%" ip-spec)
      (format t "  Port(s):   ~a~%" port)
      (format t "  Protocol:  ~a~%" proto)
      (when proxy-url
        (format t "  Proxy:     ~a~%" proxy-url))
      (when template
        (format t "  Template:  ~a~%" template))
      (format t "  Timeout:   ~ds | Workers: ~d~%" timeout workers)
      (format t "  Output:    ~a~%" output)
      (when db (format t "  Merge DB:  ~a (sheet: ~a)~%" db (or sheet "default")))
      (format t "~%")

      ;; 2. Run scan
      (m3u-scan:scan-and-save ip-spec port output
                               :proto proto
                               :proxy-url proxy-url
                               :template template
                               :threads workers
                               :timeout timeout
                               :database-path db
                               :sheet-name sheet))))

;; --- Define scan command ---
(defun scan/command ()
  (clingon:make-command
   :name "scan"
   :description "Scan IP address ranges for available channels (multicast and unicast)"
   :usage "-r <cidr> -p <port> --proto <rtp|udp|http> -o <output> [options]"
   :examples '(("Scan multicast via proxy" . "m3utool scan -r 239.1.1.0/24 -p 5000 --proto rtp -P http://192.168.0.5:7088/udp/ -o scan.xlsx")
               ("Scan and merge to DB" . "m3utool scan -r 239.1.1.0/24 -p 5000 -P http://192.168.0.5:7088/udp/ -o scan.m3u -d core.xlsx -s Shanghai")
               ("Scan HTTP range with template" . "m3utool scan --start-ip 192.168.1.1 --end-ip 192.168.1.254 -p 8080 --proto http --template 'http://~a:~a/stream' -o scan.xlsx")
               ("Scan with custom timeout and workers" . "m3utool scan -r 239.1.1.0/24 -p 5000 -P http://proxy:7088/udp/ -t 10 -w 4 -o scan.m3u"))
   :options (scan/options)
   :handler #'scan/handler))

;; --- Define Top-Level Global Options ---
(defun top-level/options ()
  (list
   (clingon:make-option :integer
                        :description "Start Swank server on specified port for remote debugging (SLIME)"
                        :long-name "slime-port"
                        :key :slime-port)
   (clingon:make-option :integer
                        :description "Start Slynk server on specified port for remote debugging (SLY)"
                        :long-name "sly-port"
                        :key :sly-port)))

;; --- Define Pre-hook for Top-Level Command ---
(defun top-level/pre-hook (cmd)
  "Hook executed before any sub-command. Checks if Swank/Slynk server should be started."
  (let ((slime-port (clingon:getopt cmd :slime-port))
        (sly-port   (clingon:getopt cmd :sly-port)))

    ;; Port collision check: prevent both from using the exact same port
    (when (and slime-port sly-port (= slime-port sly-port))
      (format t "Error: --slime-port and --sly-port cannot use the same port number (~a).~%" slime-port)
      (clingon:exit 1))

    ;; Start Swank (for SLIME) if specified
    (when slime-port
      (format t "[DEBUG] Starting Swank Server (for SLIME) on port ~a...~%" slime-port)
      (swank:create-server :interface "0.0.0.0" :port slime-port :dont-close t))

    ;; Start Slynk (for SLY) if specified
    (when sly-port
      (format t "[DEBUG] Starting Slynk Server (for SLY) on port ~a...~%" sly-port)
      (slynk:create-server :interface "0.0.0.0" :port sly-port :dont-close t))))

(defun top-level/handler (cmd)
  (clingon:print-usage-and-exit cmd t))

(defun top-level/command ()
  (clingon:make-command
   :name "m3utool"
   :version m3u-version:*version*
   :description "Common Lisp M3U Processing Swiss Army Knife"
   :authors '("Zhiqiang Xu <xeonxu@gmail.com>")
   :license "GPLv3"
   :options (top-level/options)       ;; Register global options
   :pre-hook #'top-level/pre-hook     ;; Register the pre-hook
   :handler #'top-level/handler
   :sub-commands (list (convert/command)
                       (update/command)
                       (check/command)
                       (scan/command)
                       (server/command))))

(defun main (&optional arguments)
  (let ((app (top-level/command)))
    (clingon:run app arguments)))
