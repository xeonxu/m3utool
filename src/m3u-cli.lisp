(eval-when (:load-toplevel :compile-toplevel :execute)
  (ql:quickload '(:clingon)))

(defpackage :m3u-cli
  (:use :cl)
  (:import-from :clingon)
  (:export :main))

(in-package :m3u-cli)

(defun default-output-path (input-path new-ext)
  "E.g., input.m3u -> input.xlsx"
  (let ((p (pathname input-path)))
    (make-pathname :type new-ext :defaults p)))

;; --- Define convert options ---
(defun convert/options ()
  (list
   (clingon:make-option
    :string
    :description "Specify the output filename"
    :short-name #\o
    :long-name "output"
    :key :output)
   (clingon:make-option
    :string
    :description "Change proxy address to the new server address (e.g., http://192.168.1.1:8080/udp/). It also implemented the strip function."
    :short-name #\S
    :long-name "server"
    :key :server)
   (clingon:make-option
    :boolean/true ;; This is a flag; its presence indicates True
    :description "Strip the original proxy address, keeping only the protocal://IP:Port at the end"
    :short-name #\s
    :long-name "strip-proxy"
    :key :strip-proxy)))

;; --- Define convert command ---
(defun convert/handler (cmd)
  (let* ((args (clingon:command-arguments cmd))
         (input-file (first args))
         (server (clingon:getopt cmd :server))
         (strip  (clingon:getopt cmd :strip-proxy)))

    ;; Check if input file is provided
    (when (or (not input-file) (= (length input-file) 0))
      (clingon:print-usage-and-exit cmd t)
      (return-from convert/handler))

    ;; Automatically enable strip if server URL is provided
    (when server
      (setf strip 't))

    ;; Extract file extension and determine conversion direction
    (let* ((ext (string-downcase (pathname-type (pathname input-file))))
           (is-m3u (or (string= ext "m3u") (string= ext "m3u8")))
           (is-xlsx (or (string= ext "xlsx") (string= ext "xls")))
           ;; Determine default output file based on input type
           (output-file (or (clingon:getopt cmd :output)
                            (namestring (default-output-path input-file (if is-m3u "xlsx" "m3u"))))))

      (cond
        ;; Branch A: M3U -> XLSX
        (is-m3u
         (format t "Converting M3U -> XLSX | Strip: ~a | Server: ~a~%" strip server)
         (let ((items (m3u-data::parse-m3u-file input-file)))
           (m3u-xlsx:save-xlsx items output-file
                               :server-url server
                               :strip-proxy strip)))

        ;; Branch B: XLSX -> M3U
        (is-xlsx
         (format t "Converting XLSX -> M3U | Strip: ~a | Server: ~a~%" strip server)
         (let ((items (m3u-xlsx:load-xlsx input-file)))
           ;; Transform URIs in memory before saving
           (dolist (item items)
             (setf (m3u-data::item-uri item)
                   (m3u-data:transform-uri (m3u-data::item-uri item)
                                           :strip-proxy strip
                                           :server-url server)))
           ;; Save the modified objects to M3U
           (m3u-data::save-items-to-m3u items output-file)
           (format t "M3U generation complete -> ~a~%" output-file)))

        ;; Branch C: Unsupported format
        (t
         (format t "Error: Unsupported input file extension '.~a'. Only .m3u, .m3u8, and .xlsx are supported.~%" ext)
         (clingon:exit 1))))))

(defun convert/command ()
  (clingon:make-command
   :name "convert"
   :description "Convert between M3U and XLSX (Auto-detected by file extension)"
   :usage "INPUT-FILE [-o OUT] [--server SERVER] [--strip-proxy]"
   :options (convert/options)
   :handler #'convert/handler)
  )

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

;; --- Define check command ---
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
                        :description "Port to listen on (default: 8080)"
                        :short-name #\p :long-name "port"
                        :key :port :initial-value 8080)
   (clingon:make-option :string
                        :description "Specify the new server address"
                        :short-name #\s :long-name "server"
                        :key :server)
   (clingon:make-option :boolean/true
                        :description "Strip the original proxy address"
                        :long-name "strip-proxy"
                        :key :strip-proxy)))

;; Define Server Handler
(defun server/handler (cmd)
  (let ((input (clingon:getopt cmd :input))
        (bind (clingon:getopt cmd :bind))
        (port (clingon:getopt cmd :port))
        (server (clingon:getopt cmd :server))
        (strip (clingon:getopt cmd :strip-proxy)))

    (unless input
      (format t "Error: You must provide an input XLSX file using -i or --input.~%")
      (clingon:exit 1))

    (when server
      (setf strip 't))

    ;; Delegate to the m3u-server module
    (m3u-server:start-server input bind port server strip)))

;; Build the Command Object
(defun server/command ()
  (clingon:make-command
   :name "server"
   :description "Start an HTTP server to serve a dynamic M3U playlist from an XLSX file"
   :options (server/options)
   :handler #'server/handler))

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
      (swank:create-server :port slime-port :dont-close t))

    ;; Start Slynk (for SLY) if specified
    (when sly-port
      (format t "[DEBUG] Starting Slynk Server (for SLY) on port ~a...~%" sly-port)
      (slynk:create-server :port sly-port :dont-close t))))

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
                       (check/command)
                       (server/command))))

(defun main (&optional arguments)
  (let ((app (top-level/command)))
    (clingon:run app arguments)))
