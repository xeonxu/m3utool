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

;; --- 1. Define convert options ---
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
    :description "Specify the new server address (e.g., http://192.168.1.1:8080/udp/)"
    :short-name #\s
    :long-name "server"
    :key :server)
   (clingon:make-option
    :boolean/true ;; This is a flag; its presence indicates True
    :description "Strip the original proxy address, keeping only the IP:Port at the end"
    :long-name "strip-proxy"
    :key :strip-proxy)))

;; --- 2. Update to-xlsx command ---
(defun convert/to-xlsx-handler (cmd)
  (let* ((args (clingon:command-arguments cmd))
         (input-file (first args))
         (output-file (or (clingon:getopt cmd :output)
                          (namestring (default-output-path input-file "xlsx"))))
         ;; Get arguments
         (server (clingon:getopt cmd :server))
         (strip  (clingon:getopt cmd :strip-proxy)))

    (unless input-file
      (clingon:print-usage-and-exit cmd t)
      (return-from convert/to-xlsx-handler))
    (when server
      (setf strip 't))

    (format t "Converting M3U -> XLSX | Strip: ~a | Server: ~a~%" strip server)

    (let ((items (m3u-data::parse-m3u-file input-file)))
      ;; Pass to the xlsx writer
      (m3u-xlsx:save-xlsx items output-file
                                  :server-url server
                                  :strip-proxy strip))))

;; --- 3. Update to-m3u command ---
(defun convert/to-m3u-handler (cmd)
  (let* ((args (clingon:command-arguments cmd))
         (input-file (first args))
         (output-file (or (clingon:getopt cmd :output)
                          (namestring (default-output-path input-file "m3u"))))
         (server (clingon:getopt cmd :server))
         (strip  (clingon:getopt cmd :strip-proxy)))

    (unless input-file
      (clingon:print-usage-and-exit cmd t)
      (return-from convert/to-m3u-handler))

    (when server
      (setf strip 't))

    (format t "Converting Excel -> M3U | Strip: ~a | Server: ~a~%" strip server)

    (let ((items (m3u-xlsx:load-xlsx input-file)))

      ;; Key point: Before saving as M3U, iterate through the objects in memory and modify their URIs
      (dolist (item items)
        (setf (m3u-data::item-uri item)
              (m3u-data:transform-uri (m3u-data::item-uri item)
                                      :strip-proxy strip
                                      :server-url server)))

      ;; Save the modified objects
      (m3u-data::save-items-to-m3u items output-file)
      (format t "M3U save completed: ~a~%" output-file))))

(defun convert/to-xlsx-command ()
  (clingon:make-command
   :name "to-xlsx"
   :description "M3U -> Excel (Supports stripping proxies and replacing servers)"
   :usage "INPUT-FILE [-o OUT] [-s SERVER] [--strip-proxy]"
   :options (convert/options) ;; Use common options
   :handler #'convert/to-xlsx-handler))

(defun convert/to-m3u-command ()
  (clingon:make-command
   :name "to-m3u"
   :description "Excel -> M3U (Supports stripping proxies and replacing servers)"
   :usage "INPUT-FILE [-o OUT] [-s SERVER] [--strip-proxy]"
   :options (convert/options) ;; Use common options
   :handler #'convert/to-m3u-handler))

;; --- Add these functions to your src/m3u-cli.lisp ---

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

;; --- Main definitions ---
(defun top-level/handler (cmd)
  (clingon:print-usage-and-exit cmd t))

(defun top-level/command ()
  (clingon:make-command
   :name "m3utool"
   :version "0.1"
   :description "Common Lisp M3U Processing Swiss Army Knife"
   :authors '("Zhiqiang Xu <xeonxu@gmail.com>")
   :license "GPLv3"
   :handler #'top-level/handler
   :sub-commands (list (convert/to-xlsx-command)
                       (convert/to-m3u-command)
                       (check/command))))

;; New: Silent startup to suppress 'deploy' verbose logs
(when (find-package :deploy)
  (setf (symbol-value (find-symbol "*STATUS-OUTPUT*" :deploy)) nil))

(defun main (&optional arguments)
  (let ((app (top-level/command)))
    (clingon:run app arguments)))
