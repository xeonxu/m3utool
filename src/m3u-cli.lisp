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

;; --- 1. Define common options ---
(defun common-options ()
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
(defun to-xlsx-handler (cmd)
  (let* ((args (clingon:command-arguments cmd))
         (input-file (first args))
         (output-file (or (clingon:getopt cmd :output)
                          (namestring (default-output-path input-file "xlsx"))))
         ;; Get arguments
         (server (clingon:getopt cmd :server))
         (strip  (clingon:getopt cmd :strip-proxy)))

    (unless input-file
      (clingon:print-usage-and-exit cmd t)
      (return-from to-xlsx-handler))
    (when server
      (setf strip 't))

    (format t "Converting M3U -> XLSX | Strip: ~a | Server: ~a~%" strip server)
    
    (let ((items (m3u-data::parse-m3u-file input-file)))
      ;; Pass to the xlsx writer
      (m3u-xlsx:save-xlsx items output-file
                                  :server-url server 
                                  :strip-proxy strip))))

;; --- 3. Update to-m3u command ---
(defun to-m3u-handler (cmd)
  (let* ((args (clingon:command-arguments cmd))
         (input-file (first args))
         (output-file (or (clingon:getopt cmd :output)
                          (namestring (default-output-path input-file "m3u"))))
         (server (clingon:getopt cmd :server))
         (strip  (clingon:getopt cmd :strip-proxy)))

    (unless input-file
      (clingon:print-usage-and-exit cmd t)
      (return-from to-m3u-handler))

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

(defun to-xlsx-command ()
  (clingon:make-command
   :name "to-xlsx"
   :description "M3U -> Excel (Supports stripping proxies and replacing servers)"
   :usage "INPUT-FILE [-o OUT] [-s SERVER] [--strip-proxy]"
   :options (common-options) ;; Use common options
   :handler #'to-xlsx-handler))

(defun to-m3u-command ()
  (clingon:make-command
   :name "to-m3u"
   :description "Excel -> M3U (Supports stripping proxies and replacing servers)"
   :usage "INPUT-FILE [-o OUT] [-s SERVER] [--strip-proxy]"
   :options (common-options) ;; Use common options
   :handler #'to-m3u-handler))

;; --- Main definitions ---
(defun top-level/handler (cmd)
  (clingon:print-usage-and-exit cmd t))

(defun top-level/command ()
  (clingon:make-command
   :name "m3utool"
   :version "0.1"
   :description "Common Lisp M3U Processing Swiss Army Knife"
   :authors '("Zhiqiang Xu <xeonxu@gmail.com>")
   :license "GPL V3"
   :handler #'top-level/handler
   :sub-commands (list (to-xlsx-command)
                       (to-m3u-command))))

;; New: Silent startup to suppress 'deploy' verbose logs
(when (find-package :deploy)
  (setf (symbol-value (find-symbol "*STATUS-OUTPUT*" :deploy)) nil))

(defun main (&optional arguments)
  (let ((app (top-level/command)))
    (clingon:run app arguments)))
