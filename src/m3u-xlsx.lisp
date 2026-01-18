(eval-when (:load-toplevel :compile-toplevel :execute)
  (ql:quickload '(:cl-excel)))

(defpackage :m3u-xlsx
  (:use :cl)
  (:export :save-xlsx :load-xlsx))

(in-package :m3u-xlsx)

;; ============== Hot Patch Section ==============
;; Ensure the cxml-stp package is loaded to patch the bug
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-method #'cxml-stp:data '() '(null) nil)
    (defmethod cxml-stp:data ((object null))
      "Hot-patch: Allow calling data on NIL, returning an empty string"
      "")))
;; ===============================================

;; 1. Writer (With URI transformation support)
(defun save-xlsx (items output-path &key server-url strip-proxy)
  "Write items to an .xlsx file, optionally stripping proxy and appending server prefix."
  (let* ((attr-keys (m3u-data::collect-all-attribute-keys items))
         (headers (append '("Duration" "Title" "URI") attr-keys))
         (data-rows (mapcar (lambda (item)
                              (let* ((raw-uri (m3u-data::item-uri item))
                                     ;; --- Call core transformation function ---
                                     (final-uri (m3u-data:transform-uri 
                                                 raw-uri 
                                                 :strip-proxy strip-proxy
                                                 :server-url server-url)))
                                (append 
                                 (list (m3u-data::item-duration item)
                                       (m3u-data::item-title item)
                                       final-uri) ;; Use the transformed URI
                                 (mapcar (lambda (k) (gethash k (m3u-data::item-attributes item) ""))
                                         attr-keys))))
                            items))
         (all-rows (cons headers data-rows)))
    
    (cl-excel:write-xlsx all-rows output-path :sheet "Channel List")
    (format t "XLSX generation complete (Strip: ~a, Server: ~a) -> ~a~%"
            strip-proxy server-url output-path)))

;; 2. Reader (Safe version)
(defun load-xlsx (input-path)
  "Read .xlsx file and restore objects."
    (let* ((rows (cl-excel:read-file input-path))) 
    
    (unless rows (error "Excel file is empty or cannot be read"))
    
    (let* ((headers (first rows))
           (data-rows (rest rows))
           (items '()))
      
      (format t "Headers found: ~a~%" headers)
      
      (dolist (row data-rows)
        ;; Data read by xlsx might be number, string, or nil.
        ;; Even though we patched the 'data' method, the row structure itself might contain nils.
        ;; So we perform a defensive conversion here.
        (let* ((clean-row (mapcar (lambda (cell) (format nil "~a" cell)) row))
               (clean-headers (mapcar (lambda (h) (format nil "~a" h)) headers)))
          
          ;; Only process if the row has enough content
          (when (>= (length clean-row) 3)
            (push (m3u-data::flexible-csv-row-to-item clean-row clean-headers) items))))
      
      (nreverse items))))
