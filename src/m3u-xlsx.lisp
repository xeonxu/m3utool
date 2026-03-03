(eval-when (:load-toplevel :compile-toplevel :execute)
  (ql:quickload '(:cl-excel)))

(defpackage :m3u-xlsx
  (:use :cl)
  (:export :read-xlsx-items
           :write-xlsx-items))

(in-package :m3u-xlsx)

;; ============== Hot Patch Section ==============
;; Ensure the cxml-stp package is loaded to patch the bug
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-method #'cxml-stp:data '() '(null) nil)
    (defmethod cxml-stp:data ((object null))
      "Hot-patch: Allow calling data on NIL, returning an empty string"
      "")))
;; ===============================================

(defun clean-missing-value (val)
  "Convert cl-excel's #<MISSING> token or nil to an empty string."
  (if (or (null val)
          (equal (format nil "~a" val) "#<MISSING>"))
      ""
      (format nil "~a" val)))

(defun read-xlsx-items (input-path &key sheet-name)
  "Read an existing Excel file and convert its rows into playlist-item objects.
   Currently ignores sheet-name to read the active sheet by default."
  (declare (ignore sheet-name))
  (let* ((rows (cl-excel:read-file input-path)))
    (unless rows (error "Excel file is empty or cannot be read: ~a" input-path))

    (let* ((headers (mapcar #'clean-missing-value (first rows)))
           (data-rows (rest rows))
           (items '()))

      (dolist (row data-rows)
        (let ((clean-row (mapcar #'clean-missing-value row)))
          ;; Only process if the row has enough content for the base fields
          (when (>= (length clean-row) 3)
            (push (m3u-data:flexible-csv-row-to-item clean-row headers) items))))

      (format t "[INFO] Loaded ~a records from ~a.~%" (length items) input-path)
      (nreverse items))))

(defun gather-all-headers (items)
  "Scan all items to find all unique attribute keys (for dynamic columns).
   Preserves the exact order of the fixed columns."
  (let ((dyn-headers nil))
    (dolist (item items)
      (maphash (lambda (k v)
                 (declare (ignore v))
                 ;; Collect dynamic keys
                 (pushnew k dyn-headers :test #'equal))
               (m3u-data:item-attributes item)))
    ;; Reverse the dynamic headers to chronological order,
    ;; then safely append them AFTER the fixed headers.
    (append (list "Duration" "Title" "URI") (nreverse dyn-headers))))

(defun write-xlsx-items (items output-path &key sheet-name)
  "Write a list of playlist-item objects to an XLSX file.
   It handles dynamic attributes automatically."
  (declare (ignore sheet-name))
  (let ((headers (gather-all-headers items))
        (table-data nil))

    ;; 1. Push Header Row
    (push headers table-data)

    ;; 2. Process Data Rows
    (dolist (item items)
      ;; Keep fixed columns and dynamic columns separated during construction
      (let ((fixed-cols (list (m3u-data:item-duration item)
                              (m3u-data:item-title item)
                              (m3u-data:item-uri item)))
            (dyn-cols nil))

        ;; Collect dynamic attributes
        (dolist (header (cdddr headers)) ; Skip the first 3 fixed headers
          (push (gethash header (m3u-data:item-attributes item) "") dyn-cols))

        ;; Reverse dynamic columns to match header order, append to fixed columns, and push to table
        (push (append fixed-cols (nreverse dyn-cols)) table-data)))

    ;; Write to Excel (nreverse table-data to restore row order)
    (cl-excel:write-xlsx (nreverse table-data) output-path :sheet "Channel List")
    (format t "[SUCCESS] Saved ~a items to ~a.~%" (length items) output-path)))
