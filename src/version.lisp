(defpackage :m3u-version
  (:use :cl)
  (:export :*version*))

(in-package :m3u-version)

;; Dynamically read version.txt at compile time using ASDF system source directory.
;; Fallback to "latest" if not found.
(defparameter *version*
  (or (ignore-errors
        (let* ((sys-dir (asdf:system-source-directory "m3utool"))
               (version-file (merge-pathnames "version.txt" sys-dir)))
          (with-open-file (stream version-file :direction :input :if-does-not-exist nil)
            (when stream
              (string-trim '(#\Space #\Tab #\Newline #\Return) (read-line stream))))))
      "latest"))
