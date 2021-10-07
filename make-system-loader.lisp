;;;; make-system-loader.lisp

(in-package #:make-system-loader)

(defun system-fasl-files (system-name)
  (loop :for c :in (asdf:required-components
                    (asdf:find-system system-name))
        :when (typep c 'asdf:cl-source-file)
          :collect (asdf:output-file 'asdf:compile-op c)))

(defun all-system-dependencies (system-name)
  (labels ((dependencies (system-name)
             (let* ((deps (asdf:system-depends-on
                           (asdf:find-system system-name))))
               (append deps (alexandria:mappend #'dependencies deps)))))
    (remove-duplicates (dependencies system-name)
                       :test #'string=
                       :from-end t)))

(defun check-main-function-arg (main-function)
  (when main-function
    (unless (or (symbolp main-function)
                (and (listp main-function)
                     (= 2 (length main-function))))
      (error #.(format nil "MAIN-FUNCTION should either be a symbol ~~
or a list of two string designators, i.e. (\"MYPACKAGE\" \"MAIN\").")))))

(defun write-loader-file (system-name output-file &key main-function
                                                       (if-exists :error))
  (check-main-function-arg main-function)
  (let* ((deps (all-system-dependencies system-name))
         (all-systems-in-load-order (reverse (cons system-name deps)))
         (fasls (alexandria:mappend #'system-fasl-files all-systems-in-load-order)))
    (alexandria:with-output-to-file (out output-file :if-exists if-exists)
      (dolist (fasl fasls)
        (with-standard-io-syntax
          (print `(load ,fasl) out)))
      (when main-function
        (let ((function-name (if (symbolp main-function)
                                 (symbol-name main-function)
                                 (string (first main-function))))
              (function-package (if (symbolp main-function)
                                    (package-name (symbol-package main-function))
                                    (string (second main-function)))))
          (print `(funcall (find-symbol ,function-name ,function-package))
                 out))))))
