;;;; make-system-loader.lisp

(in-package #:make-system-loader)

(defun system-fasl-files (system-name)
  (loop :for c :in (asdf:required-components
                    (asdf:find-system system-name))
        :when (typep c 'asdf:cl-source-file)
          :collect (asdf:output-file 'asdf:compile-op c)))

(defun all-system-dependencies (system-name)
  (labels ((dependencies (system-name)
             (let* ((system (asdf:find-system system-name))
                    (deps (asdf:system-depends-on system))
                    (resolved-deps
                      (alexandria:mappend
                       (lambda (s)
                         (let ((dep (asdf/find-component:resolve-dependency-spec
                                     system s)))
                           (when dep
                             (list (asdf:component-name dep)))))
                               deps)))
               (append resolved-deps
                       (alexandria:mappend #'dependencies resolved-deps)))))
    (remove-duplicates (dependencies system-name) :test #'string=)))

(defun check-main-function-arg (main-function)
  (when main-function
    (unless (or (symbolp main-function)
                (and (listp main-function)
                     (= 2 (length main-function))))
      (error #.(format nil "MAIN-FUNCTION should either be a symbol ~~
or a list of two string designators, i.e. (\"MYPACKAGE\" \"MAIN\").")))))

(defun write-loader-file (system-name output-file &key main-function
                                                       main-lisp-file
                                                       initialization-form
                                                       (if-exists :error))
  (check-main-function-arg main-function)
  (setf system-name (string-downcase system-name))
  (let* ((deps (all-system-dependencies system-name))
         (all-systems-in-load-order (reverse (cons system-name deps)))
         (fasls (alexandria:mappend #'system-fasl-files all-systems-in-load-order))
         ;; "Require" systems like sb-posix.
         (requires (remove-if-not
                    (lambda (x)
                      (typep (asdf:find-system x) 'asdf:require-system))
                    all-systems-in-load-order)))
    (alexandria:with-output-to-file (out output-file :if-exists if-exists)
      (with-standard-io-syntax
        (when initialization-form
          (print initialization-form out))
        (dolist (require requires)
          (print `(require ,(string-upcase require)) out))
        (dolist (fasl fasls)
          (print `(load ,fasl) out))
        (when main-lisp-file
          (print `(load ,(probe-file main-lisp-file)) out))
        (when main-function
          (let ((function-name (if (symbolp main-function)
                                   (symbol-name main-function)
                                   (string (second main-function))))
                (function-package (if (symbolp main-function)
                                      (package-name (symbol-package main-function))
                                      (string (first main-function)))))
            (print `(funcall (find-symbol ,function-name ,function-package))
                   out)))))))
