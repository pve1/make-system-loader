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

(defun write-loader-file (system-name output-file &key (if-exists :error))
  (let* ((deps (all-system-dependencies system-name))
         (all-systems (cons system-name deps))
         (fasls (alexandria:mappend #'system-fasl-files all-systems)))
    (alexandria:with-output-to-file (out output-file :if-exists if-exists)
      (dolist (fasl (reverse fasls))
        (with-standard-io-syntax
          (print `(load ,fasl) out))))))
