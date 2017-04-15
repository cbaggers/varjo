(in-package :varjo.tests)

;;------------------------------------------------------------
;; Helper macros

(defmacro compile-vert (args version allow-stemcells &body body)
  (destructuring-bind (in-args uniforms) (split-arguments args '(&uniform))
    `(first
      (v-compile ',uniforms ,version
                 :vertex '(,in-args ,@body)
                 :allow-stemcells ,allow-stemcells))))

(defmacro compile-frag (args version allow-stemcells &body body)
  (destructuring-bind (in-args uniforms) (split-arguments args '(&uniform))
    `(first
      (v-compile ',uniforms ,version
                 :fragment '(,in-args ,@body)
                 :allow-stemcells ,allow-stemcells))))

(defmacro compile-geom (args version allow-stemcells &body body)
  (destructuring-bind (in-args uniforms) (split-arguments args '(&uniform))
    `(first
      (v-compile ',uniforms ,version
                 :geometry '(,in-args ,@body)
                 :allow-stemcells ,allow-stemcells))))

(defmacro compile-vert-frag (uniforms version allow-stemcells &body body)
  `(v-compile ',uniforms ,version
              :vertex ',(first body)
              :fragment ',(second body)
              :allow-stemcells ,allow-stemcells))

(defmacro compile-vert-geom (uniforms version allow-stemcells &body body)
  `(v-compile ',uniforms ,version
              :vertex ',(first body)
              :geometry ',(second body)
              :allow-stemcells ,allow-stemcells))

(defmacro compile-vert-geom-frag (uniforms version allow-stemcells &body body)
  `(v-compile ',uniforms ,version
              :vertex ',(first body)
              :geometry ',(second body)
              :fragment ',(third body)
              :allow-stemcells ,allow-stemcells))

(defun ast-stabalizes-p (compile-result &optional (depth 0) (max-depth 20))
  "Returns t if compile the ast->code of compile-result gives the same ast
   It is allowed to recompile up to 'max-depth' times in order to find
   convergence"
  (let* ((code (ast->code compile-result))
         (version (varjo::get-version-from-context-list
                   (context compile-result)))
         (stemcells (stemcells-allowed compile-result))
         (recomp (first (v-compile
                         (mapcar #'varjo::to-arg-form
                                 (varjo::uniform-variables compile-result))
                         version
                         (stage-type compile-result)
                         (list (mapcar #'varjo::to-arg-form
                                       (varjo::input-variables compile-result))
                               code)
                         :allow-stemcells stemcells)))
         (recomp-code (ast->code recomp)))
    (or (values (equal code recomp-code) depth)
        (when (< depth max-depth)
          (ast-stabalizes-p recomp (incf depth))))))

(defmacro finishes-p (form)
  (alexandria:with-gensyms (res)
    `(let ((,res (varjo::listify ,form)))
       (is (every (lambda (x)
                    (and (typep x 'varjo-compile-result)
                         (ast-stabalizes-p x)
                         (not (glsl-contains-invalid x))
                         (not (glsl-contains-nil x))))
                  ,res)))))

(defun glsl-contains-invalid (compile-result)
  (not (null (cl-ppcre:all-matches-as-strings "<invalid>"
                                              (glsl-code compile-result)))))
(defun glsl-contains-nil (compile-result)
  ;; NIL usually means a bug
  (not (null (cl-ppcre:all-matches-as-strings "NIL"
                                              (glsl-code compile-result)))))

(defmacro glsl-contains-p (regex &body form)
  (assert (= 1 (length form)))
  `(let ((compile-result ,(first form)))
     (is (and (cl-ppcre:all-matches ,regex (glsl-code compile-result))
              (not (glsl-contains-invalid compile-result))
              (not (glsl-contains-nil compile-result))))))

(defmacro glsl-doesnt-contain-p (regex &body form)
  (assert (= 1 (length form)))
  `(let ((compile-result ,(first form)))
     (is (and (null (cl-ppcre:all-matches-as-strings
                     ,regex (glsl-code compile-result)))
              (not (glsl-contains-invalid compile-result))
              (not (glsl-contains-nil compile-result))))))

(defmacro glsl-contains-n-p (n regex &body form)
  (assert (= 1 (length form)))
  (alexandria:with-gensyms (count matches compiled)
    `(let* ((,count ,n)
            (,compiled ,(first form))
            (,matches (cl-ppcre:all-matches-as-strings
                       ,regex
                       (glsl-code ,compiled))))
       (is (and (= ,count (length ,matches))
                (not (glsl-contains-invalid ,compiled))
                (not (glsl-contains-nil ,compiled)))))))

;;------------------------------------------------------------

(5am:def-suite test-all)

(5am:def-suite void-tests :in test-all)
(5am:def-suite array-tests :in test-all)
(5am:def-suite build-tests :in test-all)
(5am:def-suite struct-tests :in test-all)
(5am:def-suite return-tests :in test-all)
(5am:def-suite stemcell-tests :in test-all)
(5am:def-suite qualifier-tests :in test-all)
(5am:def-suite assignment-tests :in test-all)
(5am:def-suite flow-control-tests :in test-all)
(5am:def-suite symbol-macro-tests :in test-all)
(5am:def-suite regular-macro-tests :in test-all)
(5am:def-suite name-shadowing-tests :in test-all)
(5am:def-suite compiler-macro-tests :in test-all)
(5am:def-suite first-class-func-tests :in test-all)
(5am:def-suite external-functions-tests :in test-all)
(5am:def-suite uninitialized-value-tests :in test-all)
(5am:def-suite multiple-value-return-tests :in test-all)

(5am:in-suite test-all)

;;------------------------------------------------------------
