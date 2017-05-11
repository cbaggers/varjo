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
  (labels ((stage->name (stage)
             (let ((type-name (type-of stage)))
               (elt varjo::*stage-names*
                    (position type-name varjo::*stage-type-names*)))))
    (let* ((code (ast->code compile-result))
           (version (varjo::get-version-from-context-list
                     (context compile-result)))
           (stemcells (stemcells-allowed compile-result))
           (primitive-in (primitive-in compile-result))
           (recomp (first (v-compile
                           (mapcar #'varjo::to-arg-form
                                   (varjo::uniform-variables compile-result))
                           version
                           (stage->name (starting-stage compile-result))
                           `(,(mapcar #'varjo::to-arg-form
                                      (varjo::input-variables compile-result))
                              ,@code)
                           :allow-stemcells stemcells
                           :draw-mode primitive-in)))
           (recomp-code (ast->code recomp)))
      (or (values (equal code recomp-code) depth)
          (when (< depth max-depth)
            (ast-stabalizes-p recomp (incf depth)))))))

(defmacro finishes-p (form)
  (alexandria:with-gensyms (res)
    `(let ((,res (varjo::listify ,form)))
       (is (every (lambda (x)
                    (and (typep x 'compiled-stage)
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

(defmacro glsl-contains-all-p ((&rest regexes) &body form)
  (assert (= 1 (length form)))
  (let ((gvars (loop :for i :below (length regexes) :collect (gensym))))
    (alexandria:with-gensyms (compiled)
      `(let* ((,compiled ,(first form))
              ,@(loop :for g :in gvars :for r :in regexes :collect
                   `(,g (cl-ppcre:all-matches-as-strings
                         ,r (glsl-code ,compiled)))))
         (is (and (or (and ,@gvars)
                      (map nil #'print (list ,@gvars)))
                  (not (glsl-contains-invalid ,compiled))
                  (not (glsl-contains-nil ,compiled))))))))

(defmacro glsl-contains-1-of-all-p ((&rest regexes) &body form)
  (assert (= 1 (length form)))
  (let ((gvars (loop :for i :below (length regexes) :collect (gensym))))
    (alexandria:with-gensyms (compiled)
      `(let* ((,compiled ,(first form))
              ,@(loop :for g :in gvars :for r :in regexes :collect
                   `(,g (cl-ppcre:all-matches-as-strings
                         ,r (glsl-code ,compiled)))))
         (is (and (or (every (lambda (x) (= (length x) 1))
                             (list ,@gvars))
                      (map nil #'print (list ,@gvars)))
                  (not (glsl-contains-invalid ,compiled))
                  (not (glsl-contains-nil ,compiled))))))))

;;------------------------------------------------------------

(defmacro def-finishes-test (name (&key suite) &body body)
  `(progn
     (defun ,name () ,@body)
     (def-test ,name (:suite ,suite)
       (finishes (,name)))))

(defmacro def-is-true-test (name (&key suite) &body body)
  `(progn
     (defun ,name () ,@body)
     (def-test ,name (:suite ,suite)
       (is-true (,name)))))

(defmacro def-dbind-test (name (&key suite) bind-vars test-form &body body)
  `(progn
     (defun ,name () ,@body)
     (def-test ,name (:suite ,suite)
       (destructuring-bind ,bind-vars (,name)
         ,test-form))))

(defmacro def-vbind-test (name (&key suite) bind-vars test-form &body body)
  `(progn
     (defun ,name () ,@body)
     (def-test ,name (:suite ,suite)
       (vbind ,bind-vars (,name)
         ,test-form))))

(defun make-env (stage-kind
                 &optional in-args uniforms (version :450) allow-stemcells)
  (let* ((stage (make-stage stage-kind in-args uniforms (list version) allow-stemcells))
         (env (varjo::%make-base-environment stage)))
    (varjo::pipe-> (stage env)
      #'varjo::process-primitive-type
      #'varjo::add-context-glsl-vars
      #'varjo::expand-input-variables
      #'varjo::process-uniforms
      #'(lambda (stage env)
          (values env stage)))))

;;------------------------------------------------------------

(5am:def-suite test-all)

(5am:def-suite let-tests :in test-all)
(5am:def-suite void-tests :in test-all)
(5am:def-suite array-tests :in test-all)
(5am:def-suite build-tests :in test-all)
(5am:def-suite struct-tests :in test-all)
(5am:def-suite return-tests :in test-all)
(5am:def-suite flow-id-tests :in test-all)
(5am:def-suite metadata-tests :in test-all)
(5am:def-suite stemcell-tests :in test-all)
(5am:def-suite qualifier-tests :in test-all)
(5am:def-suite assignment-tests :in test-all)
(5am:def-suite inline-glsl-tests :in test-all)
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
