(in-package :varjo.tests)

;;------------------------------------------------------------
;; Helper macros

(defmacro compile-vert (args version allow-stemcells &body body)
  (destructuring-bind (in-args uniforms) (varjo.internals::split-arguments args '(&uniform))
    `(first
      (v-compile ',uniforms ,version
                 :vertex '(,in-args ,@body)
                 :allow-stemcells ,allow-stemcells))))

(defmacro compile-frag (args version allow-stemcells &body body)
  (destructuring-bind (in-args uniforms) (varjo.internals::split-arguments args '(&uniform))
    `(first
      (v-compile ',uniforms ,version
                 :fragment '(,in-args ,@body)
                 :allow-stemcells ,allow-stemcells))))

(defmacro compile-geom (args version allow-stemcells &body body)
  (destructuring-bind (in-args uniforms) (varjo.internals::split-arguments args '(&uniform))
    `(first
      (v-compile ',uniforms ,version
                 :vertex '(() (v! 1 2 3 4))
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

(defmacro compile-compute (args version allow-stemcells &body body)
  (destructuring-bind (in-args uniforms) (varjo.internals::split-arguments args '(&uniform))
    (assert (null in-args))
    `(first
      (v-compile ',uniforms ,version
                 :compute '(,in-args ,@body)
                 :allow-stemcells ,allow-stemcells
                 :draw-mode nil))))

(defun ast-stabalizes-p (compile-result &optional (depth 0) (max-depth 20))
  "Returns t if compile the ast->code of compile-result gives the same ast
   It is allowed to recompile up to 'max-depth' times in order to find
   convergence"
  (labels ((stage->name (stage)
             (let ((type-name (type-of stage)))
               (elt varjo::*stage-names*
                    (position type-name varjo::*stage-type-names*)))))
    (let* ((code (ast->code compile-result))
           (version (varjo.internals::get-version-from-context-list
                     (context compile-result)))
           (stemcells (stemcells-allowed compile-result))
           (primitive-in (primitive-in compile-result))
           (recomp (first (v-compile
                           (mapcar #'varjo.internals:to-arg-form
                                   (varjo::uniform-variables compile-result))
                           version
                           (stage->name (varjo.internals::starting-stage compile-result))
                           `(,(mapcar #'varjo.internals:to-arg-form
                                      (varjo::input-variables compile-result))
                              ,@code)
                           :allow-stemcells stemcells
                           :draw-mode primitive-in)))
           (recomp-code (ast->code recomp)))
      (or (values (equal code recomp-code) depth)
          (when (< depth max-depth)
            (ast-stabalizes-p recomp (incf depth)))))))

(defmacro finishes-p (form)
  (alexandria:with-gensyms (res elem)
    `(let ((,res (varjo::listify ,form)))
       (loop :for ,elem :in ,res :do
          (is-true (typep ,elem 'compiled-stage))
          (is-true (ast-stabalizes-p ,elem))
          (is-false (glsl-contains-invalid ,elem))
          (is-false (glsl-contains-nil ,elem))
          (glsl-compiles-p ,res))
       ,res)))

(defmacro finishes-p-no-test-compile (form)
  (alexandria:with-gensyms (res elem)
    `(let ((,res (varjo::listify ,form)))
       (loop :for ,elem :in ,res :do
          (is-true (typep ,elem 'compiled-stage))
          (is-true (ast-stabalizes-p ,elem))
          (is-false (glsl-contains-invalid ,elem))
          (is-false (glsl-contains-nil ,elem)))
       ,res)))

(defun glsl-compiles-p (elem)
  "This function only exists so cepl.tests can replace it with an
implementation that compiles the code for real"
  (declare (ignore elem))
  nil)

(defun glsl-contains-invalid (compile-result)
  (not (null (cl-ppcre:all-matches-as-strings "£.*£"
                                              (glsl-code compile-result)))))
(defun glsl-contains-nil (compile-result)
  ;; NIL usually means a bug
  (not (null (cl-ppcre:all-matches-as-strings "NIL"
                                              (glsl-code compile-result)))))

(defmacro glsl-contains-p (regex &body form)
  (assert (= 1 (length form)))
  `(let ((compile-result ,(first form)))
     (is-true (cl-ppcre:all-matches ,regex (glsl-code compile-result)))
     (is-false (glsl-contains-invalid compile-result))
     (is-false (glsl-contains-nil compile-result))))

(defmacro glsl-doesnt-contain-p (regex &body form)
  (assert (= 1 (length form)))
  `(let ((compile-result ,(first form)))
     (is-true (null (cl-ppcre:all-matches-as-strings
                     ,regex (glsl-code compile-result))))
     (is-false (glsl-contains-invalid compile-result))
     (is-false (glsl-contains-nil compile-result))))

(defmacro glsl-contains-n-p (n regex &body form)
  (assert (= 1 (length form)))
  (alexandria:with-gensyms (count matches compiled)
    `(let* ((,count ,n)
            (,compiled ,(first form))
            (,matches (cl-ppcre:all-matches-as-strings
                       ,regex
                       (glsl-code ,compiled))))
       (is-true (= ,count (length ,matches)))
       (is-false (glsl-contains-invalid ,compiled))
       (is-false (glsl-contains-nil ,compiled)))))

(defmacro glsl-contains-all-p ((&rest regexes) &body form)
  (assert (= 1 (length form)))
  (let ((gvars (loop :for i :below (length regexes) :collect (gensym))))
    (alexandria:with-gensyms (compiled)
      `(let* ((,compiled ,(first form))
              ,@(loop :for g :in gvars :for r :in regexes :collect
                   `(,g (cl-ppcre:all-matches-as-strings
                         ,r (glsl-code ,compiled)))))
         (is-true (or (and ,@gvars)
                      (map nil #'print (list ,@gvars))))
         (is-false (glsl-contains-invalid ,compiled))
         (is-false (glsl-contains-nil ,compiled))))))

(defmacro glsl-contains-1-of-all-p ((&rest regexes) &body form)
  (assert (= 1 (length form)))
  (let ((gvars (loop :for i :below (length regexes) :collect (gensym))))
    (alexandria:with-gensyms (compiled)
      `(let* ((,compiled ,(first form))
              ,@(loop :for g :in gvars :for r :in regexes :collect
                   `(,g (cl-ppcre:all-matches-as-strings
                         ,r (glsl-code ,compiled)))))
         (is-true (or (every (lambda (x) (= (length x) 1))
                             (list ,@gvars))
                      (map nil #'print (list ,@gvars))))
         (is-false (glsl-contains-invalid ,compiled))
         (is-false (glsl-contains-nil ,compiled))))))

;;------------------------------------------------------------

(defmacro define-finishes-test (name (&key suite) &body body)
  `(progn
     (defun ,name () ,@body)
     (def-test ,name (:suite ,suite)
       (finishes (,name)))))

(defmacro define-is-true-test (name (&key suite) &body body)
  `(progn
     (defun ,name () ,@body)
     (def-test ,name (:suite ,suite)
       (is-true (,name)))))

(defmacro define-dbind-test (name (&key suite) bind-vars test-form &body body)
  `(progn
     (defun ,name () ,@body)
     (def-test ,name (:suite ,suite)
       (destructuring-bind ,bind-vars (,name)
         ,test-form))))

(defmacro define-vbind-test (name (&key suite) bind-vars test-form &body body)
  `(progn
     (defun ,name () ,@body)
     (def-test ,name (:suite ,suite)
       (vbind ,bind-vars (,name)
         ,test-form))))

(defun make-env (stage-kind
                 &optional in-args uniforms (version :410) allow-stemcells)
  (let* ((stage (make-stage stage-kind in-args uniforms (list version) allow-stemcells))
         (env (varjo.internals::%make-base-environment stage)))
    (varjo.internals::add-glsl-funcs env)
    (varjo::pipe-> (stage env)
      #'varjo.internals::process-primitive-type
      #'varjo.internals::add-context-glsl-vars
      #'varjo.internals::expand-input-variables
      #'varjo.internals::process-uniforms
      #'(lambda (stage env)
          (values env stage)))))

;;------------------------------------------------------------
