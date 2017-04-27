(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Geometry

;; points
;; line-strip
;; triangle-strip

(def-metadata-kind output-primitive (:binds-to :scope)
  kind
  max-vertices)

;;------------------------------------------------------------
;; Tessellation Control

(def-metadata-kind output-patch (:binds-to :scope)
  vertices)

;;------------------------------------------------------------
;; Tessellation Evaluation

(def-metadata-kind tessellate-to (:binds-to :scope)
  primitive
  spacing
  order)

;;------------------------------------------------------------
;; emit


(v-defspecial emit-data (&optional (form '(values)))
  :args-valid t
  :context :geometry
  :return
  (let* ((new-env (fresh-environment env :multi-val-base "return"))
         ;; we create an environment with the signal to let any 'values' forms
         ;; down the tree know they will be caught and what their name prefix
         ;; should be.
         ;; We then compile the form using the augmented environment, the
         ;; values statements will expand and flow back as 'multi-vals' and the
         ;; current-line
         (code-obj (compile-form form new-env))
         (result (%emit code-obj new-env))
         (ast (ast-node! 'emit (node-tree code-obj)
                         (code-type result)
                         env env))
         (emit-set (or (emit-set result)
                       (error 'nil-emit-set
                              :form (list 'emit form)
                              :possible-set (emit-set code-obj)))))
    ;;0
    (values (copy-code result :node-tree ast :emit-set emit-set)
            env)))

;; Used when this is the main stage function
(defun %emit (code-obj env)
  ;; If you make changes here, look at %main-return to see if it needs
  ;; similar changes
  (let ((type (v-type-of code-obj)))
    (assert (not (v-typep type (type-spec->type :void))) ()
            "Varjo: emit-data cannot emit void")
    (cond
      ((multi-vals code-obj)
       (let* ((mvals (multi-vals code-obj))
              (v-vals (mapcar #'multi-val-value mvals))
              (types (mapcar #'v-type-of v-vals))
              (glsl-lines (mapcar #'glsl-name v-vals)))
         (copy-code
          (merge-progn
           (with-fresh-env-scope (fresh-env env)
             (env-> (p-env fresh-env)
               (merge-multi-env-progn
                (%mapcar-multi-env-progn
                 (lambda (p-env type gname)
                   (compile-let (gensym) (type->type-spec type)
                                nil p-env gname))
                 p-env types glsl-lines))
               ;; We compile these ↓↓, however we dont include them in the ast
               (compile-form (%default-out-for-stage code-obj p-env)
                             p-env)
               (compile-form (mvals->out-form code-obj p-env)
                             p-env)))
           env)
          :emit-set (make-emit-set-from-code-obj code-obj))))
      (t (let ((emit-set (if (typep (stage env) 'vertex-stage)
                             (make-emit-set)
                             (make-emit-set (make-emit-val type)))))
           (copy-code
            (with-fresh-env-scope (fresh-env env)
              (compile-form (%default-out-for-stage code-obj fresh-env)
                            fresh-env))
            :emit-set emit-set
            :pure nil))))))

;;------------------------------------------------------------

(v-defmacro emit ((&key point-size) position &rest data)
  `(progn
     ,@(when point-size `((setf gl-point-size ,point-size)))
     (setf gl-position ,position)
     (emit-data (values ,@data))
     (emit-vertex)))
