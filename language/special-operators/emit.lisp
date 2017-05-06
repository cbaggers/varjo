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
  :return
  (let ((new-env (fresh-environment
                  env :multi-val-base *emit-var-name-base*)))
    ;; we create an environment with the signal to let any 'values' forms
    ;; down the tree know they will be caught and what their name prefix should
    ;; be.
    ;; We then compile the form using the augmented environment, the values
    ;; statements will expand and flow back as 'multi-vals' and the
    ;; current-line
    ;;
    ;; now there are two styles of return:
    ;; - The first is for a regular function, in which multivals become
    ;;   out-arguments and the current-line is returned
    ;; - The second is for a shader stage in which the multi-vars become
    ;;   output-variables and the current line is handled in a 'context'
    ;;   specific way.
    ;;
    ;; If you make changes here, look at #'emit to see if it needs
    ;; similar changes
    (vbind (code-obj final-env) (compile-form form new-env)
      (if (emit-set code-obj)
          (let ((ast (ast-node! 'emit-data
                                (node-tree code-obj)
                                (make-type-set)
                                env env)))
            (values (copy-compiled code-obj
                                   :type-set (make-type-set)
                                   :node-tree ast)
                    final-env))
          (%values-for-emit (list code-obj)
                            (list (extract-value-qualifiers code-obj))
                            final-env)))))

;;------------------------------------------------------------

(v-defmacro emit ((&key point-size) position &rest data)
  `(progn
     ,@(when point-size `((setf gl-point-size ,point-size)))
     (setf gl-position ,position)
     (emit-data (values ,@data))
     (emit-vertex)))
