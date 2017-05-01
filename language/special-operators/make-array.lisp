(in-package :varjo)
(in-readtable :fn.reader)

(v-defspecial make-array (&rest args)
  :args-valid t
  :return
  (dbind (dimensions &key element-type initial-element initial-contents) args
    (flet ((quoted-p (x)
             (or (and (listp x) (eq (first x) 'quote))
                 (keywordp x)
                 (null x))))
      ;; Sanity check
      (assert (and dimensions element-type) (args) 'make-array-mandatory-args
              :args args)
      (assert (numberp dimensions) (dimensions) 'multi-dimensional-array
              :dimensions dimensions)
      (assert (quoted-p element-type) (element-type) 'should-be-quoted
              :thing "element-type" :val element-type)
      (assert (quoted-p initial-contents) (initial-contents) 'should-be-quoted
              :thing "initial-contents" :val element-type)
      (assert (constantp initial-element) (initial-element) 'should-be-constant
              :thing "initial-element" :val initial-element)
      (assert (not (and initial-element initial-contents))
              (initial-element initial-contents) 'make-array-conflicting-args
              :args args)
      ;;
      (let ((initial-contents (second initial-contents)))
        (when initial-contents
          (assert (= (length initial-contents) dimensions)
                  (dimensions initial-contents) 'make-array-conflicting-lengths
                  :dims dimensions :initial-contents initial-contents))
        (let* ((element-type (type-spec->type (if (keywordp element-type)
                                                  element-type
                                                  (second element-type))))
               (len dimensions)
               (initial-contents
                (or initial-contents
                    (when initial-element
                      (n-of initial-element len))
                    (when (slot-boundp element-type 'default-value)
                      (n-of (slot-value element-type 'default-value) len))
                    (error 'make-array-cant-establish-default-value
                           :initial-contents initial-contents
                           :element-type (type->type-spec element-type))))
               (elem-objs (mapcar λ(compile-literal _ env) initial-contents))
               (types (mapcar #'primary-type elem-objs))
               (array-type (v-array-type-of element-type len (flow-id!))))
          (assert (every λ(v-casts-to-p _ element-type env) types) ()
                  'make-array-cant-cast-args
                  :element-type element-type
                  :initial-contents initial-contents)
          (let* ((glsl (gen-array-literal-string elem-objs element-type env))
                 (type-set (make-type-set array-type))
                 (ast (ast-node! :code-section (cons 'make-array args)
                                 type-set env env)))
            (values
             (make-compiled :type-set type-set
                            :current-line glsl
                            :used-types (list element-type)
                            :node-tree ast
                            :pure t)
             env)))))))

(v-defspecial vector (&rest elements)
  :args-valid t
  :return
  (vbind (objs) (mapcar λ(try-compile-arg _ env nil) elements)
    (let* ((len (length elements))
           (types (mapcar #'primary-type objs))
           (element-type (apply #'find-mutual-cast-type types))
           (array-type (v-array-type-of element-type len (flow-id!)))
           (glsl (gen-array-literal-string objs element-type env))
           (type-set (make-type-set array-type))
           (ast (ast-node! 'vector (mapcar #'node-tree objs)
                           (make-type-set array-type)
                           env env)))
      (values
       (merge-compiled objs
                       :type-set type-set
                       :current-line glsl
                       :node-tree ast)
       env))))
