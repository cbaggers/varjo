(in-package :vari.cl)
(in-readtable :fn.reader)

(v-defspecial make-array (&rest args)
  :args-valid t
  :return
  (dbind (dimensions &key element-type initial-element initial-contents) args
    (labels ((quoted-p (x)
               (and (listp x) (eq (first x) 'quote)))
             (const-p (x)
               (or (quoted-p x)
                   (keywordp x)
                   (null x))))
      ;; Sanity check
      (assert (and dimensions element-type) (args) 'make-array-mandatory-args
              :args args)
      (assert (numberp dimensions) (dimensions) 'multi-dimensional-array
              :dimensions dimensions)
      (when (quoted-p element-type)
        (warn 'v-deprecated :old "quoted type-spec" :new element-type))
      (assert (const-p initial-contents) (initial-contents) 'should-be-quoted
              :thing "initial-contents" :val initial-contents)
      (assert (constantp initial-element) (initial-element) 'should-be-constant
              :thing "initial-element" :val initial-element)
      (assert (not (and initial-element initial-contents))
              (initial-element initial-contents) 'make-array-conflicting-args
              :args args)
      ;;
      (let ((element-type (if (quoted-p element-type)
                              (second element-type)
                              element-type))
            (initial-contents (second initial-contents)))
        (when initial-contents
          (assert (= (length initial-contents) dimensions)
                  (dimensions initial-contents) 'make-array-conflicting-lengths
                  :dims dimensions :initial-contents initial-contents))
        (let ((element-type (type-spec->type element-type)))
          (assert (not (ephemeral-p element-type)) ()
                  'arrays-cannot-hold-ephemeral-types
                  :form (cons 'make-array args))
          (let* ((len dimensions)
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
            (assert (every λ(v-casts-to-p _ element-type) types) ()
                    'make-array-cant-cast-args
                    :element-type element-type
                    :initial-contents initial-contents)
            (let* ((cast-objs (cast-for-array-literal element-type elem-objs))
                   (glsl (gen-array-literal-string cast-objs element-type))
                   (type-set (make-type-set array-type)))
              (values
               (make-compiled :type-set type-set
                              :current-line glsl
                              :used-types (list array-type element-type)
                              :pure t)
               env))))))))

(v-defspecial vector (&rest elements)
  :args-valid t
  :return
  (vbind (objs) (mapcar λ(try-compile-arg _ env nil) elements)
    (let* ((len (length elements))
           (types (mapcar #'primary-type objs))
           (element-type (apply #'find-mutual-cast-type types))
           (array-type (v-array-type-of element-type len (flow-id!)))
           (cast-objs (cast-for-array-literal element-type objs))
           (glsl (gen-array-literal-string cast-objs element-type))
           (type-set (make-type-set array-type)))
      (values
       (merge-compiled objs
                       :type-set type-set
                       :current-line glsl)
       env))))
