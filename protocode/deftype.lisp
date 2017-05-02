(in-package :varjo)
(in-readtable :fn.reader)

(defmacro v-deftype (name args type-form)
  (let ((ephemeral (null type-form)))
    (assert (not args) () "args not supported yet")
    (assert (symbolp type-form) () "compound & array types not yet supported")
    (if ephemeral
        `(def-v-type-class ,name (v-ephemeral-type) ())
        (let ((shadowed-type (type-spec->type type-form)))
          `(def-v-type-class ,name (v-shadow-type)
             ((shadowed-type :initform ,shadowed-type)
              (glsl-string :initform ,(v-glsl-string shadowed-type))))))))

(defmacro def-shadow-type-functions (shadow-type &body function-identifiers)
  (flet ((func-form-p (x)
           (and (listp x) (eq (first x) 'function) (= (length x) 2))))
    (assert (every #'func-form-p function-identifiers) ()
            'def-shadow-non-func-identifier
            :name 'def-shadow-non-func-identifier
            :func-ids (remove-if #'func-form-p function-identifiers))
    (let ((function-identifiers (mapcar #'second function-identifiers)))
      `(shadow-functions ',shadow-type ',function-identifiers))))

(defmacro def-shadow-type-constructor (shadow-type function-identifier)
  (flet ((func-form-p (x)
           (and (listp x) (eq (first x) 'function) (= (length x) 2))))
    (assert (func-form-p function-identifier) ()
            'def-shadow-non-func-identifier
            :name 'def-shadow-non-func-identifier
            :func-ids (list function-identifier))
    (let ((function-identifier (second function-identifier)))
      `(shadow-constructor-function ',shadow-type ',function-identifier))))


#+nil
(v-deftype foo () ())
#+nil
(v-deftype foob () :vec4)
#+nil
(v-deftype flart () :float)

#+nil
(def-shadow-type-functions flart
  #'(sin :float))

#+nil
(def-shadow-type-constructor flart #'(float :int))

(v-defspecial init-ephemeral (name)
  :args-valid t
  :return
  (let* ((type (type-spec->type name (flow-id!)))
         (ast (ast-node! 'init-ephemeral (list name)
                         (make-type-set type) env env)))
    (assert (ephemeral-p type) () "~a is not an ephemeral type" name)
    (values (make-compiled :type-set (make-type-set type) :current-line nil
                           :node-tree ast)
            env)))
