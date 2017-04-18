(in-package :varjo)
(in-readtable :fn.reader)

;;------------------------------------------------------------

(defclass v-function ()
  ((versions :initform nil :initarg :versions :accessor v-versions)
   (argument-spec :initform nil :initarg :arg-spec :accessor v-argument-spec)
   (glsl-string :initform "" :initarg :glsl-string :reader v-glsl-string)
   (glsl-name :initarg :glsl-name :accessor v-glsl-name)
   (return-spec :initform nil :initarg :return-spec :accessor v-return-spec)
   (v-place-index :initform nil :initarg :v-place-index :reader v-place-index)
   (name :initform nil :initarg :name :reader name)
   (implicit-args :initform nil :initarg :implicit-args :reader implicit-args)
   (in-out-args :initform nil :initarg :in-out-args :reader in-out-args)
   (in-arg-flow-ids :initform (error 'flow-ids-mandatory :for :v-function
                                     :code-type :v-function)
                    :initarg :in-arg-flow-ids :reader in-arg-flow-ids)
   (flow-ids :initform (error 'flow-ids-mandatory :for :v-function
                              :code-type :v-function)
             :initarg :flow-ids :reader flow-ids)
   (emit-set :initform nil :initarg :emit-set :reader emit-set)
   (pure :initform nil :initarg :pure :reader pure-p)))

(defmethod functions ((fn v-function))
  (list fn))

;;------------------------------------------------------------

(def-v-type-class v-user-function (v-function)
  ((captured-vars :initform nil :initarg :captured-vars :reader captured-vars)
   (code :initform nil :initarg :code :reader v-code)))

;;------------------------------------------------------------

(defmethod functions ((fn v-user-function))
  (list fn))

;;------------------------------------------------------------

(defclass v-function-set ()
  ((functions :initform nil :initarg :functions :reader functions)))

;; {TODO} Proper error
(defun make-function-set (functions)
  (when functions
    (assert (every λ(or (typep _ 'v-function) (typep _ 'external-function))
                   functions)
            (functions)
            "Failed to initialize v-function-set:~% functions: ~s" functions)
    (make-instance 'v-function-set :functions functions)))

(defmethod print-object ((fs v-function-set) stream)
  (if (null (functions fs))
      (format stream "#<EMPTY-FUNCTION-SET>")
      (call-next-method)))

;;------------------------------------------------------------

(defgeneric function-identifier (func)
  (:method ((func v-function))
    (cons (name func) (mapcar #'type->type-spec (v-argument-spec func)))))

(defgeneric function-identifier-with-return (func)
  (:method ((func v-function))
    (let* ((returns (mapcar #'type->type-spec (v-return-spec func)))
           (returns (if (= (length returns) 1)
                        (first returns)
                        returns)))
      `#'(,(name func) ,(mapcar #'type->type-spec (v-argument-spec func))
           :-> ,returns))))

;;------------------------------------------------------------

;; (defclass external-function ()
;;   ((name :initarg :name :reader name)
;;    (in-args :initarg :in-args :reader in-args)
;;    (uniforms :initarg :uniforms :reader uniforms)
;;    (code :initarg :code :reader code)
;;    (glsl-versions :initarg :glsl-versions :reader glsl-versions)))

(defmethod v-type-of ((func v-function))
  (with-slots (argument-spec return-spec) func
    (assert (listp return-spec))
    (make-instance 'v-function-type
                   :ctv func
                   :arg-spec argument-spec
                   :return-spec return-spec)))

(defmethod v-type-of ((func-set v-function-set))
  (gen-any-one-of-type (mapcar #'v-type-of (functions func-set))))

(defmethod v-place-function-p ((f v-function))
  (not (null (v-place-index f))))

(defmethod print-object ((object v-function) stream)
  (with-slots (name argument-spec return-spec) object
    (format stream "#<V-FUNCTION ~s ~s -> ~s>"
            name
            (if (eq t argument-spec)
                '(t*)
                (mapcar #'type-of argument-spec))
            (typecase (first return-spec)
              (function t)
              (v-type (type-of (first return-spec)))
              (otherwise return-spec)))))

;;------------------------------------------------------------

(defun make-function-obj (name transform versions arg-spec return-spec
                          &key v-place-index glsl-name implicit-args
                            in-out-args flow-ids in-arg-flow-ids pure)
  (make-instance 'v-function
                 :glsl-string transform
                 :arg-spec (if (listp arg-spec)
                               (loop :for spec :in arg-spec :collect
                                  (try-type-spec->type
                                   (resolve-name-from-alternative spec)
                                   nil))
                               arg-spec)
                 :return-spec
                 (mapcar (lambda (rspec)
                           (if (type-specp rspec)
                               (type-spec->type rspec)
                               rspec))
                         return-spec)
                 :versions versions :v-place-index v-place-index
                 :glsl-name glsl-name
                 :name name
                 :implicit-args implicit-args
                 :in-out-args in-out-args
                 :flow-ids flow-ids
                 :in-arg-flow-ids in-arg-flow-ids
                 :pure pure))

(defun make-user-function-obj (name transform versions arg-spec return-spec
                               &key v-place-index glsl-name implicit-args
                                 in-out-args flow-ids in-arg-flow-ids
                                 code captured-vars pure emit-set)
  (make-instance 'v-user-function
                 :glsl-string transform
                 :arg-spec (if (listp arg-spec)
                               (loop :for spec :in arg-spec :collect
                                  (type-spec->type spec))
                               arg-spec)
                 :return-spec
                 (mapcar (lambda (rspec)
                           (if (type-specp rspec)
                               (type-spec->type rspec)
                               rspec))
                         return-spec)
                 :versions versions :v-place-index v-place-index
                 :glsl-name glsl-name
                 :name name
                 :implicit-args implicit-args
                 :flow-ids flow-ids
                 :in-arg-flow-ids in-arg-flow-ids
                 :in-out-args in-out-args
                 :code code
                 :captured-vars captured-vars
                 :emit-set emit-set
                 :pure pure))

;; {TODO} make this use the arg & return types
(defun gen-dummy-func-glsl-name (func-type)
  (declare (ignore func-type))
  "<dummy-func>")

(defun make-dummy-function-from-type (func-type)
  (let ((arg-spec (v-argument-spec func-type))
        (return-spec (v-return-spec func-type))
        (glsl-name (gen-dummy-func-glsl-name func-type)))
    (make-instance
     'v-function
     :glsl-string (format nil "~a(~{~a~})" glsl-name
                          (loop :for nil :in arg-spec :collect "~a"))
     :arg-spec arg-spec
     :return-spec return-spec
     :versions *supported-versions*
     :v-place-index nil
     :glsl-name glsl-name
     :name 'dummy-func
     :implicit-args nil
     :flow-ids (flow-id!)
     :in-arg-flow-ids (loop :for nil :in arg-spec :collect (flow-id!))
     :in-out-args nil)))

;;------------------------------------------------------------

(defmethod shadow-function ((func v-function) shadowed-type new-type
                            &key (convert-args t) convert-returns
                              new-name)
  (with-slots (versions argument-spec glsl-string glsl-name return-spec
                        v-place-index name implicit-args in-out-args
                        in-arg-flow-ids flow-ids)
      func
    (when convert-args
      (assert (find shadowed-type argument-spec :test #'v-type-eq) ()
              'shadowing-no-type-match :shadowed shadowed-type :func func))
    (when convert-returns
      (assert (find shadowed-type return-spec :test #'v-type-eq) ()
              'shadowing-no-return-matched :shadowed shadowed-type :func func))
    (let* ((new-arg-spec (if convert-args
                             (substitute new-type shadowed-type argument-spec
                                         :test #'v-type-eq)
                             argument-spec))
           (new-return-spec (if convert-returns
                                (substitute new-type shadowed-type return-spec
                                            :test #'v-type-eq)
                                return-spec))
           (new-func (make-instance 'v-function
                                    :versions versions
                                    :arg-spec new-arg-spec
                                    :glsl-string glsl-string
                                    :glsl-name glsl-name
                                    :return-spec new-return-spec
                                    :v-place-index v-place-index
                                    :name (or new-name name)
                                    :implicit-args implicit-args
                                    :in-out-args in-out-args
                                    :in-arg-flow-ids in-arg-flow-ids
                                    :flow-ids flow-ids)))
      (add-form-binding new-func *global-env*)
      new-func)))

;; {TODO} proper error
(defmethod shadow-function ((func v-user-function) shadowed-type new-type
                            &key (convert-args t) convert-returns)
  (declare (ignore convert-args convert-returns shadowed-type new-type))
  (error 'shadowing-user-defined-func :func func))

(defun shadow-functions (shadow-type-spec function-identifiers)
  (let* ((type (type-spec->type shadow-type-spec))
         (shadowed (shadowed-type type))
         (func-sets (mapcar λ(find-form-binding-by-literal _ *global-env*)
                            function-identifiers))
         (pairs (mapcar λ(list _ (functions _1)) function-identifiers
                        func-sets))
         (multi-sets (remove-if-not λ(> (length (second _)) 1) pairs)))
    (assert (null multi-sets) () 'shadowing-multiple-funcs
            :shadow-type type :pairs (reduce #'append multi-sets))
    (let* ((functions (reduce #'append (mapcar #'functions func-sets)))
           (has-type (remove-if-not
                      λ(find shadowed (v-argument-spec _) :test #'v-type-eq)
                      functions))
           (no-type (set-difference functions has-type))
           (valid (remove-if λ(typep _ 'v-user-function) has-type))
           (user-funcs (set-difference valid has-type)))
      (when user-funcs
        (warn 'cant-shadow-user-defined-func :funcs user-funcs))
      (when no-type
        (warn 'cant-shadow-no-type-match :shadowed shadowed :funcs no-type))
      (loop :for func :in valid :collect
         (function-identifier-with-return
          (shadow-function func shadowed type :convert-returns t))))))

(defun shadow-constructor-function (shadow-type-spec function-identifier)
  (let* ((type (type-spec->type shadow-type-spec))
         (shadowed (shadowed-type type))
         (func-set (find-form-binding-by-literal function-identifier
                                                 *global-env*))
         (functions (functions func-set)))
    (assert (not (null functions)) () 'shadowing-constructor-no-match
            :shadow-type type :func-id function-identifier)
    (assert (= (length functions) 1) () 'shadowing-multiple-constructors
            :shadow-type type :func-id function-identifier :funcs functions)
    (let* ((function (first functions))
           (has-type (find shadowed (v-return-spec function)
                           :test #'v-type-eq))
           (user-func (typep function 'v-user-function)))
      (when user-func
        (error 'shadowing-user-defined-func :func function))
      (unless has-type
        (error 'shadowing-no-return-matched :shadowed shadowed :func function))
      (function-identifier-with-return
       (shadow-function function shadowed type
                        :new-name shadow-type-spec
                        :convert-args nil
                        :convert-returns t)))))

(defun add-alt-ephemeral-constructor-function (src-type-name alt-type-name)
  (let ((src-type (type-spec->type src-type-name))
        (alt-type (type-spec->type alt-type-name)))
    (when (ephemeral-p (type-spec->type src-type-name))
      (let* ((func-set (find-form-binding-by-literal (list src-type-name)
                                                     *global-env*))
             (functions (functions func-set))
             (constr (first (remove-if λ(> (length (v-argument-spec _)) 0)
                                       functions))))
        (when (v-type-eq (first (v-return-spec constr)) src-type)
          (function-identifier-with-return
           (shadow-function constr src-type alt-type
                            :new-name alt-type-name
                            :convert-args nil
                            :convert-returns t)))))))

;;------------------------------------------------------------

(defmethod captured-vars ((fn v-function))
  nil)

;;------------------------------------------------------------
