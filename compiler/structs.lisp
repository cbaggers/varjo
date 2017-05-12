(in-package :varjo)

;;------------------------------------------------------------
;; GLSL Structs
;;--------------

;; (vdefstruct thing ()
;;   (a v-float)
;;   (to-long-to-blah v-int :accessor b))

(defun true-type-name (name) (symb 'true_ name))
(defun fake-type-name (name) (symb 'fake_ name))

;;[TODO] should this use defun?
;;       pro: this is a global struct so global func
;;       con: shadowing.. add-function for global doesnt check.
(defmacro v-defstruct (name context &body slots)
  (destructuring-bind (name &key shadowing constructor) (listify name)
    (let* ((name-string (safe-glsl-name-string name))
           (class-name (or shadowing name))
           (true-type-name (true-type-name name))
           (fake-type-name (fake-type-name name))
           (slots-with-types
            (mapcar (lambda (slot)
                      (dbind (name type . rest) slot
                        `(,name
                          ,(type-spec->type type)
                          ,@rest)))
                    slots))
           (slot-transforms
            (mapcar (lambda (x)
                      (dbind (slot-name slot-type . acc) x
                        (let* ((accessor (if (eq :accessor (first acc))
                                             (second acc)
                                             (symb name '- slot-name)))
                               (transform
                                (format nil "~~a.~a"
                                        (safe-glsl-name-string
                                         (or accessor slot-name)))))
                          (list slot-name slot-type accessor transform))))
                    slots))
           (slot-transforms-type-obj
            (mapcar (lambda (x)
                      (dbind (name type acc tran) x
                        (list name (type-spec->type type) acc tran)))
                    slot-transforms)))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (def-v-type-class ,class-name (v-user-struct)
             ((glsl-string :initform ,name-string :initarg :glsl-string
                           :reader v-glsl-string)
              (signature :initform ,(gen-struct-sig
                                     name-string slots-with-types)
                         :initarg :signature :accessor v-signature)
              (slots :initform ',slot-transforms-type-obj
                     :reader v-slots)))
           (def-v-type-class ,true-type-name (,class-name) ())
           (def-v-type-class ,fake-type-name (,class-name)
             ((signature :initform ""))))
         ,(when shadowing `(add-alternate-type-name ',name ',class-name))
         (defmethod v-true-type ((object ,class-name))
           (make-instance ',true-type-name :flow-ids (flow-ids object)))
         (defmethod v-fake-type ((object ,class-name))
           (make-instance ',fake-type-name :flow-ids (flow-ids object)))
         (defmethod type->type-spec ((type ,true-type-name))
           ',name)
         (v-def-glsl-template-fun ,(symb 'make- (or constructor name))
                                  ,(append (loop :for slot :in slots :collect (first slot))
                                           (when context `(&context ,@context)))
                                  ,(format nil "~a(~{~a~^,~^ ~})" name-string
                                           (n-of "~a" (length slots)))
                                  ,(loop :for slot :in slots :collect (second slot))
                                  ,true-type-name :v-place-index nil)
         ,@(make-struct-accessors name true-type-name context slot-transforms)
         ',name))))

(defun make-struct-accessors (name true-type-name context transforms)
  (loop :for (nil slot-type accessor slot-transform) :in transforms :collect
     `(v-def-glsl-template-fun
       ,accessor
       (,(symb name '-ob) ,@(when context `(&context ,@context)))
       ,slot-transform
       (,true-type-name)
       ,slot-type
       :v-place-index 0)))

(defun gen-struct-sig (name-string slots-with-types)
  (format nil "struct ~a {~%~{~a~%~}};"
          name-string
          (mapcar #'gen-slot-string slots-with-types)))

(defun gen-slot-string (slot)
  (destructuring-bind (slot-name slot-type &key accessor) slot
    (let ((name (or accessor slot-name)))
      (if (typep slot-type 'v-array)
          (format nil "    ~a ~a[~a];"
                  (v-glsl-string (v-element-type slot-type))
                  (safe-glsl-name-string name)
                  (v-dimensions slot-type))
          (format nil "    ~a ~a;"
                  (v-glsl-string slot-type)
                  (safe-glsl-name-string name))))))


(defmethod expand-input-variable ((stage stage)
                                  (var-type v-struct)
                                  (input-variable input-variable)
                                  (env environment))
  (declare (ignore stage))
  (let* ((glsl-name (glsl-name input-variable))
         (fake-struct (set-flow-id (v-fake-type var-type) (flow-id!))))
    ;;
    (loop :for (slot-name slot-type accessor) :in (v-slots var-type)
       :for fake-slot-name = (fake-slot-name glsl-name slot-name)

       :collect (make-function-obj accessor
                                   fake-slot-name
                                   nil ;; {TODO} Must be context
                                   (list fake-struct)
                                   (make-type-set slot-type)
                                   :v-place-index nil
                                   :pure t)
       :into funcs

       :collect (make-instance
                 'input-variable
                 :name fake-slot-name
                 :glsl-name fake-slot-name
                 :type slot-type
                 :qualifiers (qualifiers input-variable))
       :into vars

       :finally (return
                  (values (v-make-value fake-struct env
                                        :glsl-name glsl-name
                                        :read-only t)
                          vars
                          funcs)))))

;; mutates env
(defmethod add-fake-struct ((var uniform-variable) env)
  (let* (;;
         (uniform-name (name var))
         (type (v-type-of var))
         (qualifiers (qualifiers var))
         (glsl-name (glsl-name var))
         ;;
         (struct (set-flow-id (v-fake-type type) (flow-id!)))
         (fake-type (class-name (class-of struct)))
         (slots (v-slots type))
         (fake-type-obj (try-type-spec->type
                         (resolve-name-from-alternative fake-type)
                         nil))
         (new-uniform-args
          (loop :for (slot-name slot-type accessor) :in slots
             :for fake-slot-name = (fake-slot-name uniform-name slot-name)
             :do (%add-function
                  accessor
                  (make-function-obj accessor
                                     fake-slot-name
                                     nil ;; {TODO} Must be context
                                     (list fake-type-obj)
                                     (make-type-set
                                      (type-spec->type slot-type))
                                     :v-place-index nil
                                     :pure t)
                  env)
             :collect (make-instance
                       'uniform-variable
                       :name fake-slot-name
                       :glsl-name fake-slot-name
                       :type slot-type
                       :qualifiers qualifiers))))
    (setf (v-uniforms env) (append (v-uniforms env) new-uniform-args))
    (%add-symbol-binding
     uniform-name
     (v-make-value struct env :glsl-name glsl-name :read-only t)
     env)
    env))

(defun fake-slot-name (in-var-name slot-name)
  (format nil "fk_~a_~a" (string-downcase (string in-var-name))
          (string-downcase (safe-glsl-name-string slot-name))))
