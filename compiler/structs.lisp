(in-package :varjo)

;;------------------------------------------------------------
;; GLSL Structs
;;--------------

;; (vdefstruct thing ()
;;   (a v-float)
;;   (to-long-to-blah v-int :accessor b))

;;[TODO] should this use defun?
;;       pro: this is a global struct so global func
;;       con: shadowing.. add-function for global doesnt check.
(defmacro v-defstruct (name context &body slots)
  (destructuring-bind (name &key shadowing) (listify name)
    (let ((name-string (safe-glsl-name-string name))
          (class-name (or shadowing name))
          (true-type-name (symb 'varjo::true_ name))
          (fake-type-name (symb 'varjo::fake_ name)))
      `(progn
         ,(when shadowing `(add-type-shadow ',name ',class-name))
         (defclass ,class-name (v-user-struct)
           ((glsl-string :initform ,name-string :initarg :glsl-string
                         :reader v-glsl-string)
            (signature :initform ,(format nil "struct ~(~a~) {~%~{~a~%~}};"
                                          name-string
                                          (mapcar #'gen-slot-string slots))
                       :initarg :signature :accessor v-signature)
            (slots :initform ',slots
                   :reader v-slots)
            (true-type :initform ',true-type-name :initarg :true-type :reader v-true-type)
            (fake-type :initform ',fake-type-name :initarg :fake-type :reader v-fake-type)))
         (defclass ,true-type-name (,class-name) ())
         (defclass ,fake-type-name (,class-name) ((signature :initform "")))
         (v-defun ,(symb 'make- name)
             ,(append (loop :for slot :in slots :collect (first slot))
                      (when context `(&context ,@context)))
           ,(format nil "~a(~{~a~^,~^ ~})" name-string
                    (loop :for slot :in slots :collect "~a"))
           ,(loop :for slot :in slots :collect (second slot))
           ,true-type-name :place nil)
         ,@(loop :for (slot-name slot-type . acc) :in slots :collect
              (let ((accessor (if (eq :accessor (first acc)) (second acc)
                                  (symb name '- slot-name))))
                `(v-defun ,accessor (,(symb name '-ob) ,@(when context `(&context ,@context)))
                   ,(concatenate 'string "~a." (safe-glsl-name-string
                                                (or accessor slot-name)))
                   (,true-type-name) ,slot-type :place t)))
         ',name))))

()

(defun gen-slot-string (slot)
  (destructuring-bind (slot-name slot-type &key accessor) slot
    (let ((name (or accessor slot-name))
          (type-obj (type-spec->type slot-type)))
      (if (typep type-obj 'v-array)
          (format nil "    ~a ~a[~a];"
                  (v-glsl-string (type->type-spec (v-element-type type-obj)))
                  (safe-glsl-name-string name)
                  (v-dimensions type-obj))
          (format nil "    ~a ~a;"
                  (v-glsl-string type-obj)
                  (safe-glsl-name-string name))))))

(defun add-in-arg-fake-struct (in-var-name glsl-name type qualifiers env)
  (let* ((fake-type (v-fake-type type))
         (slots (v-slots type))
         (struct (make-instance fake-type))
         (new-in-args
          (loop :for (slot-name slot-type . acc) :in slots
             :for fake-slot-name = (fake-slot-name glsl-name slot-name)
             :for accessor = (if (eq :accessor (first acc))
                                 (second acc)
                                 (symb (type->type-spec type) '- slot-name))
             :do (add-function
                  accessor
                  (func-spec->function
                   (v-make-f-spec accessor
                                  fake-slot-name
                                  nil ;; {TODO} Must be context
                                  (list fake-type)
                                  slot-type :place nil) env) env t)
             :collect `(,fake-slot-name ,slot-type ,qualifiers))))
    (setf (v-in-args env) (append (v-in-args env) new-in-args))
    (add-var in-var-name
             (make-instance 'v-value :type struct :glsl-name glsl-name)
             env t)
    env))

(defun add-uniform-fake-struct (uniform-name glsl-name type qualifiers env)
  (let* ((fake-type (v-fake-type type))
         (slots (v-slots type))
         (struct (make-instance fake-type))
         (new-uniform-args
          (loop :for (slot-name slot-type . acc) :in slots
             :for fake-slot-name = (fake-slot-name uniform-name slot-name)
             :for accessor = (if (eq :accessor (first acc))
                                 (second acc)
                                 (symb (type->type-spec type) '- slot-name))
             :do (add-function
                  accessor
                  (func-spec->function
                   (v-make-f-spec accessor
                                  fake-slot-name
                                  nil ;; {TODO} Must be context
                                  (list fake-type)
                                  slot-type :place nil) env) env t)
             :collect `(,fake-slot-name ,slot-type ,qualifiers ,fake-slot-name))))
    (setf (v-uniforms env) (append (v-uniforms env) new-uniform-args))
    (add-var uniform-name
             (make-instance 'v-value :type struct :glsl-name glsl-name)
             env t)
    env))

(defun fake-slot-name (in-var-name slot-name)
  (format nil "fk_~a_~a" (string-downcase (string in-var-name))
          (string-downcase (safe-glsl-name-string slot-name))))
