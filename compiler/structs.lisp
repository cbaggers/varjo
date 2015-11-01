(in-package :varjo)

;;------------------------------------------------------------
;; GLSL Structs
;;--------------

;; (vdefstruct thing ()
;;   (a v-float)
;;   (to-long-to-blah v-int :accessor b))

(defun true-type-name (name) (symb 'varjo::true_ name))
(defun fake-type-name (name) (symb 'varjo::fake_ name))

;;[TODO] should this use defun?
;;       pro: this is a global struct so global func
;;       con: shadowing.. add-function for global doesnt check.
(defmacro v-defstruct (name context &body slots)
  (destructuring-bind (name &key shadowing constructor) (listify name)
    (let ((name-string (safe-glsl-name-string name))
          (class-name (or shadowing name))
          (true-type-name (true-type-name name))
          (fake-type-name (fake-type-name name)))
      `(progn
         ,(when shadowing `(add-type-shadow ',name ',class-name))
         (def-v-type-class ,class-name (v-user-struct)
           ((glsl-string :initform ,name-string :initarg :glsl-string
                         :reader v-glsl-string)
            (signature :initform ,(format nil "struct ~a {~%~{~a~%~}};"
                                          name-string
                                          (mapcar #'gen-slot-string slots))
                       :initarg :signature :accessor v-signature)
            (slots :initform ',slots :reader v-slots)))
         (defmethod v-true-type ((object ,class-name))
           (make-instance ',true-type-name))
         (defmethod v-fake-type ((object ,class-name))
           (make-instance ',fake-type-name))
         (def-v-type-class ,true-type-name (,class-name) ())
         (def-v-type-class ,fake-type-name (,class-name) ((signature :initform "")))
	 (defmethod type->type-spec ((type ,true-type-name))
	   ',name)
         (v-defun ,(symb 'make- (or constructor name))
             ,(append (loop :for slot :in slots :collect (first slot))
                      (when context `(&context ,@context)))
           ,(format nil "~a(~{~a~^,~^ ~})" name-string
                    (loop :for slot :in slots :collect "~a"))
           ,(loop :for slot :in slots :collect (second slot))
           ,true-type-name :place nil)
         ,@(make-struct-accessors name true-type-name context slots)
         ',name))))

(defun make-struct-accessors (name true-type-name context slots)
  (loop :for (slot-name slot-type . acc) :in slots :collect
     (let ((accessor (if (eq :accessor (first acc)) (second acc)
                         (symb name '- slot-name))))
       `(v-defun ,accessor (,(symb name '-ob) ,@(when context `(&context ,@context)))
          ,(concatenate 'string "~a." (safe-glsl-name-string
                                       (or accessor slot-name)))
          (,true-type-name) ,slot-type :place t))))

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
  (let* ((struct (v-fake-type type))
         (fake-type (class-name (class-of struct)))
         (slots (v-slots type))
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
             (v-make-value struct env glsl-name)
             env t)
    env))

(defun add-uniform-fake-struct (uniform-name glsl-name type qualifiers env)
  (let* ((struct (v-fake-type type))
         (fake-type (class-name (class-of struct)))
         (slots (v-slots type))
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
             (v-make-value struct env glsl-name)
             env t)
    env))

(defun fake-slot-name (in-var-name slot-name)
  (format nil "fk_~a_~a" (string-downcase (string in-var-name))
          (string-downcase (safe-glsl-name-string slot-name))))
