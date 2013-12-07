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
  (let ((name-string (string-downcase (symbol-name name)))
        (type-name (symb 'varjo::true_ name))
        (fake-type-name (symb 'varjo::fake_ name)))
    `(progn 
       (defclass ,name (v-user-struct) 
         ((glsl-string :initform ,name-string :initarg :glsl-string
                       :reader v-glsl-string)
          (signature :initform ,(format nil "struct ~(~a~) {~%~{~a~%~}};"
                                          name-string
                                          (mapcar #'gen-slot-string slots))
                       :initarg :signature :reader v-signature)
          (slots :initform ',slots :reader v-slots)
          (true-type :initform ',type-name :initarg :true-type :reader v-true-type)
          (fake-type :initform ',fake-type-name :initarg :fake-type :reader v-fake-type)))
       (defclass ,type-name (,name) ())
       (defclass ,fake-type-name (,name) ((signature :initform "")))
       (v-defun ,(symb 'make- name) 
           ,(append (loop :for slot :in slots :collect (first slot))
                    (when context `(&context ,@context)))
         ,(format nil "~a(~{~a~^,~^ ~})" name-string
                  (loop :for slot :in slots :collect "~a"))
         ,(loop :for slot :in slots :collect (second slot))
         ,type-name :place nil)
       ,@(loop :for (slot-name slot-type . acc) :in slots :collect
            (let ((accessor (if (eq :accessor (first acc)) (second acc) 
                                (symb name '- slot-name))))
              `(v-defun ,accessor (,(symb name '-ob) ,@(when context `(&context ,@context)))
                 ,(concatenate 'string "~a." (string slot-name))
                 (,type-name) ,slot-type :place t)))
       ',name)))

(defun gen-slot-string (slot)
  (destructuring-bind (slot-name slot-type &key accessor) slot
    (let ((name (or accessor slot-name))
          (type-obj (type-spec->type slot-type)))
      (if (typep type-obj 'v-array)
          (format nil "    ~a ~a[~a];" 
                  (v-glsl-string (v-element-type type-obj)) 
                  name 
                  (v-dimensions type-obj))
          (format nil "    ~a ~a;" 
                  (v-glsl-string type-obj) name)))))

(defgeneric add-fake-struct (in-var-name type qualifiers env))
(defmethod add-fake-struct (in-var-name (type v-user-struct) qualifiers
                            (env environment))
  (let* ((fake-type (v-fake-type type))
         (slots (v-slots type))
         (struct (make-instance fake-type)))
    (loop :for (slot-name slot-type . acc) :in slots
       :for fake-slot-name = (fake-slot-name in-var-name slot-name)
       :for accessor = (if (eq :accessor (first acc))
                           (second acc) 
                           (symb (type->type-spec type) '- slot-name))
       :do (add-function
            accessor
            (func-spec->function
             (v-make-f-spec fake-slot-name '(obj) (list fake-type)
                            slot-type :place nil) env) env t)
       :do (push `(,fake-slot-name ,slot-type ,qualifiers)
                 (v-in-args env)))
    (add-var in-var-name (make-instance 'v-value :type struct 
                                        :glsl-name (string-downcase 
                                                    (string in-var-name)))
             env t)    
    env))

(defun fake-slot-name (in-var-name slot-name) 
  (format nil "fk_~a_~a" (string-downcase (string in-var-name))
          (string-downcase (string slot-name))))
