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
  (let ((name-string (string-downcase (symbol-name name))))
    `(progn 
       (defclass ,name (v-user-struct) 
         ((glsl-string :initform ,name-string :initarg :glsl-string
                       :reader v-glsl-string)
          (signature :initform ,(format nil "struct ~(~a~) {~%~{~a~%~}};"
                                          name-string
                                          (mapcar #'gen-slot-string slots))
                       :initarg :signature :reader v-signature)
          (slots :initform ',slots :reader v-slots)))
       (v-defun ,(symb 'make- name) 
           ,(append (loop :for slot :in slots :collect (first slot))
                    (when context `(&context ,@context)))
         ,(format nil "~a(~{~a~^,~^ ~})" name-string
                  (loop :for slot :in slots :collect "~a"))
         ,(loop :for slot :in slots :collect (second slot))
         ,name :place nil)
       ,@(loop :for (slot-name slot-type . acc) :in slots :collect
            (let ((accessor (if (eq :accessor (first acc)) (second acc) 
                                (symb name '- slot-name))))
              `(v-defun ,accessor (,(symb name '-ob) ,@(when context `(&context ,@context)))
                 ,(concatenate 'string "~a." (string slot-name))
                 (,name) ,slot-type :place t)))
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
  (let* ((name (symb 'fake- (v-type-name type) '- in-var-name))
         (slots (v-slots type))         
         (struct (make-instance 'v-fake-struct
                                :signature nil
                                :slots slots
                                :fake-type-name name
                                :glsl-string (v-glsl-string type))))
    (loop :for (slot-name slot-type . acc) :in slots
       :for fake-slot-name = (fake-slot-name in-var-name slot-name)
       :for accessor = (if (eq :accessor (first acc))
                           (second acc) 
                           (symb (v-type-name type) '- slot-name))
       :do (add-function
            accessor
            (func-spec->function
             (v-make-f-spec fake-slot-name '(obj) (list name) 
                            (type-spec->type slot-type :env env) 
                            :place nil)) env t)
       :do (push `(,fake-slot-name ,slot-type ,qualifiers)
                 (v-in-args env)))
    (add-fake-type name struct env t)
    (add-var in-var-name (make-instance 'v-value :type struct 
                                        :glsl-name (string-downcase 
                                                    (string in-var-name)))
             env t)    
    env))

(defun copy-fake-struct-type (type)
  (make-instance 'v-fake-struct
                 :signature (copy-seq (v-signature type))
                 :slots (copy-seq (v-slots type))
                 :fake-type-name (v-fake-type-name type)
                 :glsl-string (copy-seq (v-glsl-string type))))

(defun fake-slot-name (in-var-name slot-name) 
  (format nil "fk_~a_~a" (string-downcase (string in-var-name))
          (string-downcase (string slot-name))))
