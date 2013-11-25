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
  `(progn 
     (defclass ,name (v-user-struct) 
       ((glsl-string :initform ,(format nil "struct ~(~a~) {~%~{~a~%~}};"
                                        name (mapcar #'gen-slot-string slots))
                     :initarg :glsl-string :reader v-glsl-string)
        (slots :initform ',slots :reader v-slots)))
     (v-defun ,(symb 'make- name) 
         ,(append (loop :for slot :in slots :collect (first slot))
                  (when context `(&context ,@context)))
       ,(format nil "~a(~{~a~^,~^ ~})" name 
                (loop :for slot :in slots :collect "~a"))
       ,(loop :for slot :in slots :collect (second slot))
       ,name :place nil)
     ,@(loop :for (slot-name slot-type . acc) :in slots :collect
          (let ((accessor (if (eq :accessor (first acc)) (second acc) slot-name)))
            `(v-defun ,accessor (,(symb name '-ob) ,@(when context `(&context ,@context)))
               ,(concatenate 'string "~a." (string slot-name))
               (,name) ,slot-type :place t)))
     ',name))

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

;;[TODO] I think there will be a problem if you let a in-arg
;;       it will end up with the wrong name in the resulting glsl code
(defmethod make-fake-struct ((type v-user-struct) (env environment))
  (let* ((name (gensym (format nil "fake-~s" (v-type-name type))))
         (slots (v-slots type))
         (fake-type (make-instance 'v-fake-struct :slots slots
                                   :fake-type-name name 
                                   :glsl-string (v-glsl-string type))))
    (loop :for (slot-name slot-type . acc) :in slots :collect
       (let ((accessor (if (eq :accessor (first acc)) (second acc) slot-name)))
         (add-function 
          accessor
          (func-spec->function (v-make-f-spec
                                (concatenate 'string "~a_" 
                                             (fake-slot-name slot-name))
                                '(obj) (list name) (type-spec->type slot-type)
                                :place nil))
          env t)))
    fake-type))

(defun fake-slot-name (slot-name) (string slot-name))
