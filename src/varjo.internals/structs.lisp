(in-package :varjo.internals)

;;------------------------------------------------------------
;; GLSL Structs
;;--------------

;; (vdefstruct thing ()
;;   (a v-float)
;;   (to-long-to-blah v-int :accessor b))

;;[TODO] should this use defun?
;;       pro: this is a global struct so global func
;;       con: shadowing.. add-function for global doesnt check.
(defmacro define-vari-struct (name context &body slots)
  (destructuring-bind (name &key shadowing constructor) (listify name)
    (let* ((name-string (safe-glsl-name-string name))
           (class-name (or shadowing name))
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
                                        (safe-glsl-name-string slot-name))))
                          (list slot-name slot-type accessor transform))))
                    slots))
           (slot-transforms-type-obj
            (mapcar (lambda (x)
                      (dbind (name type acc tran) x
                        (list name (type-spec->type type) acc tran)))
                    slot-transforms))
           (constructor-name (symb 'make- (or constructor name)))
           (unsized-array-slots
            (loop
               :for (name type) :in slot-transforms-type-obj
               :for i :from 0
               :when (v-unsigned-array-p type)
               :collect (list i name type)))
           (has-unsized (not (null unsized-array-slots)))
           (ephemeral-slots
            (loop
               :for (name type) :in slot-transforms-type-obj
               :when (or (ephemeral-p type)
                         (and (typep type 'v-container)
                              (ephemeral-p (v-element-type type))))
               :collect (list name type)))
           (unsized-struct-slots
            (loop
               :for (name type) :in slot-transforms-type-obj
               :when (and (typep type 'v-struct)
                          (has-unsized-slot-p type))
               :collect (list name type))))
      (assert (and (<= (length unsized-array-slots) 1)
                   (or (null unsized-array-slots)
                       (= (caar unsized-array-slots)
                          (- (length slots) 1))))
              () 'illegal-use-of-unsize-in-struct
              :name name
              :form `(define-vari-struct ,name ,context ,@slots)
              :slots (mapcar #'second unsized-array-slots))
      (assert (null ephemeral-slots) ()
              'struct-cannot-hold-ephemeral-types
              :name name
              :slots (mapcar #'first ephemeral-slots))
      (assert (null unsized-struct-slots) ()
              'struct-with-unsized-slot-in-struct
              :slots (mapcar #'first unsized-struct-slots)
              :form `(define-vari-struct ,name ,context ,@slots))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (define-v-type-class ,class-name (v-user-struct)
             ((glsl-string :initform ,name-string :initarg :glsl-string
                           :reader v-glsl-string)
              (signature :initform ,(gen-struct-sig
                                     name-string slots-with-types)
                         :initarg :signature :accessor v-signature)
              (slots :initform ',slot-transforms-type-obj
                     :reader v-slots)
              (has-unsized-slot-p
               :initform ,has-unsized :initarg :has-unsized-slot-p
               :accessor has-unsized-slot-p))))
         ,(when shadowing `(add-alternate-type-name ',name ',class-name))
         (defmethod type->type-spec ((type ,class-name))
           ',name)
         ,(unless unsized-array-slots
            `(v-def-glsl-template-fun ,constructor-name
                                      ,(append (loop :for slot :in slots :collect (first slot))
                                               (when context `(&context ,@context)))
                                      ,(format nil "~a(~{~a~^,~^ ~})" name-string
                                               (n-of "~a" (length slots)))
                                      ,(loop :for slot :in slots :collect (second slot))
                                      ,name :v-place-index nil))
         ,@(make-struct-accessors name  context slot-transforms)
         ,(make-copy-structure name constructor-name slot-transforms)
         ',name))))

(defmacro v-defstruct (name context &body slots)
  `(define-vari-struct ,name ,context ,@slots))

(defmethod v-glsl-size ((type v-user-struct))
  (reduce #'+ (mapcar #'v-glsl-size (mapcar #'second (v-slots type)))))

(defun make-copy-structure (name constructor-name transforms)
  `(v-defun copy-structure ((x ,name))
     (,constructor-name
      ,@(loop :for (nil nil accessor) :in transforms :collect
           `(,accessor x)))))

(defun make-struct-accessors (name context transforms)
  (loop :for (nil slot-type accessor slot-transform) :in transforms :collect
     `(v-def-glsl-template-fun
       ,accessor
       (,(symb name '-ob) ,@(when context `(&context ,@context)))
       ,slot-transform
       (,name)
       ,slot-type
       :v-place-index 0)))

(defun gen-struct-sig (name-string slots-with-types)
  (format nil "struct ~a {~%~{~a~%~}};"
          name-string
          (mapcar #'gen-slot-string slots-with-types)))

(defun gen-slot-string (slot)
  (destructuring-bind (slot-name slot-type &key accessor) slot
    (declare (ignore accessor))
    (let ((name slot-name))
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
  (assert (eq var-type (v-type-of input-variable)))
  (let* ((glsl-name (glsl-name input-variable))
         (var-type (set-flow-id var-type (flow-id!)))
         (type-for-value (make-into-block-struct (strip-qualifiers var-type)
                                                 glsl-name)))
    (assert (flow-ids var-type) ()
            "We fucked up when removing fake-struct, add the flow ids ~a"
            var-type)
    (loop :for (slot-name slot-type accessor) :in (v-slots var-type)
       :for fake-slot-name = (fake-slot-name glsl-name slot-name)

       :collect (make-function-obj accessor
                                   fake-slot-name
                                   nil ;; {TODO} Must be context
                                   (list var-type)
                                   (make-type-set slot-type)
                                   :v-place-index nil
                                   :pure t)
       :into funcs

       :collect (make-instance
                 'input-variable
                 :name fake-slot-name
                 :glsl-name fake-slot-name
                 :type (qualify-type slot-type
                                     (qualifiers
                                      (v-type-of input-variable))))
       :into vars

       :finally (return
                  (values (v-make-value type-for-value env
                                        :glsl-name glsl-name
                                        :read-only t)
                          vars
                          funcs)))))

(defun fake-slot-name (in-var-name slot-name)
  (format nil "fk_~a_~a" (string-downcase (string in-var-name))
          (string-downcase (safe-glsl-name-string slot-name))))
