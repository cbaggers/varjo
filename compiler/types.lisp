;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
(in-package :varjo)

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defvar *registered-types* nil)

(defmacro def-v-type-class (name direct-superclass direct-slots &rest options)
  (let ((new-names (if (equal (package-name (symbol-package name)) "VARJO")
                       `(append (list ,(kwd (subseq (symbol-name name) 2))
                                      ',name)
                                *registered-types*)
                       `(cons ',name *registered-types*))))
    `(progn (defclass ,name ,direct-superclass ,direct-slots ,@options)
            (setf *registered-types* (remove-duplicates ,new-names))
            ',name)))

(def-v-type-class v-spec-type ()
  ((place :initform t :initarg :place :reader v-placep)))
(def-v-type-class v-t-type () ())
(def-v-type-class v-type (v-t-type)
  ((core :initform nil :reader core-typep)
   (place :initform t :initarg :place :accessor v-placep)
   (glsl-string :initform "<invalid>" :reader v-glsl-string)
   (glsl-size :initform 1)
   (casts-to :initform nil)))

(def-v-type-class v-none (v-t-type) ())

(def-v-type-class v-container (v-type)
  ((element-type :initform nil)
   (dimensions :initform nil :accessor v-dimensions)))

(def-v-type-class v-array (v-container)
  ((element-type :initform nil :initarg :element-type)
   (dimensions :initform nil :initarg :dimensions :accessor v-dimensions)))
(defmethod v-glsl-string ((object v-array))
  (format nil "~a ~~a~{[~a]~}" (v-glsl-string (v-element-type object)) (v-dimensions object)))

(def-v-type-class v-error (v-type)
  ((payload :initform nil :initarg :payload :accessor v-payload)))

(def-v-type-class v-struct (v-type)
  ((restriction :initform nil :initarg :restriction :accessor v-restriction)
   (signature :initform nil :initarg :signature :accessor v-signature)
   (glsl-string :initform "" :initarg :glsl-string :reader v-glsl-string)
   (slots :initform nil :initarg :slots :reader v-slots)))

(def-v-type-class v-user-struct (v-struct) ())

(def-v-type-class v-function (v-type)
  ((restriction :initform nil :initarg :restriction :accessor v-restriction)
   (argument-spec :initform nil :initarg :arg-spec :accessor v-argument-spec)
   (glsl-string :initform "" :initarg :glsl-string :reader v-glsl-string)
   (glsl-name :initarg :glsl-name :accessor v-glsl-name)
   (return-spec :initform nil :initarg :return-spec :accessor v-return-spec)
   (place :initform nil :initarg :place :accessor v-placep)
   (glsl-spec-matching :initform nil :initarg :glsl-spec-matching
                       :reader v-glsl-spec-matchingp)
   (multi-return-vars :initform nil :initarg :multi-return-vars
                      :reader multi-return-vars)
   (name :initform nil :initarg :name :reader name)
   (implicit-args :initform nil :initarg :implicit-args :reader implicit-args)))

(defmethod set-place-t ((type v-type))
  (setf (v-placep type) t) type)

(defmethod core-typep ((type v-t-type))
  nil)

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun v-spec-typep (obj)
  (and (typep obj 'v-spec-type)
       (not (typep obj 'v-type))))

(defgeneric v-element-type (object))
(defmethod v-element-type ((object v-t-type))
  (when (slot-exists-p object 'element-type)
    (when (slot-value object 'element-type)
      (type-spec->type (slot-value object 'element-type)))))

(defmethod type->type-spec ((type v-t-type))
  (class-name (class-of type)))
(defmethod type->type-spec ((type v-spec-type))
  (class-name (class-of type)))
(defmethod type->type-spec ((type v-array))
  (if (and (v-element-type type) (v-dimensions type))
      (list (type->type-spec (v-element-type type)) (v-dimensions type))
      'v-array))

(defun try-type-spec->type (spec &key place (env *global-env*))
  (declare (ignore env))
  (let ((spec (cond ((keywordp spec) (p-symb 'varjo 'v- spec))
                    ((and (listp spec) (keywordp (first spec)))
                     (cons (p-symb 'varjo 'v- (first spec)) (rest spec)))
                    (t spec))))
    (cond ((null spec) nil)
          ((and (symbolp spec) (vtype-existsp spec))
           (let ((type (make-instance spec)))
	     (when (or (typep type 'v-t-type)
		       (typep type 'v-spec-type))
	       (when (slot-exists-p type 'place)
		 (setf (slot-value type 'place) place))
	       type)))
          ((and (listp spec) (vtype-existsp (first spec)))
           (destructuring-bind (type dimensions) spec
             (make-instance 'v-array :element-type (if (keywordp spec)
                                                       (symb 'v- type)
                                                       type)
                            :place place
                            :dimensions dimensions)))
          (t nil))))

(defun type-specp (spec &optional (env *global-env*))
  (handler-case (and (type-spec->type spec :env env) t)
    (unknown-type-spec (e)
      (declare (ignore e))
      nil)))

(let* ((shadow-ht (or (when (boundp '*type-shadow*) (symbol-value '*type-shadow*))
		      (make-hash-table)))
       (shadow-ht-backward
	(let ((ht (make-hash-table)))
	  (maphash #'(lambda (k v) (setf (gethash v ht) k))
		   shadow-ht)
	  ht)))
  (defun add-type-shadow (type shadowing-this-type)
    (assert (and (symbolp type) (symbolp shadowing-this-type)))
    (setf (gethash type shadow-ht) shadowing-this-type)
    (setf (gethash shadowing-this-type shadow-ht-backward) type))
  (defun un-shadow (spec)
    (if (listp spec)
	`(,(or (gethash (first spec) shadow-ht) (first spec)) ,@(rest spec))
	(or (gethash spec shadow-ht) spec)))
  (defun reverse-shadow-lookup (shadowed-type-spec)
    (if (listp shadowed-type-spec)
	`(,(or (gethash (first shadowed-type-spec) shadow-ht-backward)
	       (first shadowed-type-spec))
	   ,@(rest shadowed-type-spec))
	(or (gethash shadowed-type-spec shadow-ht-backward)
	    shadowed-type-spec))))

(defun type-spec->type (spec &key place (env *global-env*))
  (v-true-type
   (or (try-type-spec->type spec :place place :env env)
       (try-type-spec->type (un-shadow spec)
			    :place place :env env)
       (error 'unknown-type-spec :type-spec spec))))

(defmethod v-true-type ((object v-t-type))
  object)

(defmethod v-true-type ((object v-spec-type))
  object)

(defmethod v-glsl-size ((type t))
  (slot-value type 'glsl-size))

(defmethod v-glsl-size ((type v-array))
  (* (apply #'* (v-dimensions type))
     (slot-value (v-element-type type) 'glsl-size)))

(defmethod v-type-eq ((a v-type) (b v-type) &optional (env *global-env*))
  (declare (ignore env))
  (equal (type->type-spec a) (type->type-spec b)))
(defmethod v-type-eq ((a v-type) (b symbol) &optional (env *global-env*))
  (declare (ignore env))
  (equal (type->type-spec a) (type->type-spec (type-spec->type b))))
(defmethod v-type-eq ((a v-type) (b list) &optional (env *global-env*))
  (equal (type->type-spec a) (type->type-spec (type-spec->type b :env env))))

(defmethod v-typep ((a v-type) (b v-spec-type) &optional (env *global-env*))
  (declare (ignore env))
  (typep a (type-of b)))
(defmethod v-typep ((a v-type) (b v-type) &optional (env *global-env*))
  (v-typep a (type->type-spec b) env))
(defmethod v-typep ((a v-type) b &optional (env *global-env*))
  (declare (ignore env))
  (cond ((symbolp b)
	 (typep a (un-shadow b)))
        ((and (listp b) (numberp (second b)))
	 (typep a (un-shadow (first b))))))

(defmethod v-casts-to-p (from-type to-type env)
  (not (null (v-casts-to from-type to-type env))))

;;[TODO] vtypep here?
(defmethod v-casts-to ((from-type v-type) (to-type v-type) env)
  (if (v-typep from-type to-type)
      from-type
      (when (slot-exists-p from-type 'casts-to)
        (loop :for cast-type :in (slot-value from-type 'casts-to)
           :if (v-typep (type-spec->type cast-type) to-type env)
           :return cast-type))))

(defmethod v-casts-to ((from-type v-type) (to-type v-spec-type) env)
  (when (slot-exists-p from-type 'casts-to)
    (if (v-typep from-type to-type)
        from-type
        (loop :for cast-type :in (slot-value from-type 'casts-to)
           :if (v-typep (type-spec->type cast-type) to-type env)
           :return cast-type))))

(defun find-mutual-cast-type (&rest types)
  (let ((names (loop :for type :in types
                  :collect (if (typep type 'v-t-type)
                               (type->type-spec type)
                               type))))
    (if (loop :for name :in names :always (eq name (first names)))
        (first names)
        (let* ((all-casts (sort (loop :for type :in types :for name :in names :collect
                                   (cons name
                                         (if (symbolp type)
                                             (slot-value (type-spec->type type)
                                                         'casts-to)
                                             (slot-value type 'casts-to))))
                                #'> :key #'length))
               (master (first all-casts))
               (rest-casts (rest all-casts)))
          (first (sort (loop :for type :in master
                          :if (loop :for casts :in rest-casts
                                 :always (find type casts))
                          :collect type) #'> :key #'v-superior-score))))))

(let ((order-or-superiority '(v-double v-float v-int v-uint v-vec2 v-ivec2
                              v-uvec2 v-vec3 v-ivec3 v-uvec3 v-vec4 v-ivec4
                              v-uvec4 v-mat2 v-mat2x2 v-mat3 v-mat3x3 v-mat4
                              v-mat4x4)))
  (defun v-superior-score (type)
    (or (position type order-or-superiority) -1))
  (defun v-superior (x y)
    (< (or (position x order-or-superiority) -1)
       (or (position y order-or-superiority) -1))))

(defun v-superior-type (&rest types)
  (first (sort types #'v-superior)))

(defun v-errorp (obj) (typep obj 'v-error))

(defmethod post-initialise ((object v-t-type)))
(defmethod post-initialise ((object v-container))
  (setf (v-dimensions object) (listify (v-dimensions object))))

(defmethod initialize-instance :after ((type-obj v-t-type) &rest initargs)
  (declare (ignore initargs))
  (post-initialise type-obj))

(defun find-alternative-types-for-spec (type-spec)
  (when (symbolp type-spec)
    (let ((sn (symbol-name type-spec)))
      (append
       (remove-if-not
        (lambda (x)
          (let ((x (symbol-name x)))
            (or (string= sn x)
                (> (vas-string-metrics:jaro-winkler-distance sn x) 0.9))))
        *registered-types*)))))
