;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)

(defconstant *implicit-type-casts* 
  '(((:float nil nil) (:int nil nil) (:uint nil nil))
    ((:vec2 nil nil) (:ivec2 nil nil) (:uvec2 nil nil))
    ((:vec3 nil nil) (:ivec3 nil nil) (:uvec3 nil nil))
    ((:vec4 nil nil) (:ivec4 nil nil) (:uvec4 nil nil))))

(defparameter *glsl-types* '((:void nil nil) (:bool nil nil) 
			     (:int nil nil) (:uint nil nil)
			     (:float nil nil) (:bvec2 nil nil) 
			     (:bvec3 nil nil) (:bvec4 nil nil)
			     (:uvec2 nil nil) (:uvec3 nil nil) 
			     (:uvec4 nil nil) (:ivec2 nil nil)
			     (:ivec3 nil nil) (:ivec4 nil nil)
			     (:vec2 nil nil) (:vec3 nil nil)
			     (:vec4 nil nil)))
(defparameter *built-in-vars* 
  '((:core ((max-clip-distances (:vec4 nil nil) 
	     "gl_MaxClipDistances" t)
	    (max-clip-planes (:vec4 nil nil) 
	     "gl_MaxClipPlanes" t)
	    (max-draw-buffers (:vec4 nil nil) 
	     "gl_MaxDrawBuffers" t)
	    (max-texture-units (:vec4 nil nil) 
	     "gl_MaxTextureUnits" t)
	    (max-texture-coords (:vec4 nil nil) 
	     "gl_MaxTextureCoords" t)
	    (max-geometry-texture-image-units (:vec4 nil nil) 
	     "gl_MaxGeometryTextureImageUnits" t)
	    (max-texture-image-units (:vec4 nil nil) 
	     "gl_MaxTextureImageUnits" t)
	    (max-vertex-attribs (:vec4 nil nil) 
	     "gl_MaxVertexAttribs" t)
	    (max-vertex-texture-image-units (:vec4 nil nil) 
	     "gl_MaxVertexTextureImageUnits" t)
	    (max-combined-texture-image-units (:vec4 nil nil) 
	     "gl_MaxCombinesTextureImageUnits" t)
	    (max-geometry-varying-components (:vec4 nil nil) 
	     "gl_MaxGeometryVaryingComponents" t)
	    (max-varying-floats (:vec4 nil nil) 
	     "gl_MaxVaryingFloats" t)
	    (max-geometry-output-vertices (:vec4 nil nil) 
	     "gl_MaxGeometryOutputVertices" t)
	    (max-fragment-uniform-components (:vec4 nil nil) 
	    "gl_MaxFragmentUniformComponents" t)
	    (max-geometry-total-output-components (:vec4 nil nil) 
	    "gl_MaxGeometryTotalOutputComponents" t)
	    (max-geometry-uniform-components (:vec4 nil nil) 
	    "gl_MaxGeometryUniformComponents" t)
	    (max-vertex-uniform-components (:vec4 nil nil) 
	     "gl_MaxVertexUniformComponents" t)))
    (:vertex )))
(defparameter *glsl-variables* nil)
(defparameter *glsl-functions* (make-hash-table))
(defparameter *glsl-special-functions* (make-hash-table))
(defparameter *glsl-substitutions* (make-hash-table))
(defparameter *shader-type* nil)

;;------------------------------------------------------------
;; Handy Functions
;;-----------------

(defun kwd (name) 
  (intern (string name) 'keyword))

(defun print-hash (hash-table)
  (loop for x being the hash-keys of hash-table
     :do (print (format nil "~s -> ~s" x (gethash x hash-table))))
  hash-table)

(defun printf (control-string &rest format-arguments)
  (apply #'format (append (list t control-string) format-arguments)))

(defun group (source n)
  "This takes a  flat list and emit a list of lists, each n long
   containing the elements of the original list"
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n)
				   acc))
		   (nreverse (cons source acc))))))
    (if source 
	(rec source nil) 
	nil)))

(defun eqp! (x)
  (lambda (val) (eq val x)))

(defun eqlp! (x)
  (lambda (val) (eql val x)))

(defun equalp! (x)
  (lambda (val) (equal val x)))

(let ((count 0))
  (defun glsl-gensym (&optional (name "var"))
    (setf count (+ 1 count))
    (format nil "~a_varjo_~a" name count)))

(defmacro assocr (item alist &key key (test nil testp) 
			       (test-not nil notp))
  `(cdr (assoc ,item ,alist :key ,key ,@(when testp (list test))
	       ,@(when notp (list test-not)))))
;;------------------------------------------------------------
;; Code Class
;;------------

(defclass code ()
  ((type-spec
    :initarg :type
    :initform nil
    :reader code-type
    :writer (setf code-type))
   (current-line
    :initarg :current-line
    :initform nil
    :reader current-line
    :writer (setf current-line))
   (to-block
    :initarg :to-block
    :initform nil
    :reader to-block
    :writer (setf to-block))
   (to-top
    :initarg :to-top
    :initform nil
    :reader to-top
    :writer (setf to-top))))

(defmethod initialize-instance :after ((code-ob code) 
				       &key type current-line)
  (if (not (and type current-line))
      (error "Type and Code Content must be specified when creating an instance of varjo:code"))
  (setf (slot-value code-ob 'type-spec) (flesh-out-type type)
        (slot-value code-ob 'current-line) current-line))

;;------------------------------------------------------------
;; GLSL Types
;;------------

(defun flesh-out-type (type)
  (if (listp type)
      (if (> (length type) 3)
	  (error "Invalid GLSL Type Definition: ~s has more than 3 components." type)
	  (append type (make-list (- 3 (length type)))))
      (flesh-out-type (list type))))

;; [TODO] Checking length should be a '<' not an eq 
(defun glsl-valid-type (candidate spec)
  (labels ((component-valid-p (c s) 
	     (or (and (null c) (null s))
		 (if (listp s) (find c s) (eq c s))
		 (eq t s))))
    (every #'component-valid-p candidate spec)))

(defun glsl-typep (object type)
  (glsl-valid-type (code-type object) type))

(defun glsl-castablep (minor-type major-type)
  "Returns whether the type minor-type can be cast up to type major-type"
  (or (equal major-type minor-type)
      (not (null (find minor-type (assoc major-type 
					 *implicit-type-casts*
					 :test #'equal)
		       :test #'equal)))))

(defun superior-type (&rest types)
  "find the superior type, types are defined in order or superiority"
  (let ((type-strengths (remove-if #'null 
			       (mapcar (lambda (x) 
					 (position x *glsl-types*
						   :test #'equal))
				       types))))
    (when type-strengths
      (elt *glsl-types* (apply #'max type-strengths)))))

(defun types-compatiblep (&rest types)
  "Make sure every type is or can be cast up to the superior type"
  (let ((superior (apply #'superior-type types)))
    (every #'(lambda (x) (glsl-castablep x superior)) types)))

;;------------------------------------------------------------
;; GLSL Functions
;;----------------

(defun func-in-spec (x)
  (first x))

(defun func-out-spec (x)
  (second x))

(defun func-body (x)
  (third x))

(defun glsl-function (symbol)
  (gethash symbol *glsl-functions*))

(defun glsl-valid-function-args (func args)
  (let ((in-spec (func-in-spec func)))
    (and (eq (length args) (length in-spec))
	 (every #'(lambda (c s) (glsl-typep c s)) 
		args in-spec))))

(defun glsl-resolve-func-type (func args)
  (let ((in-types (mapcar #'code-type args)))
    (loop :for i in (func-out-spec func)
	  :for part from 0
	  :collect (if (numberp i)
		       (nth part (nth i in-types))
		       i))))


(defun glsl-resolve-oper-type (oper args)
  (declare (ignore oper))
  (let ((in-types (mapcar #'code-type args)))
    (declare (ignore in-types))
    `(:implement :resolve-oper-type :now)))

(defun oper-segment-list (list symbol)
  (if (rest list) 
      (list symbol 
	    (first list) 
	    (oper-segment-list (rest list) symbol)) 
      (first list)))


;;------------------------------------------------------------
;; GLSL Variables
;;----------------

(defun var-name (var)
  (first var))

(defun var-type (var)
  (second var))

(defun var-gl-name (var)
  (third var))

(defun var-read-only (var)
  (fourth var))
