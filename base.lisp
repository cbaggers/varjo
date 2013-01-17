;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)

(defparameter *glsl-component-counts*
	       '((:bool . 1) (:int . 1) (:uint . 1) (:float . 1)
		 (:bvec2 . 2) (:bvec3 . 3) (:bvec4 . 4)
		 (:uvec2 . 2) (:uvec3 . 3) (:uvec4 . 4)
		 (:ivec2 . 2) (:ivec3 . 3) (:ivec4 . 4)
		 (:vec2 . 2) (:vec3 . 3) (:vec4 . 4)
		 (:mat2 . 4) (:mat3 . 9) (:mat4 . 16)
		 (:mat2x2 . 4) (:mat2x3 . 6) (:mat2x4 . 8)
		 (:mat3x2 . 6) (:mat3x3 . 9) (:mat3x4 . 12)
		 (:mat4x2 . 8) (:mat4x3 . 12) (:mat4x4 . 16)))

(defconstant *implicit-type-casts* 
  '(((:float nil) (:int nil) (:uint nil))
    ((:vec2 nil) (:ivec2 nil) (:uvec2 nil))
    ((:vec3 nil) (:ivec3 nil) (:uvec3 nil))
    ((:vec4 nil) (:ivec4 nil) (:uvec4 nil))))

(defparameter *glsl-types* '((:void nil) (:bool nil) 
			     (:int nil) (:uint nil)
			     (:float nil) (:bvec2 nil) 
			     (:bvec3 nil) (:bvec4 nil)
			     (:uvec2 nil) (:uvec3 nil) 
			     (:uvec4 nil) (:ivec2 nil)
			     (:ivec3 nil) (:ivec4 nil)
			     (:vec2 nil) (:vec3 nil)
			     (:vec4 nil) 
			     (:mat2 nil) (:mat3 nil)
			     (:mat4 nil) (:mat2x2 nil)
			     (:mat2x3 nil) (:mat2x4 nil)
			     (:mat3x2 nil) (:mat3x3 nil)
			     (:mat3x4 nil) (:mat4x2 nil)
			     (:mat4x3 nil) (:mat4x4 nil)
			     ;; (:ISAMPLER1D NIL)
			     ;; (:ISAMPLER1DARRAY NIL)
			     ;; (:ISAMPLER2D NIL)
			     ;; (:ISAMPLER2DARRAY NIL)
			     ;; (:ISAMPLER2DMS NIL)
			     ;; (:ISAMPLER2DMSARRAY NIL)
			     ;; (:ISAMPLER2DRECT NIL)
			     ;; (:ISAMPLER3D NIL)
			     ;; (:ISAMPLERBUFFER NIL)
			     ;; (:ISAMPLERCUBE NIL)
			     ;; (:ISAMPLERCUBEARRAY NIL)
			     ;; (:SAMPLER1D NIL) 
			     ;; (:SAMPLER1DARRAY NIL)
			     ;; (:SAMPLER1DARRAYSHADOW NIL)
			     ;; (:SAMPLER1DSHADOW NIL) 
			     ;; (:SAMPLER2D NIL)
			     ;; (:SAMPLER2DARRAY NIL)
			     ;; (:SAMPLER2DARRAYSHADOW NIL)
			     ;; (:SAMPLER2DMS NIL)
			     ;; (:SAMPLER2DMSARRAY NIL)
			     ;; (:SAMPLER2DRECT NIL)
			     ;; (:SAMPLER2DRECTSHADOW NIL)
			     ;; (:SAMPLER2DSHADOW NIL)
			     ;; (:SAMPLER3D NIL)
			     ;; (:SAMPLERBUFFER NIL)
			     ;; (:SAMPLERCUBE NIL)
			     ;; (:SAMPLERCUBEARRAY NIL)
			     ;; (:SAMPLERCUBEARRAYSHADOW NIL)
			     ;; (:SAMPLERCUBESHADOW NIL)
			     ;; (:USAMPLER1D NIL)
			     ;; (:USAMPLER1DARRAY NIL)
			     ;; (:USAMPLER2D NIL)
			     ;; (:USAMPLER2DARRAY NIL)
			     ;; (:USAMPLER2DMS NIL)
			     ;; (:USAMPLER2DMSARRAY NIL)
			     ;; (:USAMPLER2DRECT NIL)
			     ;; (:USAMPLER3D NIL)
			     ;; (:USAMPLERBUFFER NIL)
			     ;; (:USAMPLERCUBE NIL)
			     ;; (:USAMPLERCUBEARRAY NIL)
			     ))

;; [TODO] What the hell is with the multitexcoord (vertex)
(defparameter *built-in-vars* 
  '((:core 
     (max-clip-distances :int "gl_MaxClipDistances" t)
     (max-clip-planes :int "gl_MaxClipPlanes" t)
     (max-draw-buffers :int "gl_MaxDrawBuffers" t)
     (max-texture-units :int "gl_MaxTextureUnits" t)
     (max-texture-coords :int "gl_MaxTextureCoords" t)
     (max-geometry-texture-image-units :int 
      "gl_MaxGeometryTextureImageUnits" t)
     (max-texture-image-units :int "gl_MaxTextureImageUnits" t)
     (max-vertex-attribs :int "gl_MaxVertexAttribs" t)
     (max-vertex-texture-image-units :int 
      "gl_MaxVertexTextureImageUnits" t)
     (max-combined-texture-image-units :int 
      "gl_MaxCombinesTextureImageUnits" t)
     (max-geometry-varying-components :int 
      "gl_MaxGeometryVaryingComponents" t)
     (max-varying-floats :int "gl_MaxVaryingFloats" t)
     (max-geometry-output-vertices :int 
      "gl_MaxGeometryOutputVertices" t)
     (max-fragment-uniform-components :int 
      "gl_MaxFragmentUniformComponents" t)
     (max-geometry-total-output-components :int 
      "gl_MaxGeometryTotalOutputComponents" t)
     (max-geometry-uniform-components :int 
      "gl_MaxGeometryUniformComponents" t)
     (max-vertex-uniform-components :int 
      "gl_MaxVertexUniformComponents" t))
    (:vertex 
     (vertex-id :int "gl_VertexID" t)
     (instance-id :int "gl_InstanceID" t)
     (color :vec4 "gl_Color" t)
     (secondary-color :vec4 "gl_SecondaryColor" t)
     (normal :vec3 "gl_Normal" t)
     (vertex :vec4 "gl_Vertex" t)
     (fog-coord :float "gl_FogCoord" t)
     (per-vertex :per-vertex-struct "gl_PerVertex" t)
     (front-color :vec4 "gl_FrontColor")
     (back-color :vec4 "gl_BackColor")
     (front-secondary-color :vec4 "gl_FrontSecondaryColor")
     (back-secondary-color :vec4 "gl_FrontSecondaryColor")
     (tex-coord :vec4 "gl_TexCoord")
     (fog-frag-coord :float "gl_FogFragCoord"))
    (:fragment 
     (frag-coord :vec4 "gl_FragCoord" t)
     (front-facing :bool  "gl_FrontFacing" t)
     (clip-distance (:float t) "gl_ClipDistance" t)
     (point-coord :vec2  "gl_PointCoord" t)
     (primitive-id :int "gl_PrimitiveID" t)
     (frag-depth :float "gl_FragDepth" nil))
    (:geometry
     (primitive-id-in :int "gl_PrimitiveIDIn" t)
     (per-vertex :per-vertex-struct "gl_PerVertex")
     (primitive-id :int "gl_PrimitiveID")
     (layer :int "gl_Layer"))))

(defparameter *glsl-variables* nil)
(defparameter *glsl-functions* nil)
(defparameter *glsl-special-functions* (make-hash-table))
(defparameter *glsl-substitutions* nil)
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

(defun symb (&rest args)
  "This takes a list of symbols (or strings) and outputs one 
   symbol.
   If the input is symbol/s then the output is a regular symbol
   If the input is string/s, then the output is
   a |symbol like this|"
  (values (intern (format nil "~{~a~}" args))))

(defun truep (x) (not (null x)))

(defun eqp! (x)
  (lambda (val) (eq val x)))

(defun eqlp! (x)
  (lambda (val) (eql val x)))

(defun equalp! (x)
  (lambda (val) (equal val x)))

(let ((count 0))
  (defun glsl-gensym (&optional (name "var"))
    (setf count (+ 1 count))
    (format nil "_~a_~a" name count)))

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
    :writer (setf to-top))
   (read-only
    :initarg :read-only
    :initform nil
    :reader read-only
    :writer (setf read-only))
   (place
    :initarg :place
    :initform nil
    :reader place
    :writer (setf place))
   (out-vars
    :initarg :out-vars
    :initform nil
    :reader out-vars
    :writer (setf out-vars))
   (invariant
    :initarg :invariant
    :initform nil
    :reader invariant
    :writer (setf invariant))))


(defmethod initialize-instance :after ((code-ob code) 
				       &key type current-line)
  (if (not (and type current-line))
      (error "Type and current-line must be specified when creating an instance of varjo:code"))
  (setf (slot-value code-ob 'type-spec) (flesh-out-type type)
        (slot-value code-ob 'current-line) current-line))

(defgeneric merge-obs (objs &key type current-line to-block 
			      to-top place out-vars invariant))

(defmethod merge-obs ((objs list) &key type current-line 
			 (to-block nil set-block)
			 (to-top nil set-top)
			 (place nil)
			 (out-vars nil set-out-vars)
			 (invariant nil))
  (make-instance 'code
		 :type (if type type (error "type is mandatory")) 
		 :current-line 
		 (if current-line current-line 
		     (error "current-line is mandatory")) 
		 :to-block (if set-block
			       to-block
			       (mapcan #'to-block objs))
		 :to-top (if set-top
			       to-top
			       (mapcan #'to-top objs))
		 :place place
		 :out-vars (if set-out-vars
			       out-vars
			       (mapcan #'out-vars objs))
		 :invariant invariant))

(defmethod merge-obs ((objs code) 
		      &key (type nil set-type)
			(current-line nil set-current-line) 
			(to-block nil set-block)
			(to-top nil set-top)
			(place nil)
			(out-vars nil set-out-vars)
			(invariant nil))
  (make-instance 'code
		 :type (if set-type
			   type
			   (code-type objs)) 
		 :current-line (if set-current-line 
				   current-line 
				   (current-line objs)) 
		 :to-block (if set-block
			       to-block
			       (to-block objs))
		 :to-top (if set-top
			       to-top
			       (to-top objs))
		 :place place
		 :out-vars (if set-out-vars
			       out-vars
			       (out-vars objs))
		 :invariant invariant))


;;------------------------------------------------------------
;; GLSL Types
;;------------

(defun flesh-out-type (type)
  (if (listp type)
      (if (> (length type) 2)
	  (error "Invalid GLSL Type Definition: ~s has more than 2 components." type)
	  (append type (make-list (- 2 (length type)))))
      (flesh-out-type (list type))))

(defun glsl-valid-type (candidate spec)
  (let ((type-s (first spec)) (type-c (first candidate))
	(length-s (second spec)) (length-c (second candidate)))
    (not
     (null
      (and (or (eq type-s t) (if (listp type-s) 
				 (find type-c type-s) 
				 (eq type-c type-s)))
	   (or (eq length-c length-s)
	       (and (eq length-s t) length-c)
	       (when (and (numberp length-c) (numberp length-s))
		 (<= length-c length-s))))))))

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

(defun type-component-count (type-spec)
  (let* ((full-type (flesh-out-type type-spec))
	 (type (first full-type))
	 (length (assocr type *glsl-component-counts*)))
    (if length
	length
	(error "Type '~a' is not a vector or matrix componant type" type))))

;;------------------------------------------------------------
;; GLSL Functions
;;----------------

(defun func-in-spec (x)
  (first x))

(defun func-out-spec (x)
  (second x))

(defun func-body (x)
  (third x))

(defun func-compatible-args (x)
  (fourth x))

(defun func-args-match (x)
  (fifth x))

(defun func-place (x)
  (sixth x))

(defun glsl-valid-function-args (func args)
  (let ((in-spec (func-in-spec func))
	(types (mapcar #'code-type args)))
    (and (eq (length args) (length in-spec))
	 (every #'(lambda (c s) (glsl-typep c s)) 
		args in-spec)
	 (if (func-compatible-args func)
	     (apply #'types-compatiblep types)
	     t)
	 (if (func-args-match func)
	     (every (equalp! (first types)) types)
	     t))))

(defun glsl-resolve-func-type (func args)
  ;; return the output type spec except for where 
  ;; the spec part is a number, in which case we 
  ;; take that part from the number'th in-arg.
  ;; Note that in cases where the args are meant
  ;; to be compatible that means we need to take
  ;; it from the superior in-arg type
  (let ((in-types (mapcar #'code-type args)))
    (loop :for i in (func-out-spec func)
	  :for part from 0
	  :collect (if (numberp i)
		       (nth part (if (func-compatible-args func)
				     (apply #'superior-type 
					    in-types)
				     (nth i in-types)))
		       i))))


;; (defun glsl-resolve-oper-type (oper args)
;;   (declare (ignore oper))
;;   (let ((in-types (mapcar #'code-type args)))
;;     (declare (ignore in-types))
;;     `(:implement :resolve-oper-type :now)))

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

;;------------------------------------------------------------
;; GLSL Structs
;;--------------

(defun struct-init-form (struct)
  (let* ((struct-name (first struct))
	 (slots (rest struct)))
    (format nil "struct ~a {~%~{~a~%~}};"
	    struct-name (mapcar #'compile-struct-type slots))))

(defun compile-struct-type (slot)
  (let ((name (or (third slot) (first slot)))
	(type (flesh-out-type (second slot))))
    (let ((principle (first type))
	  (len (third type)))
      (if len
	  (format nil "    ~a ~a[~a];" 
		  principle name (if len len ""))
	  (format nil "    ~a ~a;" 
		  principle name)))))

(defun struct-funcs (struct)
  (let ((struct-name (first struct))
	(slots (rest struct)))
    (if (not (keywordp struct-name))
	(error "Struct names must be keywords")
	(if (find struct-name *glsl-types*)
	    (error "Type already exists (~a)" struct-name)
	    (cons 
	     (list 
	      (symb 'make- struct-name)
	      (vlambda :in-args nil
		       :output-type struct-name
		       :transform "")
	      (vlambda :in-args (loop for slot in slots
				      :collect (subseq slot 0 2))
		       :output-type struct-name
		       :transform (format nil "~a(~{~a~^,~^ ~})"
					  struct-name
					  (loop for slot in slots
						collect "~a"))))
	     (loop 
	       :for slot :in slots 
	       :collect
	       (list
		(symb struct-name '- (first slot))
		(vlambda :in-args `((x (,struct-name)))
			 :output-type (second slot)
			 :transform (format nil "~~a.~a" 
					    (or (third slot)
						(first slot)))
			 :place t))))))))
