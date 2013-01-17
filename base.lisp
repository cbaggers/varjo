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
			     (:vec4 nil nil) 
			     (:mat2 nil nil) (:mat3 nil nil)
			     (:mat4 nil nil) (:mat2x2 nil nil)
			     (:mat2x3 nil nil) (:mat2x4 nil nil)
			     (:mat3x2 nil nil) (:mat3x3 nil nil)
			     (:mat3x4 nil nil) (:mat4x2 nil nil)
			     (:mat4x3 nil nil) (:mat4x4 nil nil)
			     ;; (:ISAMPLER1D NIL NIL)
			     ;; (:ISAMPLER1DARRAY NIL NIL)
			     ;; (:ISAMPLER2D NIL NIL)
			     ;; (:ISAMPLER2DARRAY NIL NIL)
			     ;; (:ISAMPLER2DMS NIL NIL)
			     ;; (:ISAMPLER2DMSARRAY NIL NIL)
			     ;; (:ISAMPLER2DRECT NIL NIL)
			     ;; (:ISAMPLER3D NIL NIL)
			     ;; (:ISAMPLERBUFFER NIL NIL)
			     ;; (:ISAMPLERCUBE NIL NIL)
			     ;; (:ISAMPLERCUBEARRAY NIL NIL)
			     ;; (:SAMPLER1D NIL NIL) 
			     ;; (:SAMPLER1DARRAY NIL NIL)
			     ;; (:SAMPLER1DARRAYSHADOW NIL NIL)
			     ;; (:SAMPLER1DSHADOW NIL NIL) 
			     ;; (:SAMPLER2D NIL NIL)
			     ;; (:SAMPLER2DARRAY NIL NIL)
			     ;; (:SAMPLER2DARRAYSHADOW NIL NIL)
			     ;; (:SAMPLER2DMS NIL NIL)
			     ;; (:SAMPLER2DMSARRAY NIL NIL)
			     ;; (:SAMPLER2DRECT NIL NIL)
			     ;; (:SAMPLER2DRECTSHADOW NIL NIL)
			     ;; (:SAMPLER2DSHADOW NIL NIL)
			     ;; (:SAMPLER3D NIL NIL)
			     ;; (:SAMPLERBUFFER NIL NIL)
			     ;; (:SAMPLERCUBE NIL NIL)
			     ;; (:SAMPLERCUBEARRAY NIL NIL)
			     ;; (:SAMPLERCUBEARRAYSHADOW NIL NIL)
			     ;; (:SAMPLERCUBESHADOW NIL NIL)
			     ;; (:USAMPLER1D NIL NIL)
			     ;; (:USAMPLER1DARRAY NIL NIL)
			     ;; (:USAMPLER2D NIL NIL)
			     ;; (:USAMPLER2DARRAY NIL NIL)
			     ;; (:USAMPLER2DMS NIL NIL)
			     ;; (:USAMPLER2DMSARRAY NIL NIL)
			     ;; (:USAMPLER2DRECT NIL NIL)
			     ;; (:USAMPLER3D NIL NIL)
			     ;; (:USAMPLERBUFFER NIL NIL)
			     ;; (:USAMPLERCUBE NIL NIL)
			     ;; (:USAMPLERCUBEARRAY NIL NIL)
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
     (clip-distance (:float :array t) "gl_ClipDistance" t)
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
    :writer (setf read-only))))

(defmethod initialize-instance :after ((code-ob code) 
				       &key type current-line)
  (if (not (and type current-line))
      (error "Type and Code Content must be specified when creating an instance of varjo:code"))
  (setf (slot-value code-ob 'type-spec) (flesh-out-type type)
        (slot-value code-ob 'current-line) current-line))

;; (defgeneric copy-code-ob (code-ob &key type current-line
;; 				   to-block to-top read-only))

;; (defmethod copy-code-ob ((obj code) &key (type nil set-type)
;; 				   (current-line nil set-line)
;; 				   (to-block nil set-block)
;; 				   (to-top nil set-top)
;; 				   (read-only nil read-only))
;;     (make-instance 'code
;; 		   :type (if set-type
;; 			     type
;; 			     (code-type obj))
;; 		   :current-line (if set-line
;; 				     current-line
;; 				     (current-line obj))
;; 		   :))

;; (defmethod copy-code-ob ((obj list) &key (type nil set-type)
;; 				      (current-line nil set-line)
;; 				      (to-block nil set-block)
;; 				      (to-top nil set-top)
;; 				      (read-only nil read-only))
;;   )

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

(defun func-compatible-args (x)
  (fourth x))

(defun func-args-match (x)
  (fifth x))

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
	  (structure (second type))
	  (len (third type)))
      (if (eq structure :array)
	  (format nil "    ~a ~a[~a];" 
		  principle name (if len len ""))
	  (format nil "    ~a ~a;" 
		  principle name)))))

;; (defun struct-macros (struct)
;;   (let ((struct-name (first struct)))
;;     (list (symb 'make struct-name) 
;; 	  (LAMBDA () `(%instance-struct ,struct-name)))))

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
						(first slot)))))))))))
