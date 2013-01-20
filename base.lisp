;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)

(defparameter *glsl-type-sizes*
	       '((:bool . 1) (:int . 1) (:uint . 1) (:float . 1)
		 (:bvec2 . 1) (:bvec3 . 1) (:bvec4 . 1)
		 (:uvec2 . 1) (:uvec3 . 1) (:uvec4 . 1)
		 (:ivec2 . 1) (:ivec3 . 1) (:ivec4 . 1)
		 (:vec2 . 1) (:vec3 . 1) (:vec4 . 1)
		 (:mat2 . 1) (:mat3 . 3) (:mat4 . 4)
		 (:mat2x2 . 2) (:mat2x3 . 2) (:mat2x4 . 2)
		 (:mat3x2 . 3) (:mat3x3 . 3) (:mat3x4 . 3)
		 (:mat4x2 . 4) (:mat4x3 . 4) (:mat4x4 . 4)))

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

;; following 3 were constant
(defparameter *shader-types*
  '(:vertex :fragment :geometry))

(defparameter -default-version- :330)

(defparameter *implicit-type-casts*
  '(((:float nil nil) (:int nil nil) (:uint nil nil))
    ((:vec2 nil nil) (:ivec2 nil nil) (:uvec2 nil nil))
    ((:vec3 nil nil) (:ivec3 nil nil) (:uvec3 nil nil))
    ((:vec4 nil nil) (:ivec4 nil nil) (:uvec4 nil nil))
    ((:mat2 nil nil) (:mat2x2 nil nil))
    ((:mat3 nil nil) (:mat3x3 nil nil))
    ((:mat4 nil nil) (:mat4x4 nil nil))))

(defparameter *types* nil)

(defparameter *struct-definitions* nil)

(defparameter *built-in-types* '((:none nil)
				 (:void nil) (:bool nil) 
				 (:int nil) (:uint nil)
				 (:float nil) (:double nil)
				 (:bvec2 nil) 
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
				 (:isampler1d nil)
				 (:isampler1darray nil)
				 (:isampler2d nil)
				 (:isampler2darray nil)
				 (:isampler2dms nil)
				 (:isampler2dmsarray nil)
				 (:isampler2drect nil)
				 (:isampler3d nil)
				 (:isamplerbuffer nil)
				 (:isamplercube nil)
				 (:isamplercubearray nil)
				 (:sampler1d nil) 
				 (:sampler1darray nil)
				 (:sampler1darrayshadow nil)
				 (:sampler1dshadow nil) 
				 (:sampler2d nil)
				 (:sampler2darray nil)
				 (:sampler2darrayshadow nil)
				 (:sampler2dms nil)
				 (:sampler2dmsarray nil)
				 (:sampler2drect nil)
				 (:sampler2drectshadow nil)
				 (:sampler2dshadow nil)
				 (:sampler3d nil)
				 (:samplerbuffer nil)
				 (:samplercube nil)
				 (:samplercubearray nil)
				 (:samplercubearrayshadow nil)
				 (:samplercubeshadow nil)
				 (:usampler1d nil)
				 (:usampler1darray nil)
				 (:usampler2d nil)
				 (:usampler2darray nil)
				 (:usampler2dms nil)
				 (:usampler2dmsarray nil)
				 (:usampler2drect nil)
				 (:usampler3d nil)
				 (:usamplerbuffer nil)
				 (:usamplercube nil)
				 (:usamplercubearray nil)))

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
     (gl-in (gl-per-vertex-v t) "gl_PerVertex" t)
     (per-vertex gl-per-vertex-v "gl_PerVertex" t)
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
     (per-vertex 'gl-per-vertex-g "gl_PerVertex")
     (primitive-id :int "gl_PrimitiveID")
     (layer :int "gl_Layer"))))

(defparameter *glsl-variables* nil)
(defparameter *glsl-functions* nil)
(defparameter *glsl-special-functions* nil)
(defparameter *glsl-substitutions* nil)
(defparameter *shader-context* nil)

;;------------------------------------------------------------
;; Handy Functions
;;-----------------

(defun acons-many (data a-list)
  (if data (let* ((func (first data))
		  (name (first func))
		  (body (second func)))
	     (acons name (cons body (rest (assoc name a-list)))
		    (acons-many (rest data) a-list)))
      a-list))

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

(defun eq-elements (list) 
  (or (null list) (every (eqp! (car list)) list)))

(defun eql-elements (list) 
  (or (null list) (every (eqlp! (car list)) list)))

(defun equal-elements (list) 
  (or (null list) (every (equalp! (car list)) list)))

(defun identity-filter (list t-map) 
  (mapcan (lambda (x m) (when m (list x))) list t-map))

(let ((count 0))
  (defun glsl-gensym (&optional (name "var"))
    (setf count (+ 1 count))
    (let ((safe-name (cl-ppcre:regex-replace-all "[^a-zA-Z0-9]"
						 (string name)
						 "_")))
      (format nil "_~a_~a" safe-name count))))



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
   (out-vars
    :initarg :out-vars
    :initform nil
    :reader out-vars
    :writer (setf out-vars))
   (invariant
    :initarg :invariant
    :initform nil
    :reader invariant
    :writer (setf invariant))
   (returns
    :initarg :returns
    :initform nil
    :reader returns
    :writer (setf returns))))


(defmethod initialize-instance :after 
    ((code-ob code) &key (type nil set-type)
		      (current-line nil set-current))
  (if (not (and set-current set-type))
      (error "Type and current-line must be specified when creating an instance of varjo:code"))
  (setf (slot-value code-ob 'type-spec) (flesh-out-type type)
        (slot-value code-ob 'current-line) current-line))

(defgeneric merge-obs (objs &key type current-line to-block 
			      to-top out-vars invariant returns))

(defmethod merge-obs ((objs list) &key type current-line 
			 (to-block nil set-block)
			 (to-top nil set-top)
			 (out-vars nil set-out-vars)
			 (invariant nil) (returns nil set-returns))
  (make-instance 'code
		 :type (if type type (error "type is mandatory")) 
		 :current-line current-line 
		 :to-block (if set-block
			       to-block
			       (mapcan #'to-block objs))
		 :to-top (if set-top
			       to-top
			       (mapcan #'to-top objs))
		 :out-vars (if set-out-vars
			       out-vars
			       (mapcan #'out-vars objs))
		 :invariant invariant
		 :returns (if set-returns
			       returns
			       (mapcan #'returns objs))))

(defmethod merge-obs ((objs code) 
		      &key (type nil set-type)
			(current-line nil set-current-line) 
			(to-block nil set-block)
			(to-top nil set-top)
			(out-vars nil set-out-vars)
			(invariant nil) (returns nil set-returns))
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
		 :out-vars (if set-out-vars
			       out-vars
			       (out-vars objs))
		 :read-only (read-only objs)
		 :invariant invariant
		 :returns (if set-returns
			      returns
			      (returns objs))))


;;------------------------------------------------------------
;; GLSL Types
;;------------

(defun flesh-out-type-with-check (type)
  (if (not (listp type))
      (flesh-out-type-with-check (list type))
      (if (if (consp (first type))
	      (every #'(lambda (x) (assoc x *types*))
		     (first type))
	      (assoc (first type) *types*))
	  (flesh-out-type type)
	  (error "Varjo: '~s' is not a valid type in this context ~a" type *shader-context*))))

(defun flesh-out-type (type)
  (if (consp type)
      (if (> (length type) 3)
	  (error "Invalid GLSL Type Definition: ~s has more than 3 components." type)
	  (append type (make-list (- 3 (length type)))))
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

(defun set-place-t (type)
  (list (first type) (second type) t))

(defun set-place-nil (type)
  (list (first type) (second type) nil))

(defun get-place (x)
  (third x))

(defun placep (object)
  (get-place (code-type object)))

(defun glsl-typep (object type)
  (glsl-valid-type (code-type object) type))

(defun type-equal (a b)
  (equal (subseq a 0 2) (subseq b 0 2)))

(defun glsl-castablep (minor-type major-type)
  "Returns whether the type minor-type can be cast up to type major-type"
  (or (type-equal major-type minor-type)
      (not (null (find minor-type (assoc major-type 
					 *implicit-type-casts*
					 :test #'type-equal)
		       :test #'type-equal)))))

(defun superior-type (&rest types)
  "find the superior type, types are defined in order or superiority"
  (let ((type-strengths 
	  (remove-if #'null 
		     (mapcar (lambda (x) 
			       (position x *types*
					 :test #'type-equal))
			     types))))
    (when type-strengths
      (elt *types* (apply #'max type-strengths)))))

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

(defun vlambda (&key in-args output-type transform
		  context-restriction)
  (list (mapcar #'flesh-out-type
		(mapcar #'second in-args))
	(flesh-out-type output-type)
	transform
	(mapcar #'(lambda (x) (find :compatible x)) in-args)
	(mapcar #'(lambda (x) (find :match x)) in-args)
	context-restriction))

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

(defun func-restriction (x)
  (sixth x))

(defun glsl-valid-function-args (func args)
  (let ((in-spec (func-in-spec func))
	(types (mapcar #'code-type args)))
    (and (eq (length args) (length in-spec))
	 (every #'(lambda (c s) (if (get-place s)
				    (get-place c)
				    t)) types in-spec)
	 (every #'(lambda (c s) (glsl-typep c s)) 
		args in-spec)
	 (apply #'types-compatiblep
		(identity-filter types (func-compatible-args func)))
	 (let* ((filtered-types (identity-filter 
				 types (func-args-match func)))
		(comp (first filtered-types)))
	   (notany #'null (mapcar #'(lambda (x)
				      (type-equal x comp)) 
				  filtered-types))))))

(defun glsl-resolve-func-type (func args)
  ;; return the output type spec except for where 
  ;; the spec part is a number, in which case we 
  ;; take that part from the number'th in-arg.
  ;; Note that in cases where the args are meant
  ;; to be compatible that means we need to take
  ;; it from the superior in-arg type
  (let* ((in-types (mapcar #'code-type args))
	 (superior (apply #'superior-type 
			 (identity-filter 
			  in-types (func-compatible-args func))))
	 (final-type
	   (flesh-out-type
	    (loop :for i in (func-out-spec func)
		  :for part from 0
		  :collect 
		  (if (numberp i)
		      (nth part (if (nth i (func-compatible-args
					    func))
				    superior
				    (nth i in-types)))
		      i)))))
    ;; (print in-types)
    ;; (print (func-out-spec func))
    ;; (print (func-compatible-args func))
    ;; (print final-type)
    final-type))

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

(defun compile-let-forms (let-forms &optional (typify t))
  ;; takes forms and returns a list of two things
  ;; the compiled forms and the variable forms which can be 
  ;; appended to *glsl-variables*
  (labels ((var-name (form) 
	     (if (listp (first form)) (first (first form))
		 (first form)))
	   (var-type (form) 
	     (when (listp (first form))
	       (flesh-out-type (second (first form)))))
	   (val (form) 
	     (second form))
	   (compile-form (name type value)
	     (if value
		 (if typify
		     (varjo->glsl `(%typify 
				    (setf (%make-var ,name ,type)
					  ,value)))
		     (varjo->glsl `(setf (%make-var ,name ,type) 
					 ,value)))
		 (if typify
		     (varjo->glsl `(%typify 
				    (%make-var ,name ,type)))
		     (varjo->glsl `(%make-var ,name ,type) )))))
    (let* ((val-objs (loop :for form in let-forms
			   :collect (varjo->glsl (val form))))
	   (var-names (mapcar #'var-name let-forms))
	   (var-gl-names (mapcar #'glsl-gensym var-names))
	   (var-types (loop :for form :in let-forms
			    :for obj :in val-objs
			    :collect (or (var-type form)
					 (when obj 
					   (code-type obj))))))
      ;; THe above can be nil when the val half __^^^^^^
      ;; of the let form is left blank
      (list (mapcar #'compile-form var-gl-names var-types val-objs)
	    (mapcar #'list var-names var-types var-gl-names)))))

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
  (%struct-funcs (first struct) nil nil (rest struct)))

(defun %struct-funcs (name slot-prefix context-restriction slots)
  (cons 
   (list (symb 'make- (or slot-prefix name))
	 (vlambda :in-args (loop for slot in slots
				 :collect (subseq slot 0 2))
		  :output-type name
		  :transform (format nil "~a(~{~a~^,~^ ~})"
				     name
				     (loop for slot in slots
					   collect "~a"))
		  :context-restriction context-restriction))
   (loop :for slot :in slots 
	 :collect
	 (list (symb (or slot-prefix name) '- (first slot))
	       (vlambda :in-args `((x (,name)))
			:output-type (set-place-t 
				      (flesh-out-type 
				       (second slot)))
			:transform (format nil "~~a.~a" 
					   (or (third slot)
					       (first slot)))
			:context-restriction context-restriction)))))

(defmacro vdefstruct (name &body slots)
  (let ((*types* (cons (list name nil) *built-in-types*))) 
    `(progn     
       (setf *glsl-functions* 
	     (acons-many ',(%struct-funcs name nil nil slots)
			 *glsl-functions*))
       (setf *struct-definitions*
	     (acons ',name ',slots
		    *struct-definitions*))
       ',name)))

(defmacro %vdefstruct (name (&key slot-prefix context-restriction)
		       &body slots)
  (let ((*types* (cons (list name nil) *built-in-types*))) 
    `(progn
       (setf *glsl-functions* 
	     (acons-many ',(%struct-funcs name slot-prefix
					  context-restriction 
					  slots)
			 *glsl-functions*))
       (setf *built-in-types* 
	     (acons ',name '(nil) *built-in-types*))
       ',name)))
