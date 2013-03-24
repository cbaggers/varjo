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
    (:mat2 . 2) (:mat3 . 3) (:mat4 . 4)
    (:mat2x2 . 2) (:mat2x3 . 2) (:mat2x4 . 2)
    (:mat3x2 . 3) (:mat3x3 . 3) (:mat3x4 . 3)
    (:mat4x2 . 4) (:mat4x3 . 4) (:mat4x4 . 4)))

(defparameter *glsl-component-counts*
  '((:vec2 . 2) (:vec3 . 3) (:vec4 . 4)
    (:ivec2 . 2) (:ivec3 . 3) (:ivec4 . 4)
    (:uvec2 . 2) (:uvec3 . 3) (:uvec4 . 4)
    (:mat2 . 4) (:mat3 . 9) (:mat4 . 16)
    (:mat2x2 . 4) (:mat2x3 . 6) (:mat2x4 . 8)
    (:mat3x2 . 6) (:mat3x3 . 9) (:mat3x4 . 12)
    (:mat4x2 . 8) (:mat4x3 . 12) (:mat4x4 . 16)))

(defparameter *glsl-component-type*
  '((:bvec2 . :bool)   (:bvec3 . :bool)   (:bvec4 . :bool)
    (:uvec2 . :uint)   (:uvec3 . :uint)   (:uvec4 . :uint)
    (:ivec2 . :int)    (:ivec3 . :int)    (:ivec4 . :int)
    (:vec2 . :float)   (:vec3 . :float)   (:vec4 . :float)
    (:mat2 . :float)   (:mat3 . :float)   (:mat4 . :float)
    (:mat2x2 . :float) (:mat2x3 . :float) (:mat2x4 . :float)
    (:mat3x2 . :float) (:mat3x3 . :float) (:mat3x4 . :float)
    (:mat4x2 . :float) (:mat4x3 . :float) (:mat4x4 . :float)))

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

(defparameter *built-in-types* 
  `((vgl-per-vertex-g nil nil "gl_per_vertex_g")
    (vgl-per-vertex-v nil nil "gl_per_vertex_v")
    (:none nil nil "none") (:void nil nil "void") 
    (:bool nil nil "bool") (:int nil nil "int") 
    (:uint nil nil "uint") (:float nil nil "float")
    (:double nil nil "double") (:bvec2 nil nil "bvec2") 
    (:bvec3 nil nil "bvec3") (:bvec4 nil nil "bvec4")
    (:uvec2 nil nil "uvec2") (:uvec3 nil nil "uvec3")
    (:uvec4 nil nil "uvec4") (:ivec2 nil nil "ivec2") 
    (:ivec3 nil nil "ivec3") (:ivec4 nil nil "ivec4")
    (:vec2 nil nil "vec2") (:vec3 nil nil "vec3")
    (:vec4 nil nil "vec4") (:mat2 nil nil "mat2") 
    (:mat3 nil nil "mat3") (:mat4 nil nil "mat4")
    (:mat2x2 nil nil "mat2x2") (:mat2x3 nil nil "mat2x3")
    (:mat2x4 nil nil "mat2x4") (:mat3x2 nil nil "mat3x2")
    (:mat3x3 nil nil "mat3x3") (:mat3x4 nil nil "mat3x4")
    (:mat4x2 nil nil "mat4x2") (:mat4x3 nil nil "mat4x3")
    (:mat4x4 nil nil "mat4x4") (:isampler-1D nil nil "isampler1D")
    (:isampler-1d-Array nil nil "isampler1DArray")
    (:isampler-2D nil nil "isampler2D")
    (:isampler-2d-Array nil nil "isampler2DArray")
    (:isampler-2d-MS nil nil "isampler2DMS")
    (:isampler-2d-MS-Array nil nil "isampler2DMSArray")
    (:isampler-2d-Rect nil nil "isampler2DRect")
    (:isampler-3d nil nil "isampler3d")
    (:isampler-Buffer nil nil "isamplerBuffer")
    (:isampler-Cube nil nil "isamplerCube")
    (:isampler-Cube-Array nil nil "isamplerCubeArray")
    (:sampler-1D nil nil "sampler1D")
    (:sampler-1d-Array nil nil "sampler1DArray")
    (:sampler-1d-Array-Shadow nil nil "sampler1DArrayShadow")
    (:sampler-1d-Shadow nil nil "sampler1DShadow")
    (:sampler-2D nil nil "sampler2D")
    (:sampler-2d-Array nil nil "sampler2DArray")
    (:sampler-2d-Array-Shadow nil nil "sampler2DArrayShadow")
    (:sampler-2d-MS nil nil "sampler2DMS")
    (:sampler-2d-MS-Array nil nil "sampler2DMSArray")
    (:sampler-2d-Rect nil nil "sampler2DRect")
    (:sampler-2d-Rect-Shadow nil nil "sampler2DRectShadow")
    (:sampler-2d-Shadow nil nil "sampler2DShadow")
    (:sampler-3d nil nil "sampler3d")
    (:sampler-Buffer nil nil "samplerBuffer")
    (:sampler-Cube nil nil "samplerCube")
    (:sampler-Cube-Array nil nil "samplerCubeArray")
    (:sampler-Cube-Array-Shadow nil nil "samplerCubeArrayShadow")
    (:sampler-Cube-Shadow nil nil "samplerCubeShadow")
    (:usampler-1D nil nil "usampler1D")
    (:usampler-1d-Array nil nil "usampler1DArray")
    (:usampler-2D nil nil "usampler2D")
    (:usampler-2d-Array nil nil "usampler2DArray")
    (:usampler-2d-MS nil nil "usampler2DMS")
    (:usampler-2d-MS-Array nil nil "usampler2DMSArray")
    (:usampler-2d-Rect nil nil "usampler2DRect") 
    (:usampler-3d nil nil "usampler3d")
    (:usampler-Buffer nil nil "usamplerBuffer")
    (:usampler-Cube nil nil "usamplerCube")
    (:usampler-Cube-Array nil nil "usamplerCubeArray")))

(defparameter *built-in-vars* 
  '((:core 
     (gl-max-clip-distances :int "gl_MaxClipDistances" t)
     (gl-max-clip-planes :int "gl_MaxClipPlanes" t)
     (gl-max-draw-Buffers :int "gl_MaxDrawBuffers" t)
     (gl-max-texture-units :int "gl_MaxTextureUnits" t)
     (gl-max-texture-coords :int "gl_MaxTextureCoords" t)
     (gl-max-geometry-texture-image-units :int 
      "gl_MaxGeometryTextureImageUnits" t)
     (gl-max-texture-image-units :int "gl_MaxTextureImageUnits" t)
     (gl-max-vertex-attribs :int "gl_MaxVertexAttribs" t)
     (gl-max-vertex-texture-image-units :int 
      "gl_MaxVertexTextureImageUnits" t)
     (gl-max-combined-texture-image-units :int 
      "gl_MaxCombinesTextureImageUnits" t)
     (gl-max-geometry-varying-components :int 
      "gl_MaxGeometryVaryingComponents" t)
     (gl-max-varying-floats :int "gl_MaxVaryingFloats" t)
     (gl-max-geometry-output-vertices :int 
      "gl_MaxGeometryOutputVertices" t)
     (gl-max-fragment-uniform-components :int 
      "gl_MaxFragmentUniformComponents" t)
     (gl-max-geometry-total-output-components :int 
      "gl_MaxGeometryTotalOutputComponents" t)
     (gl-max-geometry-uniform-components :int 
      "gl_MaxGeometryUniformComponents" t)
     (gl-max-vertex-uniform-components :int 
      "gl_MaxVertexUniformComponents" t))
    (:vertex 
     (gl-vertex-id :int "gl_VertexID" t)
     (gl-instance-id :int "gl_InstanceID" t)
     (gl-color :vec4 "gl_Color" t)
     (gl-secondary-color :vec4 "gl_SecondaryColor" t)
     (gl-normal :vec3 "gl_Normal" t)
     (gl-vertex :vec4 "gl_Vertex" t)
     (gl-multi-tex-coord-0 :vec4 "gl_MultiTexCoord0" t)
     (gl-multi-tex-coord-1 :vec4 "gl_MultiTexCoord1" t)
     (gl-multi-tex-coord-2 :vec4 "gl_MultiTexCoord2" t)
     (gl-multi-tex-coord-3 :vec4 "gl_MultiTexCoord3" t)
     (gl-multi-tex-coord-4 :vec4 "gl_MultiTexCoord4" t)
     (gl-multi-tex-coord-5 :vec4 "gl_MultiTexCoord5" t)
     (gl-multi-tex-coord-6 :vec4 "gl_MultiTexCoord6" t)
     (gl-multi-tex-coord-7 :vec4 "gl_MultiTexCoord7" t)
     (gl-fog-coord :float "gl_FogCoord" t)
     (gl-position :vec4 "gl_Position")
     (gl-point-size :float "gl_PointSize")
     (gl-clip-distance (:float t) "gl_ClipDistance")
     (gl-clip-vertex :vec4 "gl_ClipVertex")
     (gl-front-color :vec4 "gl_FrontColor")
     (gl-back-color :vec4 "gl_BackColor")
     (gl-front-secondary-color :vec4 "gl_FrontSecondaryColor")
     (gl-back-secondary-color :vec4 "gl_FrontSecondaryColor")
     (gl-tex-coord (:vec4 t) "gl_TexCoord")
     (gl-fog-frag-coord :float "gl_FogFragCoord"))
    (:fragment 
     (gl-frag-coord :vec4 "gl_FragCoord" t)
     (gl-front-facing :bool  "gl_FrontFacing" t)
     (gl-clip-distance (:float t) "gl_ClipDistance" t)
     (gl-point-coord :vec2  "gl_PointCoord" t)
     (gl-primitive-id :int "gl_PrimitiveID" t)
     (gl-frag-depth :float "gl_FragDepth" nil))
    (:geometry
     (gl-in (vgl-per-vertex-g t) "gl_in" t)
     (gl-primitive-id-in :int "gl_PrimitiveIDIn" t)
     (gl-position :vec4 "gl_Position")
     (gl-point-size :float "gl_PointSize")
     (gl-clip-distance (:float t) "gl_ClipDistance")
     (gl-primitive-id :int "gl_PrimitiveID")
     (gl-layer :int "gl_Layer"))))

(defparameter *glsl-variables* nil)
(defparameter *glsl-functions* nil)
(defparameter *glsl-special-functions* nil)
(defparameter *glsl-substitutions* nil)
(defparameter *shader-context* nil)

;;------------------------------------------------------------
;; Handy Functions
;;-----------------

;; [TODO] need assoc test... I dont thing so
(defun acons-many (data a-list)
  (if data (let* ((func (first data))
                  (name (first func))
                  (body (second func)))
             (acons name (cons body (rest (assoc name a-list)))
                    (acons-many (rest data) a-list)))
      a-list))

(defun kwd (&rest args) 
  (intern (format nil "~{~a~}" args) 'keyword))

(defun fmt (control-string &rest format-args) 
  (apply #'format `(nil ,control-string ,@format-args)))

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

(defun symbol-name-equal (a b)
  (when (and (symbolp a) (symbolp b))
    (equal (symbol-name a) (symbol-name b))))

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
  (defun glsl-gensym (&optional (name 'var))
    (setf count (+ 1 count))
    (let ((safe-name (safe-gl-name name)))
      (format nil "_~a_~a" safe-name count))))

(defun safe-gl-name (&rest name-parts)
  (let* ((n (string-downcase (string (apply #'symb name-parts))))
         (matches (cl-ppcre:all-matches "[^a-zA-Z0-9-]" n)))
    (if (or matches
            (not (or (< (length n) 2) (not (equal "gl" (subseq n 0 2))))))
        (error "Varjo: Names of variables and functions must be only contain~%alpha-numeric characters and the hyphen character (-).~%They also may not start with 'gl'~%~a" n)
        (cl-ppcre:regex-replace-all "[-]" n "_"))))

(defun symbol-name-position (symbol list)
  (let ((symb-name (string-upcase symbol)))
    (position-if #'(lambda (x) (when (symbolp x) (equal (symbol-name x) symb-name))) list)))

(defmacro assocr (item alist &key (key nil keyp) 
			       (test nil testp) 
                               (test-not nil notp))
  `(cdr (assoc ,item ,alist 
	       ,@(when keyp (list :key key)) 
	       ,@(when testp (list :test test))
               ,@(when notp (list test-not)))))

(defun lists-contain-duplicates-p (&rest lists)
  (let ((joined (apply #'append lists)))
    (not (eq (length joined) (length (remove-duplicates joined))))))
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

(defun flesh-out-type (type-spec)
  (if (consp type-spec)
      (if (> (length type-spec) 4)
          (error "Invalid GLSL Type Definition: ~s has more than 4 components." type-spec)
          (list (type-principle type-spec)
                (type-array-length type-spec)
                (type-place type-spec)
                (or (type-gl-name type-spec)
                    (when (symbolp (type-principle type-spec))
                      (safe-gl-name (type-principle type-spec))))))
      (flesh-out-type (list type-spec))))

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

;;-----------
(defun type-principle (type)
  (first type))

(defun type-arrayp (type)
  (not (null (second type))))

(defun type-array-length (type)
  (second type))

(defun type-place (type)
  (third type))

(defun type-placep (type)
  (third type))

(defun type-gl-name (type)
  (fourth type))

(defun type-built-inp (type)
  (not (null (assoc (type-principle type) *built-in-types*))))

(defun built-in-vars (context)
  (loop :for part :in context
     :append (assocr part *built-in-vars*
		     :test #'symbol-name-equal)))
;;-----------

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

(defun type-aggregate-p (type-spec)
  (let* ((full-type (flesh-out-type type-spec))
         (type (type-principle full-type))
         (length (rest (assoc type *glsl-component-counts*))))
    (when length t)))

(defun type-component-count (type-spec)
  (let* ((full-type (flesh-out-type type-spec))
         (type (type-principle full-type))
         (length (assocr type *glsl-component-counts*)))
    (if length
        length
        (error "Type '~a' is not a vector or matrix componant type" full-type))))

(defun type-component-type (type)
  (let* ((ftype (flesh-out-type type))
         (ptype (type-principle ftype)))
    (if (type-arrayp ftype) 
        ptype
        (if (assocr ptype *glsl-component-type*)
            (assocr ptype *glsl-component-type*)
            (error "Type '~s' is not a vector or matrix componant type" 
                   ptype)))))

(defun type-glsl-size (type-spec)
  (let* ((full-type (flesh-out-type type-spec))
         (type (type-principle full-type))
         (length (rest (assoc type *glsl-type-sizes*))))
    (if length
        length
        (error "Type '~a' is not a vector or matrix componant type" type))))

(defun mat-typep (mat-type)
  (equal "MAT" (subseq (symbol-name (type-principle (flesh-out-type mat-type))) 0 3)))

(defun mat/vec-length (mat-type)
  (let ((name (symbol-name (type-principle (flesh-out-type mat-type)))))
    (parse-integer (string (elt name (1- (length name)))))))

(defun vec-typep (mat-type)
  (let ((name (symbol-name (type-principle (flesh-out-type mat-type)))))
    (equal "VEC" (subseq name (max 0 (- (length name) 4)) (1- (length name))))))

(defun type-mat-col-to-vec (mat-type)
  (kwd (format nil "VEC~a" 
               (elt (symbol-name 
                     (type-principle (flesh-out-type mat-type))) 
                    3))))

(defun type-vec-core-type (vec-type)
  (let* ((name (symbol-name (type-principle (flesh-out-type vec-type))))
         (id (elt name 0)))
    (cond ((eq id #\V) :float)
          ((eq id #\I) :int)
          ((eq id #\U) :uint)
          (t (error "unknown vector type")))))

(defun change-vec-length (vec-type length)
  (if (type-arrayp vec-type)
      (error "Varjo: Expected vector, got vector array")
      (let* ((type (string (type-principle vec-type)))
             (len (length type))
             (base (subseq type 0 (1- len))))
        (flesh-out-type (kwd base length)))))

;;------------------------------------------------------------
;; GLSL Functions
;;----------------

(defun vlambda (&key in-args output-type transform
                  context-restriction (packageless t))
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

(defun func-packageless (x)
  (seventh x))

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
         (made-type
          (loop :for i in (func-out-spec func)
             :for part from 0
             :collect 
             (if (numberp i)
                 (nth part (if (nth i (func-compatible-args
                                       func))
                               superior
                               (nth i in-types)))
                 (if (consp i) (first i) i))))
         (final-type (flesh-out-type made-type)))
    ;; (print in-types)
    ;; (print (func-out-spec func))
    ;; (print (func-compatible-args func))
    ;; (print made-type)
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

(defun var-restriction (var)
  (fifth var))

(defun compile-let-forms (let-forms &optional (typify t) 
                                      (gensym-vars t))
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
           (var-gl-names (if gensym-vars
                             (mapcar #'glsl-gensym var-names)
                             (mapcar #'safe-gl-name var-names)))
           (var-types (loop :for form :in let-forms
                         :for obj :in val-objs
                         :collect (or (var-type form)
                                      (when obj 
                                        (code-type obj))))))
      ;; THe above can be nil when the val half __^^^^^^
      ;; of the let form is left blank
      (list (mapcar #'compile-form var-gl-names var-types val-objs)
            (mapcar #'list var-names var-types var-gl-names)))))

(defun var-existsp (name)
  (not (null (assoc name *glsl-variables*
		    :test #'symbol-name-equal))))

;;probably redundant
(defmacro add-vars ((var-declarations &optional (shadowing t))
                    &body body)
  
  `(if (or ,shadowing 
           (notany #'(lambda (var) (var-existsp (var-name var)))
                   ,var-declarations))
       (let ((*glsl-variables* (append ,var-declarations 
                                       *glsl-variables*)))
         ,@body)
       (error "Variable already defined and cannot be shadowed.~%~a~%~a" ,var-declarations *glsl-variables*)))

;;------------------------------------------------------------
;; GLSL Structs
;;--------------

(defun struct-init-form (struct)
  (let* ((struct-name (first struct))
         (slots (rest struct)))
    (format nil "struct ~(~a~) {~%~{~a~%~}};"
            struct-name (mapcar #'compile-struct-type slots))))

(defun compile-struct-type (slot)
  (let ((name (safe-gl-name (or (third slot) (first slot))))
        (type (flesh-out-type (second slot))))
    (let ((principle (varjo-type->glsl-type (first type)))
          (len (third type)))
      (if len
          (format nil "    ~a ~a[~a];" 
                  principle name len)
          (format nil "    ~a ~a;" 
                  principle name)))))

(defun type-struct-p (type)
  (let ((ftype (flesh-out-type type)))
    (not (null (assoc (type-principle ftype) *struct-definitions*)))))

(defun struct-definition (type-name)
  (let ((descrip (assoc type-name *struct-definitions*)))
    (or (rest descrip) 
        (error "Varjo: Struct ~a does not exist" type-name))))

(defun get-struct-definitions (types)
  (if (not types) 
      (error "Varjo: get-struct-definitions called with no types")

      (let* ((found (loop for type in types 
                       :collect (assoc type
                                       *struct-definitions*)))
             (error-pos (position-if #'null found)))
        (if (not error-pos)
            found
            (error "Varjo: Struct ~a does not exist" 
                   (nth error-pos types))))))

(defun fake-struct-vars (var-name struct-name)
  (let ((slots (rest (first (get-struct-definitions 
                             (list struct-name))))))
    (loop for slot in slots
       :collect `(,(symb '-f- var-name '- (var-name slot))
		  ,(flesh-out-type (var-type slot))
		  ,(safe-gl-name '-f- var-name '- (var-name slot))))))

(defun make-fake-struct (struct-name)
  (let ((fake-type (symb '-f- struct-name))
        (slots (rest (first (get-struct-definitions 
                             (list struct-name))))))    
    (list
     (list struct-name fake-type)
     (loop :for slot :in slots 
        :collect
        (list (or (fifth slot) (symb struct-name '- (first slot)))
              (vlambda :in-args `((x (,fake-type)))
                       :output-type
                       (literal-number-output-type
                        (set-place-t 
                         (flesh-out-type 
                          (second slot))))
                       :transform (format nil "_f_~~(~~a_~a~~)" 
                                          (safe-gl-name (first slot)))))))))

(defun literal-number-output-type (type)
  (loop for i in type :collect (if (numberp i) (list i) i)))

(defun struct-funcs (struct)
  (%struct-funcs (first struct) nil nil 
		 (loop for slot in (rest struct)
		       collect (list (safe-gl-name (first slot))
				     (second slot)))))

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
      (list (or (fifth slot)
                (symb (or slot-prefix name) '- (first slot)))
            (vlambda :in-args `((x (,name)))
                     :output-type 
                     (literal-number-output-type
                      (set-place-t (flesh-out-type 
                                    (second slot))))
                     :transform (format nil "~~a.~a" 
					(or (third slot) (first slot)))
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

;;------------------------------------------------------------

(defun varjo->glsl (varjo-code)
  (cond ((null varjo-code) nil)
        ((typep varjo-code 'code) varjo-code)
        ((atom varjo-code) 
         (if (assoc varjo-code *glsl-variables*
		    :test #'symbol-name-equal)
             (instance-var varjo-code)
             (error "Varjo: '~s' is unidentified." varjo-code)))
        ((special-functionp (first varjo-code)) 
         (apply-special (first varjo-code) (rest varjo-code)))
        ((vfunctionp (first varjo-code))
         (compile-function (first varjo-code) (rest varjo-code)))
        (t (error "Function '~s' is not available for ~A shaders in varjo." (first varjo-code) *shader-context*))))


(defun compile-function (func-name args)
  (let ((func-specs (func-specs func-name))
        (arg-objs (mapcar #'varjo->glsl args)))
    (loop :for f-spec :in func-specs 
       :if (glsl-valid-function-args f-spec arg-objs )
       :return (merge-obs arg-objs
                          :type (glsl-resolve-func-type f-spec arg-objs)
                          :current-line (apply #'format 
                                               (append 
                                                (list nil (func-body f-spec))
                                                (mapcar #'current-line
                                                        arg-objs))))
       :finally (error "There is no applicable method for the glsl function '~s'~%when called with argument types:~%~s " func-name (mapcar #'code-type arg-objs)))))

(defun macroexpand-and-substitute (varjo-code)
  (cond ((null varjo-code) nil)
        ((listp varjo-code) 
         (let ((sub (substitution (first varjo-code)))) 
           (if sub
               (mapcar #'macroexpand-and-substitute
                       (apply sub (rest varjo-code)))
               (mapcar #'macroexpand-and-substitute
                       varjo-code))))
        (t varjo-code)))

;; [TODO] How should we specify unsigned?
(defun replace-literals (varjo-code)
  (labels ((num-suffix (type)
	     (or (assocr (type-principle type) 
			 '((:float . "f") (:uint . "u")))
		 "")))
    (cond ((null varjo-code) nil)	 
	  ((eq t varjo-code) 
	   (make-instance 'code :current-line "true" 
				:type '(:bool nil)))
	  ((numberp varjo-code) 
	   (let ((num-type (get-number-type varjo-code)))
	     (make-instance 'code :current-line 
			    (format nil "~a~a" varjo-code
				    (num-suffix num-type))
				  :type num-type)))
	  ((listp varjo-code) (mapcar #'replace-literals
				      varjo-code))
	  (t varjo-code))))

(defun get-number-type (x)
  (cond ((floatp x) '(:float nil))
        ((integerp x) '(:int nil))
        (t (error "Varjo: Do not know the type of the number '~s'"
                  x))))

(defun compile-var (name type &rest qualifiers)
  (%compile-var name type qualifiers))

(defun %compile-var (name type &optional qualifiers)
  (%qualify (varjo->glsl `(%in-typify (%make-var
                                       ,name 
                                       ,(flesh-out-type type))))
            qualifiers))

(defun %qualify (obj qualifiers)
  (merge-obs obj :current-line (format nil "~(~{~a ~}~)~a" 
                                       qualifiers 
                                       (current-line obj))))

(defun qualify (obj &rest qualifiers)
  (%qualify obj qualifiers))

;;---------------------------------------------------------------

(defun glsl-defun (&key name in-args output-type
                     transform context-restriction)
  (let ((*types* *built-in-types*))
    (let* ((func-spec (vlambda :in-args in-args 
                               :output-type output-type
                               :transform transform
                               :context-restriction 
                               context-restriction)))
      (setf *glsl-functions*
            (acons name (cons func-spec
                              (assocr name *glsl-functions*
				      :test #'symbol-name-equal))
                   *glsl-functions*)))))

(defun get-vars-for-context (context)
  (loop for item in context
     :append (assocr item *built-in-vars*
		     :test #'symbol-name-equal)))


(defun func-valid-for-contextp (context func)
  (let ((restriction (func-restriction func)))
    (if restriction
        (when (every #'identity
                     (loop for item in restriction
                        :collect (find item context)))
          func)
        func)))

(defun func-specs (name)
  (let ((all-matching (assocr name *glsl-functions* 
			      :test #'symbol-name-equal)))
    (remove-if 
     #'null (loop for spec in all-matching
               :collect (func-valid-for-contextp 
                         *shader-context* spec)))))

(defun vfunctionp (name)
  (not (null (func-specs name))))

(defun special-functionp (symbol)
  (not (null (assoc symbol *glsl-special-functions*
		    :test #'symbol-name-equal))))

(defun apply-special (symbol arg-objs)
  (if (special-functionp symbol)
      (apply (assocr symbol *glsl-special-functions*
		     :test #'symbol-name-equal) arg-objs)
      (error "Varjo: '~a' is not a special function" symbol)))

(defun register-special-function (symbol function)
  (setf *glsl-special-functions* 
        (cons (cons symbol function) *glsl-special-functions*)))

(defmacro vdefspecial (name args &body body)
  `(register-special-function ',name #'(lambda ,args ,@body)))

(defun register-substitution (symbol function 
			      &optional (packageless nil))
  (setf *glsl-substitutions*
        (acons symbol (list function packageless)
	       *glsl-substitutions*)))

(defun substitutionp (symbol)
  (let ((sub (assoc symbol *glsl-substitutions*
		     :test #'symbol-name-equal)))
    (and (not (null sub))
	 (or (third sub) (eq symbol (first sub))))))

(defun substitution (symbol)
  (when (substitutionp symbol)
    (first (assocr symbol *glsl-substitutions*
		   :test #'symbol-name-equal))))

(defmacro vdefmacro (name lambda-list &body body)
  `(register-substitution
    ',name
    (lambda ,lambda-list
      ,@body)))

(defmacro %vdefmacro (name packageless lambda-list &body body)
  `(register-substitution
    ',name
    (lambda ,lambda-list
      ,@body)
    ,packageless))

(defun varjo-type->glsl-type (type-spec)
  (let* ((type (flesh-out-type type-spec))
         (type-name (or (type-gl-name type) (type-principle type)))
         (len (second type)))
    (if len
        (format nil "~a[~a]" type-name (if (numberp len) len ""))
        (format nil "~a" type-name))))

(defun instance-var (symbol)
  (let ((var-spec (assoc symbol *glsl-variables*
			 :test #'symbol-name-equal)))
    (make-instance 'code
                   :type (let ((new-type (flesh-out-type (var-type var-spec))))
                           (if (var-read-only var-spec)
                               new-type
                               (set-place-t new-type)))
                   :current-line (format nil "~a" 
                                         (or (var-gl-name var-spec)
                                             (var-name var-spec))))))


(defgeneric indent (input))

(defmethod indent ((input string))
  (mapcar #'(lambda (x) (format nil "    ~a" x))
          (split-sequence:split-sequence #\newline input)))

(defmethod indent ((input list))
  (mapcan #'indent input))

(defun indent-ob (code-obj)
  (merge-obs code-obj
             :to-block (indent (to-block code-obj))))

(defun make-none-ob ()
  (make-instance 'code :type :none
                 :current-line ""))

(defun end-line (ob)
  (if (find (type-principle (code-type ob)) '(:none :void))
      ob
      (merge-obs ob :current-line (format nil "~a;" (current-line ob)))))


