(in-package :varjo)

(defun kwd (&rest args)
  (intern (format nil "~{~a~}" args) :keyword))

(defun symb (&rest args)
  (intern (format nil "~{~a~}" args)))

(defun symb-package (package &rest args)
  (intern (format nil "~{~a~}" args) package))

(defmacro string-case (form &body forms)
  (let ((g (gensym "case")))
    `(let ((,g ,form))
       (cond
	 ,@(loop :for (match . body) :in forms :collect
	      (if (eq match 'otherwise)
		  `(t ,@body)
		  `((string= ,g ,match) ,@body)))))))

(defun it (elem)
  (destructuring-bind (&key name return args versions) elem
    (let* ((lisp-name (cffi:translate-camelcase-name name))
	   ;; (arg-names (mapcar #'cffi:translate-camelcase-name
	   ;; 		     (mapcar #'first args)))
	   (arg-types (mapcar #'second args))
	   (lisp-arg-types (mapcar #'translate-type arg-types))
	   (lisp-return (translate-type return))
	   (transform (format nil "~a(~{~a~^,~})" name
			      (loop :for i :below (length args) :collect
				 "~a")))
	   (lisp-return (if (member return generic-types :test #'equal)
			    (or (position return arg-types :test #'equal)
				lisp-return)
			    lisp-return)))
      ;;(print (list lisp-name lisp-return lisp-arg-types))
      (add-function
       lisp-name
       (v-make-f-spec lisp-name transform versions lisp-arg-types lisp-return
		      :v-place-index nil :glsl-name nil :flow-ids (%gl-flow-id!)
		      :in-arg-flow-ids (n-of (%gl-flow-id!) (length args)))
       *global-env*))))

(defun varg (elem)
  (destructuring-bind (&key name type place-p versions stage) elem
    (let ((lisp-name (cffi:translate-underscore-separated-name
		      (symbol-name (cffi:translate-camelcase-name name)))))
      (print lisp-name)
      `(blah blah))))

(defun translate-type (x)
  (string-case x
    ("mat" 'v-matrix)
    ("vec" 'v-vector)
    ("int" 'v-int)
    ("bool" 'v-bool)
    ("dmat" 'v-dmatrix)
    ("ivec" 'v-ivector)
    ("bvec" 'v-bvector)
    ("uvec" 'v-uvector)
    ("vec3" 'v-vec3)
    ("mat2" 'v-mat2)
    ("mat3" 'v-mat3)
    ("mat4" 'v-mat4)
    ("void" 'v-void)
    ("vec2" 'v-vec2)
    ("vec4" 'v-vec4)
    ("uint" 'v-uint)
    ("dvec2" 'v-dvec2)
    ("dvec3" 'v-dvec3)
    ("dvec4" 'v-dvec4)
    ("gvec4" 'v-gvec4)
    ("float" 'v-float)
    ("ivec2" 'v-ivec2)
    ("ivec3" 'v-ivec3)
    ("dmat2" 'v-dmat2)
    ("dmat3" 'v-dmat3)
    ("dmat4" 'v-dmat4)
    ("uvec2" 'v-uvec2)
    ("mat3x2" 'v-mat3x2)
    ("mat2x3" 'v-mat2x3)
    ("mat4x2" 'v-mat4x2)
    ("mat2x4" 'v-mat2x4)
    ("mat4x3" 'v-mat4x3)
    ("mat3x4" 'v-mat3x4)
    ("double" 'v-double)
    ("dmat3x2" 'v-dmat3x2)
    ("dmat2x3" 'v-dmat2x3)
    ("dmat4x2" 'v-dmat4x2)
    ("dmat2x4" 'v-dmat2x4)
    ("dmat4x3" 'v-dmat4x3)
    ("dmat3x4" 'v-dmat3x4)
    ("genType" 'v-tf)
    ("gimage1D" 'v-gimage-1d)
    ("gimage2D" 'v-gimage-2d)
    ("gimage3D" 'v-gimage-3d)
    ("genBType" 'v-tb)
    ("genDType" 'v-td)
    ("genIType" 'v-ti)
    ("genUType" 'v-tu)
    ("gimageRect" 'v-gimage-rect)
    ("gimageCube" 'v-gimage-cube)
    ("gimage2DMS" 'v-gimage-2d-ms)
    ("atomic_uint" 'v-atomic-uint)
    ("gimageBuffer" 'v-gimage-buffer)
    ("gimage2DRect" 'v-gimage-2d-rect)
    ("gimage1DArray" 'v-gimage-1d-array)
    ("gimage2DArray" 'v-gimage-2d-array)
    ("gimageCubeArray" 'v-gimage-cube-array)
    ("gimage2DMSArray" 'v-gimage-2d-ms-array)
    ("sampler1DShadow" 'v-sampler-1d-shadow)
    ("sampler2DShadow" 'v-sampler-2d-shadow)
    ("gsampler1DArray" 'v-gsampler-1d-array)
    ("gsampler2DArray" 'v-gsampler-2d-array)
    ("gsampler1DShadow" 'v-gsampler-1d-shadow)
    ("gsampler2DShadow" 'v-gsampler-2d-shadow)
    ("samplerCubeArray" 'v-sampler-cube-array)
    ("gsamplerCubeArray" 'v-gsampler-cube-array)
    ("samplerCubeShadow" 'v-sampler-cube-shadow)
    ("gsampler2DMSArray" 'v-gsampler-2d-ms-array)
    ("gsamplerCubeShadow" 'v-gsampler-cube-shadow)
    ("gsamplerRectShadow" 'v-gsampler-rect-shadow)
    ("sampler2DRectShadow" 'v-sampler-2d-rect-shadow)
    ("gsampler2DRectShadow" 'v-gsampler-rect-shadow)
    ("sampler1DArrayShadow" 'v-sampler-1d-array-shadow)
    ("sampler2DArrayShadow" 'v-sampler-2d-array-shadow)
    ("gsampler1DArrayShadow" 'v-gsampler-1d-array-shadow)
    ("gsampler2DArrayShadow" 'v-gsampler-2d-array-shadow)
    ("samplerCubeArrayShadow" 'v-sampler-cube-array-shadow)
    ("gsamplerCubeArrayShadow" 'v-gsampler-cube-array-shadow)
    ("gbufferImage" 'v-gbuffer-image)
    ("gsampler1D" 'v-gsampler-1d)
    ("gsampler2D" 'v-gsampler-2d)
    ("gsampler3D" 'v-gsampler-3d)
    ("gsamplerCube" 'v-gsampler-cube)
    ("gsamplerRect" 'v-gsampler-rect)
    ("gsampler2DMS" 'v-gsampler-2d-ms)
    ("gsampler2DRect" 'v-gsampler-2d-rect)
    ("gsamplerBuffer" 'v-gsampler-buffer)
    ("gl_DepthRangeParameters" 'GL-DEPTH-RANGE-PARAMETERS)
    (otherwise
     (if (char= (aref x 0) #\[)
	 `(,(translate-type (subseq x 1)) *)))))

(defparameter types
  '("mat" "vec" "int" "bool" "dmat" "ivec" "bvec" "uvec"
    "vec3" "mat2" "mat3" "mat4" "void" "vec2" "vec4" "uint" "dvec2"
    "dvec3" "dvec4" "gvec4" "float" "ivec2" "ivec3" "dmat2" "dmat3"
    "dmat4" "uvec2" "mat3x2" "mat2x3" "mat4x2" "mat2x4" "mat4x3" "mat3x4"
    "double" "dmat3x2" "dmat2x3" "dmat4x2" "dmat2x4" "dmat4x3" "dmat3x4"
    "genType" "gimage1D" "gimage2D" "gimage3D" "genBType" "genDType"
    "genIType" "genUType" "gimageRect" "gimageCube" "gimage2DMS"
    "gsampler1D" "gsampler2D" "gsampler3D" "atomic_uint" "gimageBuffer"
    "gimage2DRect" "gbufferImage" "gsamplerCube" "gsamplerRect"
    "gsampler2DMS" "gimage1DArray" "gimage2DArray" "gsampler2DRect"
    "gsamplerBuffer" "gimageCubeArray" "gimage2DMSArray"
    "sampler1DShadow" "sampler2DShadow" "gsampler1DArray"
    "gsampler2DArray" "gsampler2DDArray" "gsampler1DShadow"
    "gsampler2DShadow" "samplerCubeArray" "gsamplerCubeArray"
    "samplerCubeShadow" "gsampler2DMSArray" "gsamplerCubeShadow"
    "gsamplerRectShadow" "sampler2DRectShadow" "gsampler2DRectShadow"
    "sampler1DArrayShadow" "sampler2DArrayShadow" "gsampler1DArrayShadow"
    "gsampler2DArrayShadow" "samplerCubeArrayShadow"
    "gsamplerCubeArrayShadow"))

(defparameter generic-types
  '("gvec4" "genType" "gimage1D" "gimage2D" "gimage3D" "genBType"
    "genDType" "genIType" "genUType" "gimageRect" "gimageCube"
    "gimage2DMS" "gsampler1D" "gsampler2D" "gsampler3D" "gimageBuffer"
    "gimage2DRect" "gbufferImage" "gsamplerCube" "gsamplerRect"
    "gsampler2DMS" "gimage1DArray" "gimage2DArray" "gsampler2DRect"
    "gsamplerBuffer" "gimageCubeArray" "gimage2DMSArray"

    "gsampler1DArray" "gsampler2DArray" "gsampler2DDArray"
    "gsampler1DShadow" "gsampler2DShadow" "gsamplerCubeArray"
    "gsampler2DMSArray" "gsamplerCubeShadow" "gsamplerRectShadow"
    "gsampler2DRectShadow" "gsampler1DArrayShadow"
    "gsampler2DArrayShadow" "gsamplerCubeArrayShadow"
    "mat" "vec" "dmat" "ivec" "bvec" "uvec"))

;; take this ..
(closer-mop:class-direct-subclasses
 (find-class (translate-type "gvec4")))
;; ..and this..
(closer-mop:class-direct-subclasses
 (find-class (translate-type "gsampler2DRect")))
;; and use closer-mop to find the intersection of the element-types
;;
;; generate a new def for each subclass of the generic return type.
;;

(defun g-p (type-name)
  (not (null (member type-name generic-types :test #'equal))))

(defun subclasses-when-generic (type-name)
  (when (g-p type-name)
    (closer-mop:class-direct-subclasses
     (find-class (translate-type type-name)))))

(defun to-concrete (type-name)
  (let ((gg (mapcar #'class-name
		    (closer-mop:class-direct-subclasses
		     (find-class (if (stringp type-name)
				     (translate-type type-name)
				     type-name))))))
    (if gg
	(remove-duplicates (mapcat #'to-concrete gg))
	(list type-name))))

(defun flerg (f)
  (dbind (&key name return args versions) f
    (let* ((arg-types (mapcar #'second args))
	   (garg-types (remove-duplicates (remove-if-not #'g-p arg-types)
					  :test #'equal))
	   (the-gtype (first garg-types)))
      (when the-gtype '(t)))))
