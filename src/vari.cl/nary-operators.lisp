(in-package :vari.cl)

;;------------------------------------------------------------
;; Devil magic that makes operators work on any length

(v-def-glsl-template-fun = (a) "true" (t) v-bool :pure t)

(v-def-glsl-template-fun = (a b c &rest d) "(~a £==£ ~a £==£ ~a ~{ £==£ ~a~})"
                         (t t t &rest t) t :pure t)

(v-define-compiler-macro = ((a t) (b t) (c t) &rest (d t))
  (let ((ga (gensym "a")))
    `(let ((,ga ,a))
       (and (= ,ga ,b)
            (= ,ga ,c)
            ,@(loop :for x :in d :collect
                 `(= ,ga ,x))))))

;;------------------------------------------------------------

(v-def-glsl-template-fun * (a b c &rest d) "(~a £*£ ~a £*£ ~a ~{ £*£ ~a~})"
                         (t t t &rest t) t :pure t)

(v-define-compiler-macro * ((a t) (b t) (c t) &rest (d t))
  `(* ,a (* ,b (* ,c ,@d))))

(v-def-glsl-template-fun / (a b c &rest d) "(~a £/£ ~a £/£ ~a ~{ £/£ ~a~})"
                         (t t t &rest t) t :pure t)

(v-define-compiler-macro / ((a t) (b t) (c t) &rest (d t))
  `(/ ,a (/ ,b (/ ,c ,@d))))

(v-def-glsl-template-fun + (a b c &rest d) "(~a £+£ ~a £+£ ~a ~{ £+£ ~a~})"
                         (t t t &rest t) t :pure t)

(v-define-compiler-macro + ((a t) (b t) (c t) &rest (d t))
  `(+ ,a (+ ,b (+ ,c ,@d))))

(v-def-glsl-template-fun - (a b c &rest d) "(~a £-£ ~a £-£ ~a ~{ £-£ ~a~})"
                         (t t t &rest t) t :pure t)

(v-define-compiler-macro - ((a t) (b t) (c t) &rest (d t))
  `(- ,a (- ,b (- ,c ,@d))))

(v-def-glsl-template-fun + () "0" () :int :pure t)
(v-def-glsl-template-fun * () "1" () :int :pure t)

(v-def-glsl-template-fun * (a) "~a" (v-double) v-double :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-short-float) v-short-float :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-float) v-float :pure t)
(v-def-glsl-template-fun * (a) "~a" (:int) :int :pure t)
(v-def-glsl-template-fun * (a) "~a" (:uint) :uint :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-rational) v-rational :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-dvec4) v-dvec4 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-dvec3) v-dvec3 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-dvec2) v-dvec2 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-ivec4) v-ivec4 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-ivec3) v-ivec3 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-ivec2) v-ivec2 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-uvec4) v-uvec4 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-uvec3) v-uvec3 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-uvec2) v-uvec2 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-bvec4) v-bvec4 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-bvec3) v-bvec3 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-bvec2) v-bvec2 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-vec4) v-vec4 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-vec3) v-vec3 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-vec2) v-vec2 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-mat4x4) v-mat4x4 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-mat4x3) v-mat4x3 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-mat4x2) v-mat4x2 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-mat3x4) v-mat3x4 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-mat3x3) v-mat3x3 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-mat3x2) v-mat3x2 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-mat2x4) v-mat2x4 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-mat2x3) v-mat2x3 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-mat2x2) v-mat2x2 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-mat4) v-mat4 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-mat3) v-mat3 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-mat2) v-mat2 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-dmat4x4) v-dmat4x4 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-dmat4x3) v-dmat4x3 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-dmat4x2) v-dmat4x2 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-dmat3x4) v-dmat3x4 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-dmat3x3) v-dmat3x3 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-dmat3x2) v-dmat3x2 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-dmat2x4) v-dmat2x4 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-dmat2x3) v-dmat2x3 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-dmat2x2) v-dmat2x2 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-dmat4) v-dmat4 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-dmat3) v-dmat3 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-dmat2) v-dmat2 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-double) v-double :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-short-float) v-short-float :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-float) v-float :pure t)
(v-def-glsl-template-fun + (a) "~a" (:int) :int :pure t)
(v-def-glsl-template-fun + (a) "~a" (:uint) :uint :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-rational) v-rational :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-dvec4) v-dvec4 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-dvec3) v-dvec3 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-dvec2) v-dvec2 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-ivec4) v-ivec4 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-ivec3) v-ivec3 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-ivec2) v-ivec2 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-uvec4) v-uvec4 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-uvec3) v-uvec3 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-uvec2) v-uvec2 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-bvec4) v-bvec4 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-bvec3) v-bvec3 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-bvec2) v-bvec2 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-vec4) v-vec4 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-vec3) v-vec3 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-vec2) v-vec2 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-mat4x4) v-mat4x4 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-mat4x3) v-mat4x3 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-mat4x2) v-mat4x2 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-mat3x4) v-mat3x4 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-mat3x3) v-mat3x3 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-mat3x2) v-mat3x2 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-mat2x4) v-mat2x4 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-mat2x3) v-mat2x3 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-mat2x2) v-mat2x2 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-mat4) v-mat4 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-mat3) v-mat3 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-mat2) v-mat2 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-dmat4x4) v-dmat4x4 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-dmat4x3) v-dmat4x3 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-dmat4x2) v-dmat4x2 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-dmat3x4) v-dmat3x4 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-dmat3x3) v-dmat3x3 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-dmat3x2) v-dmat3x2 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-dmat2x4) v-dmat2x4 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-dmat2x3) v-dmat2x3 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-dmat2x2) v-dmat2x2 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-dmat4) v-dmat4 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-dmat3) v-dmat3 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-dmat2) v-dmat3 :pure t)

(v-def-glsl-template-fun / (a) "(1.0f / ~a)" (v-real) :float :pure t)
(v-def-glsl-template-fun / (a) "(1.0l / ~a)" (v-dvec4) v-dvec4 :pure t)
(v-def-glsl-template-fun / (a) "(1.0l / ~a)" (v-dvec3) v-dvec3 :pure t)
(v-def-glsl-template-fun / (a) "(1.0l / ~a)" (v-dvec2) v-dvec2 :pure t)
(v-def-glsl-template-fun / (a) "(1 / ~a)" (v-ivec4) v-ivec4 :pure t)
(v-def-glsl-template-fun / (a) "(1 / ~a)" (v-ivec3) v-ivec3 :pure t)
(v-def-glsl-template-fun / (a) "(1 / ~a)" (v-ivec2) v-ivec2 :pure t)
(v-def-glsl-template-fun / (a) "(1 / ~a)" (v-uvec4) v-uvec4 :pure t)
(v-def-glsl-template-fun / (a) "(1 / ~a)" (v-uvec3) v-uvec3 :pure t)
(v-def-glsl-template-fun / (a) "(1 / ~a)" (v-uvec2) v-uvec2 :pure t)
(v-def-glsl-template-fun / (a) "(1.0 / ~a)" (v-vec4) v-vec4 :pure t)
(v-def-glsl-template-fun / (a) "(1.0 / ~a)" (v-vec3) v-vec3 :pure t)
(v-def-glsl-template-fun / (a) "(1.0 / ~a)" (v-vec2) v-vec2 :pure t)

;;------------------------------------------------------------

(v-def-glsl-template-fun max (a b c &rest d) "£max£(~a, £max£(~a, £max£(~a~{ ,~a~})"
                         (t t t &rest t) t :pure t)

(v-define-compiler-macro max ((a t) (b t) (c t) &rest (d t))
  `(max ,a (max ,b (max ,c ,@d))))

(v-def-glsl-template-fun min (a b c &rest d) "£min£(~a, £min£(~a, £min£(~a~{ ,~a~})"
                         (t t t &rest t) t :pure t)

(v-define-compiler-macro min ((a t) (b t) (c t) &rest (d t))
  `(min ,a (min ,b (min ,c ,@d))))

(v-def-glsl-template-fun max (a) "~a" (:float) :float :pure t)
(v-def-glsl-template-fun max (a) "~a" (:vec2) :vec2 :pure t)
(v-def-glsl-template-fun max (a) "~a" (:vec3) :vec3 :pure t)
(v-def-glsl-template-fun max (a) "~a" (:vec4) :vec4 :pure t)

(v-def-glsl-template-fun max (a) "~a" (:int) :int :pure t)
(v-def-glsl-template-fun max (a) "~a" (:ivec2) :ivec2 :pure t)
(v-def-glsl-template-fun max (a) "~a" (:ivec3) :ivec3 :pure t)
(v-def-glsl-template-fun max (a) "~a" (:ivec4) :ivec4 :pure t)

(v-def-glsl-template-fun max (a) "~a" (:uint) :uint :pure t)
(v-def-glsl-template-fun max (a) "~a" (:uvec2) :uvec2 :pure t)
(v-def-glsl-template-fun max (a) "~a" (:uvec3) :uvec3 :pure t)
(v-def-glsl-template-fun max (a) "~a" (:uvec4) :uvec4 :pure t)

(v-def-glsl-template-fun min (a) "~a" (:float) :float :pure t)
(v-def-glsl-template-fun min (a) "~a" (:vec2) :vec2 :pure t)
(v-def-glsl-template-fun min (a) "~a" (:vec3) :vec3 :pure t)
(v-def-glsl-template-fun min (a) "~a" (:vec4) :vec4 :pure t)

(v-def-glsl-template-fun min (a) "~a" (:int) :int :pure t)
(v-def-glsl-template-fun min (a) "~a" (:ivec2) :ivec2 :pure t)
(v-def-glsl-template-fun min (a) "~a" (:ivec3) :ivec3 :pure t)
(v-def-glsl-template-fun min (a) "~a" (:ivec4) :ivec4 :pure t)

(v-def-glsl-template-fun min (a) "~a" (:uint) :uint :pure t)
(v-def-glsl-template-fun min (a) "~a" (:uvec2) :uvec2 :pure t)
(v-def-glsl-template-fun min (a) "~a" (:uvec3) :uvec3 :pure t)
(v-def-glsl-template-fun min (a) "~a" (:uvec4) :uvec4 :pure t)

;;------------------------------------------------------------
