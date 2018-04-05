(in-package :vari.cl)

;;------------------------------------------------------------
;; Devil magic that makes operators work on any length

(v-def-glsl-template-fun = (a) "true" (t) v-bool :pure t)

(v-def-glsl-template-fun = (a b c &rest c) "(~a £==£ ~a £==£ ~a ~{ £==£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(v-define-compiler-macro = ((a t) (b t) (c t) &rest (d t))
  (let ((ga (gensym "a")))
    `(let ((,ga ,a))
       (and (= ,ga ,b)
            (= ,ga ,c)
            ,@(loop :for x :in d :collect
                 `(= ,ga ,x))))))

;;------------------------------------------------------------

(v-def-glsl-template-fun * (a b c &rest c) "(~a £*£ ~a £*£ ~a ~{ £*£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(v-define-compiler-macro * ((a t) (b t) (c t) &rest (d t))
  `(* ,a (* ,b (* ,c ,@d))))

(v-def-glsl-template-fun / (a b c &rest c) "(~a £/£ ~a £/£ ~a ~{ £/£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(v-define-compiler-macro / ((a t) (b t) (c t) &rest (d t))
  `(/ ,a (/ ,b (/ ,c ,@d))))

(v-def-glsl-template-fun + (a b c &rest c) "(~a £+£ ~a £+£ ~a ~{ £+£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(v-define-compiler-macro + ((a t) (b t) (c t) &rest (d t))
  `(+ ,a (+ ,b (+ ,c ,@d))))

(v-def-glsl-template-fun - (a b c &rest c) "(~a £-£ ~a £-£ ~a ~{ £-£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(v-define-compiler-macro - ((a t) (b t) (c t) &rest (d t))
  `(- ,a (- ,b (- ,c ,@d))))

(v-def-glsl-template-fun + () "0" () :int :pure t)
(v-def-glsl-template-fun + (x &rest y) "(~a~{ + ~a~})" (v-number &rest v-number) nil :pure t)

(v-def-glsl-template-fun + (a) "~a" (v-real) 0 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-vector) 0 :pure t)

(v-def-glsl-template-fun * () "1" () :int :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-real) 0 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-vector) 0 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-matrix) 0 :pure t)

(v-def-glsl-template-fun / (a) "(1.0f / ~a)" (v-real) :float :pure t)
(v-def-glsl-template-fun / (a) "(1 / ~a)" (v-vector) 0 :pure t)

;;------------------------------------------------------------

(v-def-glsl-template-fun max (a b c &rest c) "max(~a, max(~a, max(~a~{ ,~a~})"
                         (t t t &rest t) 0 :pure t)

(v-define-compiler-macro max ((a t) (b t) (c t) &rest (d t))
  `(max ,a (max ,b (max ,c ,@d))))

(v-def-glsl-template-fun min (a b c &rest c) "min(~a, min(~a, min(~a~{ ,~a~})"
                         (t t t &rest t) 0 :pure t)

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
