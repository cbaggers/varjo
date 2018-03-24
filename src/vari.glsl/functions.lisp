(in-package :vari.glsl)

;;------------------------------------------------------------

(v-def-glsl-template-fun break () "break" () v-void)
(v-def-glsl-template-fun continue () "continue" () v-void)

(v-def-glsl-template-fun identity (x) "(~a)" (v-type) 0 :pure t)

(v-def-glsl-template-fun int (x) "~a" (v-int) v-int :pure t)
(v-def-glsl-template-fun uint (x) "~a" (v-uint) v-uint :pure t)
(v-def-glsl-template-fun bool (x) "~a" (v-bool) v-bool :pure t)
(v-def-glsl-template-fun float (x) "~a" (v-float) v-float :pure t)
(v-def-glsl-template-fun double (x) "~a" (v-double)  v-double :pure t)

(v-def-glsl-template-fun int (x) "int(~a)" (v-uint) v-int :pure t)
(v-def-glsl-template-fun int (x) "int(~a)" (v-bool) v-int :pure t)
(v-def-glsl-template-fun int (x) "int(~a)" (v-float) v-int :pure t)
(v-def-glsl-template-fun int (x) "int(~a)" (v-double)  v-int :pure t)
;; uint handled by special form
(v-def-glsl-template-fun bool (x) "bool(~a)" (v-int) v-bool :pure t)
(v-def-glsl-template-fun bool (x) "bool(~a)" (v-uint) v-bool :pure t)
(v-def-glsl-template-fun bool (x) "bool(~a)" (v-float)  v-bool :pure t)
(v-def-glsl-template-fun bool (x) "bool(~a)" (v-double)  v-bool :pure t)
(v-def-glsl-template-fun float (x) "float(~a)" (v-int) v-float :pure t)
(v-def-glsl-template-fun float (x) "float(~a)" (v-uint)  v-float :pure t)
(v-def-glsl-template-fun float (x) "float(~a)" (v-bool)  v-float :pure t)
(v-def-glsl-template-fun float (x) "float(~a)" (v-double) v-float :pure t)
(v-def-glsl-template-fun double (x) "double(~a)" (v-int)  v-double :pure t)
(v-def-glsl-template-fun double (x) "double(~a)" (v-uint)  v-double :pure t)
(v-def-glsl-template-fun double (x) "double(~a)" (v-bool)  v-double :pure t)
(v-def-glsl-template-fun double (x) "double(~a)" (v-float) v-double :pure t)

(v-def-glsl-template-fun < (a b &rest c) "(~a < ~a~{ < ~a~})" (v-real v-real &rest v-real) v-bool :pure t)
(v-def-glsl-template-fun > (a b &rest c) "(~a > ~a~{ > ~a~})" (v-real v-real &rest v-real) v-bool :pure t)
(v-def-glsl-template-fun <= (a b &rest c) "(~a <= ~a~{ <= ~a~})" (v-real v-real &rest v-real) v-bool :pure t)
(v-def-glsl-template-fun >= (a b &rest c) "(~a >= ~a~{ >= ~a~})" (v-real v-real &rest v-real) v-bool :pure t)

(v-def-glsl-template-fun = (a b) "(~a == ~a)" (v-real v-real) v-bool :pure t)
(v-def-glsl-template-fun = (a b) "(~a == ~a)" (v-vec2 v-vec2) v-bool :pure t)
(v-def-glsl-template-fun = (a b) "(~a == ~a)" (v-vec3 v-vec3) v-bool :pure t)
(v-def-glsl-template-fun = (a b) "(~a == ~a)" (v-vec4 v-vec4) v-bool :pure t)
(v-def-glsl-template-fun = (a b) "(~a == ~a)" (v-ivec2 v-ivec2) v-bool :pure t)
(v-def-glsl-template-fun = (a b) "(~a == ~a)" (v-ivec3 v-ivec3) v-bool :pure t)
(v-def-glsl-template-fun = (a b) "(~a == ~a)" (v-ivec4 v-ivec4) v-bool :pure t)
(v-def-glsl-template-fun = (a b) "(~a == ~a)" (v-uvec2 v-uvec2) v-bool :pure t)
(v-def-glsl-template-fun = (a b) "(~a == ~a)" (v-uvec3 v-uvec3) v-bool :pure t)
(v-def-glsl-template-fun = (a b) "(~a == ~a)" (v-uvec4 v-uvec4) v-bool :pure t)
(v-def-glsl-template-fun = (a b) "(~a == ~a)" (v-dvec2 v-dvec2) v-bool :pure t)
(v-def-glsl-template-fun = (a b) "(~a == ~a)" (v-dvec3 v-dvec3) v-bool :pure t)
(v-def-glsl-template-fun = (a b) "(~a == ~a)" (v-dvec4 v-dvec4) v-bool :pure t)
(v-def-glsl-template-fun = (a b) "(~a == ~a)" (v-mat2 v-mat2) v-bool :pure t)
(v-def-glsl-template-fun = (a b) "(~a == ~a)" (v-mat3 v-mat3) v-bool :pure t)
(v-def-glsl-template-fun = (a b) "(~a == ~a)" (v-mat4 v-mat4) v-bool :pure t)
(v-def-glsl-template-fun = (a b) "(~a == ~a)" (v-array v-array) v-bool :pure t)
(v-def-glsl-template-fun = (a) "true" (t) v-bool :pure t)

(v-def-glsl-template-fun ++ (a) "(++ ~a)" (v-real) nil)
(v-def-glsl-template-fun -- (a) "(-- ~a)" (v-real) nil)

;;------------------------------------------------------------
;; Devil magic that makes operators work on any length

(v-def-glsl-template-fun = (a b c &rest c) "(~a £==£ ~a £==£ ~a ~{ £==£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(v-define-compiler-macro = ((a t) (b t) (c t) &rest (d t))
  (let ((ga (gensym "a")))
    `(let ((,ga ,a))
       (and (= ,ga ,b)
            (= ,ga ,c)
            ,@(loop :for x :in d :collect
                 `(= ,ga ,x))))))

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

;;------------------------------------------------------------

(v-def-glsl-template-fun + () "0" () :int :pure t)
(v-def-glsl-template-fun + (x &rest y) "(~a~{ + ~a~})" (v-number &rest v-number) nil :pure t)

(v-def-glsl-template-fun + (a) "~a" (v-real) 0 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-vector) 0 :pure t)

(v-def-glsl-template-fun + (a b) "(~a + ~a)" (v-vec2 v-vec2) 0 :pure t)
(v-def-glsl-template-fun + (a b) "(~a + ~a)" (v-vec3 v-vec3) 0 :pure t)
(v-def-glsl-template-fun + (a b) "(~a + ~a)" (v-vec4 v-vec4) 0 :pure t)

(v-def-glsl-template-fun + (a b) "(~a + ~a)" (v-ivec2 v-ivec2) 0 :pure t)
(v-def-glsl-template-fun + (a b) "(~a + ~a)" (v-ivec3 v-ivec3) 0 :pure t)
(v-def-glsl-template-fun + (a b) "(~a + ~a)" (v-ivec4 v-ivec4) 0 :pure t)

(v-def-glsl-template-fun + (a b) "(~a + ~a)" (v-uvec2 v-uvec2) 0 :pure t)
(v-def-glsl-template-fun + (a b) "(~a + ~a)" (v-uvec3 v-uvec3) 0 :pure t)
(v-def-glsl-template-fun + (a b) "(~a + ~a)" (v-uvec4 v-uvec4) 0 :pure t)

(v-def-glsl-template-fun + (a b) "(~a + ~a)" (v-dvec2 v-dvec2) 0 :pure t)
(v-def-glsl-template-fun + (a b) "(~a + ~a)" (v-dvec3 v-dvec3) 0 :pure t)
(v-def-glsl-template-fun + (a b) "(~a + ~a)" (v-dvec4 v-dvec4) 0 :pure t)

(v-def-glsl-template-fun - (a) "(-~a)" (v-real) 0 :pure t)
(v-def-glsl-template-fun - (a) "(-~a)" (v-vec2) 0 :pure t)
(v-def-glsl-template-fun - (a) "(-~a)" (v-vec3) 0 :pure t)
(v-def-glsl-template-fun - (a) "(-~a)" (v-vec4) 0 :pure t)

(v-def-glsl-template-fun - (a b) "(~a - ~a)" (v-real v-real) nil :pure t)
(v-def-glsl-template-fun - (a b) "(~a - ~a)" (v-vec2 v-vec2) 0 :pure t)
(v-def-glsl-template-fun - (a b) "(~a - ~a)" (v-vec3 v-vec3) 0 :pure t)
(v-def-glsl-template-fun - (a b) "(~a - ~a)" (v-vec4 v-vec4) 0 :pure t)

(v-def-glsl-template-fun - (a b) "(~a - ~a)" (v-ivec2 v-ivec2) 0 :pure t)
(v-def-glsl-template-fun - (a b) "(~a - ~a)" (v-ivec3 v-ivec3) 0 :pure t)
(v-def-glsl-template-fun - (a b) "(~a - ~a)" (v-ivec4 v-ivec4) 0 :pure t)

(v-def-glsl-template-fun - (a b) "(~a - ~a)" (v-uvec2 v-uvec2) 0 :pure t)
(v-def-glsl-template-fun - (a b) "(~a - ~a)" (v-uvec3 v-uvec3) 0 :pure t)
(v-def-glsl-template-fun - (a b) "(~a - ~a)" (v-uvec4 v-uvec4) 0 :pure t)

(v-def-glsl-template-fun - (a b) "(~a - ~a)" (v-dvec2 v-dvec2) 0 :pure t)
(v-def-glsl-template-fun - (a b) "(~a - ~a)" (v-dvec3 v-dvec3) 0 :pure t)
(v-def-glsl-template-fun - (a b) "(~a - ~a)" (v-dvec4 v-dvec4) 0 :pure t)

(v-def-glsl-template-fun * () "1" () :int :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-real) 0 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-vector) 0 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-matrix) 0 :pure t)

(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-real v-real) nil :pure t)
(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-vec2 v-vec2) 0 :pure t)
(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-vec3 v-vec3) 0 :pure t)
(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-vec4 v-vec4) 0 :pure t)

(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-ivec2 v-ivec2) 0 :pure t)
(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-ivec3 v-ivec3) 0 :pure t)
(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-ivec4 v-ivec4) 0 :pure t)

(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-uvec2 v-uvec2) 0 :pure t)
(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-uvec3 v-uvec3) 0 :pure t)
(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-uvec4 v-uvec4) 0 :pure t)

(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-dvec2 v-dvec2) 0 :pure t)
(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-dvec3 v-dvec3) 0 :pure t)
(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-dvec4 v-dvec4) 0 :pure t)
(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-matrix v-matrix) nil :pure t)

(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-mat2 v-vec2) 1 :pure t)
(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-mat3 v-vec3) 1 :pure t)
(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-mat4 v-vec4) 1 :pure t)

(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-matrix v-real) 0 :pure t)
(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-vector v-real) 0 :pure t)

(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-float v-vec2) 1 :pure t)
(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-float v-vec3) 1 :pure t)
(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-float v-vec4) 1 :pure t)

(v-def-glsl-template-fun / (a) "(1.0f / ~a)" (v-real) :float :pure t)
(v-def-glsl-template-fun / (a) "(1 / ~a)" (v-vector) 0 :pure t)

(v-def-glsl-template-fun / (a b) "(~a / ~a)" (v-real v-real) nil :pure t)
(v-def-glsl-template-fun / (a b) "(~a / ~a)" (v-vector v-real) 0 :pure t)
(v-def-glsl-template-fun / (a b) "(~a / ~a)" (v-vec2 v-vec2) 0 :pure t)
(v-def-glsl-template-fun / (a b) "(~a / ~a)" (v-vec3 v-vec3) 0 :pure t)
(v-def-glsl-template-fun / (a b) "(~a / ~a)" (v-vec4 v-vec4) 0 :pure t)

(v-def-glsl-template-fun / (a b) "(~a / ~a)" (v-ivec2 v-ivec2) 0 :pure t)
(v-def-glsl-template-fun / (a b) "(~a / ~a)" (v-ivec3 v-ivec3) 0 :pure t)
(v-def-glsl-template-fun / (a b) "(~a / ~a)" (v-ivec4 v-ivec4) 0 :pure t)

(v-def-glsl-template-fun / (a b) "(~a / ~a)" (v-uvec2 v-uvec2) 0 :pure t)
(v-def-glsl-template-fun / (a b) "(~a / ~a)" (v-uvec3 v-uvec3) 0 :pure t)
(v-def-glsl-template-fun / (a b) "(~a / ~a)" (v-uvec4 v-uvec4) 0 :pure t)

(v-def-glsl-template-fun / (a b) "(~a / ~a)" (v-dvec2 v-dvec2) 0 :pure t)
(v-def-glsl-template-fun / (a b) "(~a / ~a)" (v-dvec3 v-dvec3) 0 :pure t)
(v-def-glsl-template-fun / (a b) "(~a / ~a)" (v-dvec4 v-dvec4) 0 :pure t)

(v-def-glsl-template-fun v-not (x &context (:330 :440)) "not(~a)"
         (v-bvector) 0 :pure t)

(v-def-glsl-template-fun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-bvector v-bvector)
  0 :pure t)
(v-def-glsl-template-fun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-float v-float)
  v-bool :pure t)
(v-def-glsl-template-fun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-vec2 v-vec2)
  v-bvec2 :pure t)
(v-def-glsl-template-fun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-vec3 v-vec3)
  v-bvec3 :pure t)
(v-def-glsl-template-fun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-vec4 v-vec4)
  v-bvec4 :pure t)

(v-def-glsl-template-fun v-not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-bvector v-bvector) 0 :pure t)
(v-def-glsl-template-fun v-not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-float v-float) v-bool :pure t)
(v-def-glsl-template-fun v-not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-vec2 v-vec2) v-bvec2 :pure t)
(v-def-glsl-template-fun v-not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-vec3 v-vec3) v-bvec3 :pure t)
(v-def-glsl-template-fun v-not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-vec4 v-vec4) v-bvec4 :pure t)

(v-def-glsl-template-fun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-int v-int)
  v-bool :pure t)

(v-def-glsl-template-fun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-ivec2 v-ivec2)
  v-bvec2 :pure t)

(v-def-glsl-template-fun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-ivec3 v-ivec3)
  v-bvec3 :pure t)

(v-def-glsl-template-fun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-ivec4 v-ivec4)
  v-bvec4 :pure t)

(v-def-glsl-template-fun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-uint v-uint)
  v-bool :pure t)

(v-def-glsl-template-fun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-uvec2 v-uvec2)
  v-bvec2 :pure t)

(v-def-glsl-template-fun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-uvec3 v-uvec3)
  v-bvec3 :pure t)

(v-def-glsl-template-fun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-uvec4 v-uvec4)
  v-bvec4 :pure t)

(v-def-glsl-template-fun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-double v-double) v-bool :pure t)

(v-def-glsl-template-fun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-dvec2 v-dvec2)
  v-bvec2 :pure t)

(v-def-glsl-template-fun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-dvec3 v-dvec3)
  v-bvec3 :pure t)

(v-def-glsl-template-fun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-dvec4 v-dvec4)
  v-bvec4 :pure t)

(v-def-glsl-template-fun atomic-counter (c &context (:330 :440))
  "atomicCounter(~a)"
  (v-atomic-uint) v-uint)

;;------------------------------------------------------------

(v-def-glsl-template-fun mat2 (a b c d) "mat2(~a,~a,~a,~a)" (v-float v-float v-float v-float)
         v-mat2 :pure t)
(v-def-glsl-template-fun mat2 (a b) "mat2(~a,~a)" (v-vec2 v-vec2) v-mat2 :pure t)

(v-def-glsl-template-fun mat3 (a b c d e f g h i) "mat3(~a,~a,~a,~a,~a,~a,~a,~a,~a)"
         (v-float v-float v-float v-float v-float
                  v-float v-float v-float v-float) v-mat3 :pure t)

(v-def-glsl-template-fun mat3 (a b c) "mat3(~a,~a,~a)" (v-vec3 v-vec3 v-vec3) v-mat3
                         :pure t)

(v-def-glsl-template-fun mat4 (a b c d e f g h i j k l m n o p)
  "mat4(~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a)"
  (v-float v-float v-float v-float v-float v-float v-float v-float v-float
           v-float v-float v-float v-float v-float v-float v-float)
  v-mat4 :pure t)

(v-def-glsl-template-fun mat4 (a b c d)
  "mat4(~a,~a,~a,~a)" (v-vec4 v-vec4 v-vec4 v-vec4) v-mat4 :pure t)

;;------------------------------------------------------------

(v-def-glsl-template-fun vec3 (x y) "vec3(~a,~a)" (v-float v-vec2) v-vec3 :pure t)
(v-def-glsl-template-fun vec3 (x y) "vec3(~a,~a)" (v-vec2 v-float) v-vec3 :pure t)

(v-def-glsl-template-fun vec4 (x y) "vec4(~a,~a)" (v-vec2 v-vec2) v-vec4 :pure t)
(v-def-glsl-template-fun vec4 (x y) "vec4(~a,~a)" (v-float v-vec3) v-vec4 :pure t)
(v-def-glsl-template-fun vec4 (x y) "vec4(~a,~a)" (v-vec3 v-float) v-vec4 :pure t)

(v-def-glsl-template-fun vec4 (x y z) "vec4(~a,~a,~a)" (v-vec2 v-float v-float) v-vec4 :pure t)
(v-def-glsl-template-fun vec4 (x y z) "vec4(~a,~a,~a)" (v-float v-vec2 v-float) v-vec4 :pure t)
(v-def-glsl-template-fun vec4 (x y z) "vec4(~a,~a,~a)" (v-float v-float v-vec2) v-vec4 :pure t)

(v-def-glsl-template-fun vec2 (x y) "vec2(~a,~a)" (v-float v-float) v-vec2 :pure t)
(v-def-glsl-template-fun vec3 (x y z) "vec3(~a,~a,~a)" (v-float v-float v-float) v-vec3 :pure t)
(v-def-glsl-template-fun vec4 (x y z w) "vec4(~a,~a,~a,~a)" (v-float v-float v-float v-float)
                         v-vec4 :pure t)

(v-def-glsl-template-fun vec2 (x) "vec2(~a)" (v-float) v-vec2 :pure t)

(v-def-glsl-template-fun vec3 (x) "vec3(~a)" (v-float) v-vec3 :pure t)
(v-def-glsl-template-fun vec3 (x y) "vec3(~a, ~a, 0.0f)" (v-float v-float) v-vec3 :pure t)

(v-def-glsl-template-fun vec4 (x) "vec4(~a)" (v-float) v-vec4 :pure t)
(v-def-glsl-template-fun vec4 (x y) "vec4(~a, ~a, 0.0f, 0.0f)" (v-float v-float) v-vec4 :pure t)
(v-def-glsl-template-fun vec4 (x y z) "vec4(~a, ~a, ~a, 0.0f)" (v-float v-float v-float) v-vec4 :pure t)

;;------------------------------------------------------------

(v-def-glsl-template-fun bvec3 (x y) "bvec3(~a,~a)" (v-bool v-bvec2) v-bvec3 :pure t)
(v-def-glsl-template-fun bvec3 (x y) "bvec3(~a,~a)" (v-bvec2 v-bool) v-bvec3 :pure t)

(v-def-glsl-template-fun bvec4 (x y) "bvec4(~a,~a)" (v-bvec2 v-bvec2) v-bvec4 :pure t)
(v-def-glsl-template-fun bvec4 (x y) "bvec4(~a,~a)" (v-bool v-bvec3) v-bvec4 :pure t)
(v-def-glsl-template-fun bvec4 (x y) "bvec4(~a,~a)" (v-bvec3 v-bool) v-bvec4 :pure t)

(v-def-glsl-template-fun bvec4 (x y z) "bvec4(~a,~a,~a)" (v-bvec2 v-bool v-bool) v-bvec4 :pure t)
(v-def-glsl-template-fun bvec4 (x y z) "bvec4(~a,~a,~a)" (v-bool v-bvec2 v-bool) v-bvec4 :pure t)
(v-def-glsl-template-fun bvec4 (x y z) "bvec4(~a,~a,~a)" (v-bool v-bool v-bvec2) v-bvec4 :pure t)

(v-def-glsl-template-fun bvec2 (x y) "bvec2(~a,~a)" (v-bool v-bool) v-bvec2 :pure t)
(v-def-glsl-template-fun bvec3 (x y z) "bvec3(~a,~a,~a)" (v-bool v-bool v-bool) v-bvec3 :pure t)
(v-def-glsl-template-fun bvec4 (x y z w) "bvec4(~a,~a,~a,~a)" (v-bool v-bool v-bool v-bool)
         v-bvec4 :pure t)

;;------------------------------------------------------------

(v-def-glsl-template-fun ivec2 (x) "ivec2(~a)" (v-vec2) v-ivec2 :pure t)
(v-def-glsl-template-fun ivec3 (x) "ivec3(~a)" (v-vec3) v-ivec3 :pure t)
(v-def-glsl-template-fun ivec4 (x) "ivec4(~a)" (v-vec4) v-ivec4 :pure t)

(v-def-glsl-template-fun ivec3 (x y) "ivec3(~a,~a)" (v-int v-ivec2) v-ivec3 :pure t)
(v-def-glsl-template-fun ivec3 (x y) "ivec3(~a,~a)" (v-ivec2 v-int) v-ivec3 :pure t)

(v-def-glsl-template-fun ivec4 (x y) "ivec4(~a,~a)" (v-ivec2 v-ivec2) v-ivec4 :pure t)
(v-def-glsl-template-fun ivec4 (x y) "ivec4(~a,~a)" (v-int v-ivec3) v-ivec4 :pure t)
(v-def-glsl-template-fun ivec4 (x y) "ivec4(~a,~a)" (v-ivec3 v-int) v-ivec4 :pure t)

(v-def-glsl-template-fun ivec4 (x y z) "ivec4(~a,~a,~a)" (v-ivec2 v-int v-int) v-ivec4 :pure t)
(v-def-glsl-template-fun ivec4 (x y z) "ivec4(~a,~a,~a)" (v-int v-ivec2 v-int) v-ivec4 :pure t)
(v-def-glsl-template-fun ivec4 (x y z) "ivec4(~a,~a,~a)" (v-int v-int v-ivec2) v-ivec4 :pure t)

(v-def-glsl-template-fun ivec2 (x y) "ivec2(~a,~a)" (v-int v-int) v-ivec2 :pure t)
(v-def-glsl-template-fun ivec3 (x y z) "ivec3(~a,~a,~a)" (v-int v-int v-int) v-ivec3 :pure t)
(v-def-glsl-template-fun ivec4 (x y z w) "ivec4(~a,~a,~a,~a)" (v-int v-int v-int v-int)
         v-ivec4 :pure t)

(v-def-glsl-template-fun ivec2 (x) "ivec2(~a)" (v-int) v-ivec2 :pure t)

(v-def-glsl-template-fun ivec3 (x) "ivec3(~a)" (v-int) v-ivec3 :pure t)
(v-def-glsl-template-fun ivec3 (x y) "ivec3(~a, ~a, 0)" (v-int v-int) v-ivec3 :pure t)

(v-def-glsl-template-fun ivec4 (x) "ivec4(~a)" (v-int) v-ivec4 :pure t)
(v-def-glsl-template-fun ivec4 (x y) "ivec4(~a, ~a, 0, 0)" (v-int v-int) v-ivec4 :pure t)
(v-def-glsl-template-fun ivec4 (x y z) "ivec4(~a, ~a, ~a, 0)" (v-int v-int v-int) v-ivec4 :pure t)

;;------------------------------------------------------------

(v-def-glsl-template-fun uvec3 (x y) "uvec3(~a,~a)" (v-uint v-uvec2) v-uvec3 :pure t)
(v-def-glsl-template-fun uvec3 (x y) "uvec3(~a,~a)" (v-uvec2 v-uint) v-uvec3 :pure t)

(v-def-glsl-template-fun uvec4 (x y) "uvec4(~a,~a)" (v-uvec2 v-uvec2) v-uvec4 :pure t)
(v-def-glsl-template-fun uvec4 (x y) "uvec4(~a,~a)" (v-uint v-uvec3) v-uvec4 :pure t)
(v-def-glsl-template-fun uvec4 (x y) "uvec4(~a,~a)" (v-uvec3 v-uint) v-uvec4 :pure t)

(v-def-glsl-template-fun uvec4 (x y z) "uvec4(~a,~a,~a)" (v-uvec2 v-uint v-uint) v-uvec4 :pure t)
(v-def-glsl-template-fun uvec4 (x y z) "uvec4(~a,~a,~a)" (v-uint v-uvec2 v-uint) v-uvec4 :pure t)
(v-def-glsl-template-fun uvec4 (x y z) "uvec4(~a,~a,~a)" (v-uint v-uint v-uvec2) v-uvec4 :pure t)

(v-def-glsl-template-fun uvec2 (x y) "uvec2(~a,~a)" (v-uint v-uint) v-uvec2 :pure t)
(v-def-glsl-template-fun uvec3 (x y z) "uvec3(~a,~a,~a)" (v-uint v-uint v-uint) v-uvec3 :pure t)
(v-def-glsl-template-fun uvec4 (x y z w) "uvec4(~a,~a,~a,~a)" (v-uint v-uint v-uint v-uint)
         v-uvec4 :pure t)

(v-def-glsl-template-fun uvec2 (x) "uvec2(~a)" (v-uint) v-uvec2 :pure t)

(v-def-glsl-template-fun uvec3 (x) "uvec3(~a)" (v-uint) v-uvec3 :pure t)
(v-def-glsl-template-fun uvec3 (x y) "uvec3(~a, ~a, 0)" (v-uint v-uint) v-uvec3 :pure t)

(v-def-glsl-template-fun uvec4 (x) "uvec4(~a)" (v-uint) v-uvec4 :pure t)
(v-def-glsl-template-fun uvec4 (x y) "uvec4(~a, ~a, 0, 0)" (v-uint v-uint) v-uvec4 :pure t)
(v-def-glsl-template-fun uvec4 (x y z) "uvec4(~a, ~a, ~a, 0)" (v-uint v-uint v-uint) v-uvec4 :pure t)

;;------------------------------------------------------------

(v-def-glsl-template-fun dvec3 (x y) "dvec3(~a,~a)" (v-double v-dvec2) v-dvec3 :pure t)
(v-def-glsl-template-fun dvec3 (x y) "dvec3(~a,~a)" (v-dvec2 v-double) v-dvec3 :pure t)

(v-def-glsl-template-fun dvec4 (x y) "dvec4(~a,~a)" (v-dvec2 v-dvec2) v-dvec4 :pure t)
(v-def-glsl-template-fun dvec4 (x y) "dvec4(~a,~a)" (v-double v-dvec3) v-dvec4 :pure t)
(v-def-glsl-template-fun dvec4 (x y) "dvec4(~a,~a)" (v-dvec3 v-double) v-dvec4 :pure t)

(v-def-glsl-template-fun dvec4 (x y z) "dvec4(~a,~a,~a)" (v-dvec2 v-double v-double) v-dvec4 :pure t)
(v-def-glsl-template-fun dvec4 (x y z) "dvec4(~a,~a,~a)" (v-double v-dvec2 v-double) v-dvec4 :pure t)
(v-def-glsl-template-fun dvec4 (x y z) "dvec4(~a,~a,~a)" (v-double v-double v-dvec2) v-dvec4 :pure t)

(v-def-glsl-template-fun dvec2 (x y) "dvec2(~a,~a)" (v-double v-double) v-dvec2 :pure t)
(v-def-glsl-template-fun dvec3 (x y z) "dvec3(~a,~a,~a)" (v-double v-double v-double) v-dvec3 :pure t)
(v-def-glsl-template-fun dvec4 (x y z w) "dvec4(~a,~a,~a,~a)" (v-double v-double v-double v-double)
         v-dvec4 :pure t)

(v-def-glsl-template-fun dvec2 (x) "dvec2(~a)" (v-double) v-dvec2 :pure t)

(v-def-glsl-template-fun dvec3 (x) "dvec3(~a)" (v-double) v-dvec3 :pure t)
(v-def-glsl-template-fun dvec3 (x y) "dvec3(~a, ~a, 0.0lf)" (v-double v-double) v-dvec3 :pure t)

(v-def-glsl-template-fun dvec4 (x) "dvec4(~a)" (v-double) v-dvec4 :pure t)
(v-def-glsl-template-fun dvec4 (x y) "dvec4(~a, ~a, 0.0lf, 0.0lf)" (v-double v-double) v-dvec4 :pure t)
(v-def-glsl-template-fun dvec4 (x y z) "dvec4(~a, ~a, ~a, 0.0lf)" (v-double v-double v-double) v-dvec4 :pure t)

;;------------------------------------------------------------

(v-def-glsl-template-fun max (a) "~a" (:float) :float :pure t)
(v-def-glsl-template-fun max (a) "~a" (:vec2) :float :pure t)
(v-def-glsl-template-fun max (a) "~a" (:vec3) :float :pure t)
(v-def-glsl-template-fun max (a) "~a" (:vec4) :float :pure t)

(v-def-glsl-template-fun max (a) "~a" (:int) :float :pure t)
(v-def-glsl-template-fun max (a) "~a" (:ivec2) :float :pure t)
(v-def-glsl-template-fun max (a) "~a" (:ivec3) :float :pure t)
(v-def-glsl-template-fun max (a) "~a" (:ivec4) :float :pure t)

(v-def-glsl-template-fun max (a) "~a" (:uint) :float :pure t)
(v-def-glsl-template-fun max (a) "~a" (:uvec2) :float :pure t)
(v-def-glsl-template-fun max (a) "~a" (:uvec3) :float :pure t)
(v-def-glsl-template-fun max (a) "~a" (:uvec4) :float :pure t)

(v-def-glsl-template-fun max (a b c &rest c) "max(~a, max(~a, max(~a~{ ,~a~})"
                         (t t t &rest t) 0 :pure t)

(v-define-compiler-macro max ((a t) (b t) (c t) &rest (d t))
  `(max ,a (max ,b (max ,c ,@d))))

(v-def-glsl-template-fun min (a b c &rest c) "min(~a, min(~a, min(~a~{ ,~a~})"
                         (t t t &rest t) 0 :pure t)

(v-define-compiler-macro min ((a t) (b t) (c t) &rest (d t))
  `(min ,a (min ,b (min ,c ,@d))))
