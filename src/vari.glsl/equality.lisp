(in-package :vari.glsl)

;;------------------------------------------------------------

(v-def-glsl-template-fun = (a b) "(~a == ~a)" (v-array v-array) v-bool :pure t)

;;------------------------------------------------------------

(v-def-glsl-template-fun v-not (x &context (:330 :440)) "not(~a)"
         (v-bvector) 0 :pure t)

;;------------------------------------------------------------

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

;;------------------------------------------------------------
