(in-package :varjo)

(v-defun aref (x i) "~a[~a]" (v-array v-int) (:element 0) :v-place-index 0)
(v-defun aref (x i) "~a[~a]" (v-vector v-int) (:element 0) :v-place-index 0)

(v-defun break () "break" () v-void :glsl-spec-matching t)

(v-defun int (x) "int(~a)" (v-uint) v-int :glsl-spec-matching t)
(v-defun int (x) "int(~a)" (v-bool) v-int :glsl-spec-matching t)
(v-defun int (x) "int(~a)" (v-float) v-int :glsl-spec-matching t)
(v-defun int (x) "int(~a)" (v-double)  v-int :glsl-spec-matching t)
(v-defun uint (x) "uint(~a)" (v-int) v-uint :glsl-spec-matching t)
(v-defun uint (x) "uint(~a)" (v-bool) v-uint :glsl-spec-matching t)
(v-defun uint (x) "uint(~a)" (v-float)  v-uint :glsl-spec-matching t)
(v-defun uint (x) "uint(~a)" (v-double)  v-uint :glsl-spec-matching t)
(v-defun bool (x) "bool(~a)" (v-int) v-bool :glsl-spec-matching t)
(v-defun bool (x) "bool(~a)" (v-uint) v-bool :glsl-spec-matching t)
(v-defun bool (x) "bool(~a)" (v-float)  v-bool :glsl-spec-matching t)
(v-defun bool (x) "bool(~a)" (v-double)  v-bool :glsl-spec-matching t)
(v-defun float (x) "float(~a)" (v-int) v-float :glsl-spec-matching t)
(v-defun float (x) "float(~a)" (v-uint)  v-float :glsl-spec-matching t)
(v-defun float (x) "float(~a)" (v-bool)  v-float :glsl-spec-matching t)
(v-defun float (x) "float(~a)" (v-double) v-float :glsl-spec-matching t)
(v-defun double (x) "double(~a)" (v-int)  v-double :glsl-spec-matching t)
(v-defun double (x) "double(~a)" (v-uint)  v-double :glsl-spec-matching t)
(v-defun double (x) "double(~a)" (v-bool)  v-double :glsl-spec-matching t)
(v-defun double (x) "double(~a)" (v-float) v-double :glsl-spec-matching t)

(v-defun x (a) "~a.x" (v-vector) (:element 0) :glsl-spec-matching t)
(v-defun y (a) "~a.y" (v-vector) (:element 0) :glsl-spec-matching t)
(v-defun z (a) "~a.z" (v-vec3)  (:element 0) :glsl-spec-matching t)
(v-defun z (a) "~a.z" (v-bvec3) (:element 0) :glsl-spec-matching t)
(v-defun z (a) "~a.z" (v-ivec3) (:element 0) :glsl-spec-matching t)
(v-defun z (a) "~a.z" (v-uvec3) (:element 0) :glsl-spec-matching t)
(v-defun z (a) "~a.z" (v-dvec3) (:element 0) :glsl-spec-matching t)
(v-defun z (a) "~a.z" (v-vec4)  (:element 0) :glsl-spec-matching t)
(v-defun z (a) "~a.z" (v-bvec4) (:element 0) :glsl-spec-matching t)
(v-defun z (a) "~a.z" (v-ivec4) (:element 0) :glsl-spec-matching t)
(v-defun z (a) "~a.z" (v-uvec4) (:element 0) :glsl-spec-matching t)
(v-defun z (a) "~a.z" (v-dvec4) (:element 0) :glsl-spec-matching t)
(v-defun w (a) "~a.w" (v-vec4) (:element 0) :glsl-spec-matching t)
(v-defun w (a) "~a.w" (v-bvec4) (:element 0) :glsl-spec-matching t)
(v-defun w (a) "~a.w" (v-ivec4) (:element 0) :glsl-spec-matching t)
(v-defun w (a) "~a.w" (v-uvec4) (:element 0) :glsl-spec-matching t)
(v-defun w (a) "~a.w" (v-dvec4) (:element 0) :glsl-spec-matching t)

(v-defun not (a) "(~a == true)" (v-bool) v-bool :glsl-spec-matching t)
(v-defun not (a) "false" (v-type) v-bool :glsl-spec-matching t)

(v-defun %< (a b) "(~a < ~a)" (v-number v-number) v-bool :glsl-spec-matching t)
(v-defun %> (a b) "(~a > ~a)" (v-number v-number) v-bool :glsl-spec-matching t)
(v-defun %<= (a b) "(~a <= ~a)" (v-number v-number) v-bool :glsl-spec-matching t)
(v-defun %>= (a b) "(~a >= ~a)" (v-number v-number) v-bool :glsl-spec-matching t)

(v-defun %equal (a b) "(~a == ~a)" (v-number v-number) v-bool :glsl-spec-matching t)
(v-defun %eql (a b) "(~a == ~a)" (v-number v-number) v-bool :glsl-spec-matching t)
(v-defun %= (a b) "(~a == ~a)" (v-number v-number) v-bool :glsl-spec-matching t)

(v-defun ++ (a) "(++ ~a)" (v-number) nil :glsl-spec-matching t)
(v-defun -- (a) "(-- ~a)" (v-number) nil :glsl-spec-matching t)

(v-defun %+ (a) "(~a)" (v-number) nil :glsl-spec-matching t)
(v-defun %+ (a b) "(~a + ~a)" (v-number v-number) nil :glsl-spec-matching t)

(v-defun %+ (a b) "(~a + ~a)" (v-vec2 v-vec2) 0 :glsl-spec-matching t)
(v-defun %+ (a b) "(~a + ~a)" (v-vec3 v-vec3) 0 :glsl-spec-matching t)
(v-defun %+ (a b) "(~a + ~a)" (v-vec4 v-vec4) 0 :glsl-spec-matching t)

(v-defun %+ (a b) "(~a + ~a)" (v-ivec2 v-ivec2) 0 :glsl-spec-matching t)
(v-defun %+ (a b) "(~a + ~a)" (v-ivec3 v-ivec3) 0 :glsl-spec-matching t)
(v-defun %+ (a b) "(~a + ~a)" (v-ivec4 v-ivec4) 0 :glsl-spec-matching t)

(v-defun %+ (a b) "(~a + ~a)" (v-uvec2 v-uvec2) 0 :glsl-spec-matching t)
(v-defun %+ (a b) "(~a + ~a)" (v-uvec3 v-uvec3) 0 :glsl-spec-matching t)
(v-defun %+ (a b) "(~a + ~a)" (v-uvec4 v-uvec4) 0 :glsl-spec-matching t)

(v-defun %+ (a b) "(~a + ~a)" (v-dvec2 v-dvec2) 0 :glsl-spec-matching t)
(v-defun %+ (a b) "(~a + ~a)" (v-dvec3 v-dvec3) 0 :glsl-spec-matching t)
(v-defun %+ (a b) "(~a + ~a)" (v-dvec4 v-dvec4) 0 :glsl-spec-matching t)

(v-defun %- (a b) "(~a - ~a)" (v-number v-number) nil :glsl-spec-matching t)
(v-defun %- (a) "(- ~a)" (v-number) 0 :glsl-spec-matching t)
(v-defun %- (a b) "(~a - ~a)" (v-vec2 v-vec2) 0 :glsl-spec-matching t)
(v-defun %- (a b) "(~a - ~a)" (v-vec3 v-vec3) 0 :glsl-spec-matching t)
(v-defun %- (a b) "(~a - ~a)" (v-vec4 v-vec4) 0 :glsl-spec-matching t)
(v-defun %- (a) "(-~a)" (v-vec2) 0 :glsl-spec-matching t)
(v-defun %- (a) "(-~a)" (v-vec3) 0 :glsl-spec-matching t)
(v-defun %- (a) "(-~a)" (v-vec4) 0 :glsl-spec-matching t)

(v-defun %- (a b) "(~a - ~a)" (v-ivec2 v-ivec2) 0 :glsl-spec-matching t)
(v-defun %- (a b) "(~a - ~a)" (v-ivec3 v-ivec3) 0 :glsl-spec-matching t)
(v-defun %- (a b) "(~a - ~a)" (v-ivec4 v-ivec4) 0 :glsl-spec-matching t)

(v-defun %- (a b) "(~a - ~a)" (v-uvec2 v-uvec2) 0 :glsl-spec-matching t)
(v-defun %- (a b) "(~a - ~a)" (v-uvec3 v-uvec3) 0 :glsl-spec-matching t)
(v-defun %- (a b) "(~a - ~a)" (v-uvec4 v-uvec4) 0 :glsl-spec-matching t)

(v-defun %- (a b) "(~a - ~a)" (v-dvec2 v-dvec2) 0 :glsl-spec-matching t)
(v-defun %- (a b) "(~a - ~a)" (v-dvec3 v-dvec3) 0 :glsl-spec-matching t)
(v-defun %- (a b) "(~a - ~a)" (v-dvec4 v-dvec4) 0 :glsl-spec-matching t)

(v-defun %* (a b) "(~a * ~a)" (v-number v-number) nil :glsl-spec-matching t)
(v-defun %* (a b) "(~a * ~a)" (v-vector v-vector) nil :glsl-spec-matching t)
(v-defun %* (a b) "(~a * ~a)" (v-matrix v-matrix) nil :glsl-spec-matching t)

(v-defun %* (a b) "(~a * ~a)" (v-mat2 v-vec2) 1 :glsl-spec-matching t)
(v-defun %* (a b) "(~a * ~a)" (v-mat3 v-vec3) 1 :glsl-spec-matching t)
(v-defun %* (a b) "(~a * ~a)" (v-mat4 v-vec4) 1 :glsl-spec-matching t)

(v-defun %* (a b) "(~a * ~a)" (v-mat2 v-float) 0 :glsl-spec-matching t)
(v-defun %* (a b) "(~a * ~a)" (v-mat4 v-float) 0 :glsl-spec-matching t)
(v-defun %* (a b) "(~a * ~a)" (v-mat3 v-float) 0 :glsl-spec-matching t)

(v-defun %* (a b) "(~a * ~a)" (v-vec2 v-float) 0 :glsl-spec-matching t)
(v-defun %* (a b) "(~a * ~a)" (v-vec3 v-float) 0 :glsl-spec-matching t)
(v-defun %* (a b) "(~a * ~a)" (v-vec4 v-float) 0 :glsl-spec-matching t)
(v-defun %* (a b) "(~a * ~a)" (v-float v-vec2) 1 :glsl-spec-matching t)
(v-defun %* (a b) "(~a * ~a)" (v-float v-vec3) 1 :glsl-spec-matching t)
(v-defun %* (a b) "(~a * ~a)" (v-float v-vec4) 1 :glsl-spec-matching t)

(v-defun %/ (a b) "(~a / ~a)" (v-number v-number) nil :glsl-spec-matching t)
(v-defun %/ (a b) "(~a / ~a)" (v-vector v-number) 0 :glsl-spec-matching t)
(v-defun %/ (a b) "(~a / ~a)" (v-vector v-vector) nil :glsl-spec-matching t)

(v-defun v! (x y) "vec3(~a,~a)" (v-float v-vec2) v-vec3 :glsl-spec-matching t)
(v-defun v! (x y) "vec3(~a,~a)" (v-vec2 v-float) v-vec3 :glsl-spec-matching t)

(v-defun v! (x y) "vec4(~a,~a)" (v-vec2 v-vec2) v-vec4 :glsl-spec-matching t)
(v-defun v! (x y) "vec4(~a,~a)" (v-float v-vec3) v-vec4 :glsl-spec-matching t)
(v-defun v! (x y) "vec4(~a,~a)" (v-vec3 v-float) v-vec4 :glsl-spec-matching t)

(v-defun v! (x y z) "vec4(~a,~a,~a)" (v-vec2 v-float v-float) v-vec4 :glsl-spec-matching t)
(v-defun v! (x y z) "vec4(~a,~a,~a)" (v-float v-vec2 v-float) v-vec4 :glsl-spec-matching t)
(v-defun v! (x y z) "vec4(~a,~a,~a)" (v-float v-float v-vec2) v-vec4 :glsl-spec-matching t)

(v-defun v! (x y) "vec2(~a,~a)" (v-float v-float) v-vec2 :glsl-spec-matching t)
(v-defun v! (x y z) "vec3(~a,~a,~a)" (v-float v-float v-float) v-vec3 :glsl-spec-matching t)
(v-defun v! (x y z w) "vec4(~a,~a,~a,~a)" (v-float v-float v-float v-float)
         v-vec4 :glsl-spec-matching t)


(v-defun v!bool (x y) "bvec3(~a,~a)" (v-bool v-bvec2) v-bvec3 :glsl-spec-matching t)
(v-defun v!bool (x y) "bvec3(~a,~a)" (v-bvec2 v-bool) v-bvec3 :glsl-spec-matching t)

(v-defun v!bool (x y) "bvec4(~a,~a)" (v-bvec2 v-bvec2) v-bvec4 :glsl-spec-matching t)
(v-defun v!bool (x y) "bvec4(~a,~a)" (v-bool v-bvec3) v-bvec4 :glsl-spec-matching t)
(v-defun v!bool (x y) "bvec4(~a,~a)" (v-bvec3 v-bool) v-bvec4 :glsl-spec-matching t)

(v-defun v!bool (x y z) "bvec4(~a,~a,~a)" (v-bvec2 v-bool v-bool) v-bvec4 :glsl-spec-matching t)
(v-defun v!bool (x y z) "bvec4(~a,~a,~a)" (v-bool v-bvec2 v-bool) v-bvec4 :glsl-spec-matching t)
(v-defun v!bool (x y z) "bvec4(~a,~a,~a)" (v-bool v-bool v-bvec2) v-bvec4 :glsl-spec-matching t)

(v-defun v!bool (x y) "bvec2(~a,~a)" (v-bool v-bool) v-bvec2 :glsl-spec-matching t)
(v-defun v!bool (x y z) "bvec3(~a,~a,~a)" (v-bool v-bool v-bool) v-bvec3 :glsl-spec-matching t)
(v-defun v!bool (x y z w) "bvec4(~a,~a,~a,~a)" (v-bool v-bool v-bool v-bool)
         v-bvec4 :glsl-spec-matching t)


(v-defun v!int (x y) "ivec3(~a,~a)" (v-int v-ivec2) v-ivec3 :glsl-spec-matching t)
(v-defun v!int (x y) "ivec3(~a,~a)" (v-ivec2 v-int) v-ivec3 :glsl-spec-matching t)

(v-defun v!int (x y) "ivec4(~a,~a)" (v-ivec2 v-ivec2) v-ivec4 :glsl-spec-matching t)
(v-defun v!int (x y) "ivec4(~a,~a)" (v-int v-ivec3) v-ivec4 :glsl-spec-matching t)
(v-defun v!int (x y) "ivec4(~a,~a)" (v-ivec3 v-int) v-ivec4 :glsl-spec-matching t)

(v-defun v!int (x y z) "ivec4(~a,~a,~a)" (v-ivec2 v-int v-int) v-ivec4 :glsl-spec-matching t)
(v-defun v!int (x y z) "ivec4(~a,~a,~a)" (v-int v-ivec2 v-int) v-ivec4 :glsl-spec-matching t)
(v-defun v!int (x y z) "ivec4(~a,~a,~a)" (v-int v-int v-ivec2) v-ivec4 :glsl-spec-matching t)

(v-defun v!int (x y) "ivec2(~a,~a)" (v-int v-int) v-ivec2 :glsl-spec-matching t)
(v-defun v!int (x y z) "ivec3(~a,~a,~a)" (v-int v-int v-int) v-ivec3 :glsl-spec-matching t)
(v-defun v!int (x y z w) "ivec4(~a,~a,~a,~a)" (v-int v-int v-int v-int)
         v-ivec4 :glsl-spec-matching t)


(v-defun v!uint (x y) "uvec3(~a,~a)" (v-uint v-uvec2) v-uvec3 :glsl-spec-matching t)
(v-defun v!uint (x y) "uvec3(~a,~a)" (v-uvec2 v-uint) v-uvec3 :glsl-spec-matching t)

(v-defun v!uint (x y) "uvec4(~a,~a)" (v-uvec2 v-uvec2) v-uvec4 :glsl-spec-matching t)
(v-defun v!uint (x y) "uvec4(~a,~a)" (v-uint v-uvec3) v-uvec4 :glsl-spec-matching t)
(v-defun v!uint (x y) "uvec4(~a,~a)" (v-uvec3 v-uint) v-uvec4 :glsl-spec-matching t)

(v-defun v!uint (x y z) "uvec4(~a,~a,~a)" (v-uvec2 v-uint v-uint) v-uvec4 :glsl-spec-matching t)
(v-defun v!uint (x y z) "uvec4(~a,~a,~a)" (v-uint v-uvec2 v-uint) v-uvec4 :glsl-spec-matching t)
(v-defun v!uint (x y z) "uvec4(~a,~a,~a)" (v-uint v-uint v-uvec2) v-uvec4 :glsl-spec-matching t)

(v-defun v!uint (x y) "uvec2(~a,~a)" (v-uint v-uint) v-uvec2 :glsl-spec-matching t)
(v-defun v!uint (x y z) "uvec3(~a,~a,~a)" (v-uint v-uint v-uint) v-uvec3 :glsl-spec-matching t)
(v-defun v!uint (x y z w) "uvec4(~a,~a,~a,~a)" (v-uint v-uint v-uint v-uint)
         v-uvec4 :glsl-spec-matching t)


(v-defun v!double (x y) "dvec3(~a,~a)" (v-double v-dvec2) v-dvec3 :glsl-spec-matching t)
(v-defun v!double (x y) "dvec3(~a,~a)" (v-dvec2 v-double) v-dvec3 :glsl-spec-matching t)

(v-defun v!double (x y) "dvec4(~a,~a)" (v-dvec2 v-dvec2) v-dvec4 :glsl-spec-matching t)
(v-defun v!double (x y) "dvec4(~a,~a)" (v-double v-dvec3) v-dvec4 :glsl-spec-matching t)
(v-defun v!double (x y) "dvec4(~a,~a)" (v-dvec3 v-double) v-dvec4 :glsl-spec-matching t)

(v-defun v!double (x y z) "dvec4(~a,~a,~a)" (v-dvec2 v-double v-double) v-dvec4 :glsl-spec-matching t)
(v-defun v!double (x y z) "dvec4(~a,~a,~a)" (v-double v-dvec2 v-double) v-dvec4 :glsl-spec-matching t)
(v-defun v!double (x y z) "dvec4(~a,~a,~a)" (v-double v-double v-dvec2) v-dvec4 :glsl-spec-matching t)

(v-defun v!double (x y) "dvec2(~a,~a)" (v-double v-double) v-dvec2 :glsl-spec-matching t)
(v-defun v!double (x y z) "dvec3(~a,~a,~a)" (v-double v-double v-double) v-dvec3 :glsl-spec-matching t)
(v-defun v!double (x y z w) "dvec4(~a,~a,~a,~a)" (v-double v-double v-double v-double)
         v-dvec4 :glsl-spec-matching t)




(v-defun m! (a b c d) "mat2(~a,~a,~a,~a)" (v-float v-float v-float v-float)
         v-mat2 :glsl-spec-matching t)
(v-defun m! (a b) "mat2(~a,~a)" (v-vec2 v-vec2) v-mat2 :glsl-spec-matching t)

(v-defun m! (a b c d e f g h i) "mat3(~a,~a,~a,~a,~a,~a,~a,~a,~a)"
         (v-float v-float v-float v-float v-float
                  v-float v-float v-float v-float) v-mat3 :glsl-spec-matching t)
(v-defun m! (a b c) "mat3(~a,~a,~a)" (v-vec3 v-vec3 v-vec3) v-mat3
         :glsl-spec-matching t)

(v-defun m! (a b c d e f g h i j k l m n o p)
  "mat4(~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a)"
  (v-float v-float v-float v-float v-float v-float v-float v-float v-float
           v-float v-float v-float v-float v-float v-float v-float v-float)
  v-mat4 :glsl-spec-matching t)
(v-defun m! (a b c d)
  "mat4(~a,~a,~a,~a)" (v-vec4 v-vec4 v-vec4 v-vec4) v-mat4
  :glsl-spec-matching t)

(v-defun v-not (x &context (:330 :440)) "not(~a)"
	 (v-bvector) 0
         :glsl-spec-matching t)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-bvector v-bvector)
  0 :glsl-spec-matching t)
(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-float v-float)
  v-bool :glsl-spec-matching t)
(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-vec2 v-vec2)
  v-bvec2 :glsl-spec-matching t)
(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-vec3 v-vec3)
  v-bvec3 :glsl-spec-matching t)
(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-vec4 v-vec4)
  v-bvec4 :glsl-spec-matching t)

(v-defun v-not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-bvector v-bvector) 0 :glsl-spec-matching t)
(v-defun v-not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-float v-float) v-bool :glsl-spec-matching t)
(v-defun v-not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-vec2 v-vec2) v-bvec2 :glsl-spec-matching t)
(v-defun v-not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-vec3 v-vec3) v-bvec3 :glsl-spec-matching t)
(v-defun v-not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-vec4 v-vec4) v-bvec4 :glsl-spec-matching t)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-int v-int)
  v-bool :glsl-spec-matching t)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-ivec2 v-ivec2)
  v-bvec2 :glsl-spec-matching t)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-ivec3 v-ivec3)
  v-bvec3 :glsl-spec-matching t)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-ivec4 v-ivec4)
  v-bvec4 :glsl-spec-matching t)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-uint v-uint)
  v-bool :glsl-spec-matching t)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-uvec2 v-uvec2)
  v-bvec2 :glsl-spec-matching t)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-uvec3 v-uvec3)
  v-bvec3 :glsl-spec-matching t)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-uvec4 v-uvec4)
  v-bvec4 :glsl-spec-matching t)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-double v-double) v-bool :glsl-spec-matching t)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-dvec2 v-dvec2)
  v-bvec2 :glsl-spec-matching t)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-dvec3 v-dvec3)
  v-bvec3 :glsl-spec-matching t)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-dvec4 v-dvec4)
  v-bvec4 :glsl-spec-matching t)

(v-defun atomic-counter (c &context (:330 :440))
  "atomicCounter(~a)"
  (v-atomic_uint) v-uint :glsl-spec-matching t)
