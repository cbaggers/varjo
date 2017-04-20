(in-package :varjo)

(v-defun break () "break" () v-void)

(v-defun int (x) "~a" (v-int) v-int)
(v-defun uint (x) "~a" (v-uint) v-uint)
(v-defun bool (x) "~a" (v-bool) v-bool)
(v-defun float (x) "~a" (v-float) v-float)
(v-defun double (x) "~a" (v-double)  v-double)

(v-defun int (x) "int(~a)" (v-uint) v-int)
(v-defun int (x) "int(~a)" (v-bool) v-int)
(v-defun int (x) "int(~a)" (v-float) v-int)
(v-defun int (x) "int(~a)" (v-double)  v-int)
;; uint handled by special form
(v-defun bool (x) "bool(~a)" (v-int) v-bool)
(v-defun bool (x) "bool(~a)" (v-uint) v-bool)
(v-defun bool (x) "bool(~a)" (v-float)  v-bool)
(v-defun bool (x) "bool(~a)" (v-double)  v-bool)
(v-defun float (x) "float(~a)" (v-int) v-float)
(v-defun float (x) "float(~a)" (v-uint)  v-float)
(v-defun float (x) "float(~a)" (v-bool)  v-float)
(v-defun float (x) "float(~a)" (v-double) v-float)
(v-defun double (x) "double(~a)" (v-int)  v-double)
(v-defun double (x) "double(~a)" (v-uint)  v-double)
(v-defun double (x) "double(~a)" (v-bool)  v-double)
(v-defun double (x) "double(~a)" (v-float) v-double)

(v-defun not (a) "!~a" (v-bool) v-bool)
(v-defun not (a) "false" (v-type) v-bool)

(v-defun %< (a b) "(~a < ~a)" (v-number v-number) v-bool)
(v-defun %> (a b) "(~a > ~a)" (v-number v-number) v-bool)
(v-defun %<= (a b) "(~a <= ~a)" (v-number v-number) v-bool)
(v-defun %>= (a b) "(~a >= ~a)" (v-number v-number) v-bool)

(v-defun %equal (a b) "(~a == ~a)" (v-number v-number) v-bool)
(v-defun %eql (a b) "(~a == ~a)" (v-number v-number) v-bool)
(v-defun %= (a b) "(~a == ~a)" (v-number v-number) v-bool)

(v-defun ++ (a) "(++ ~a)" (v-number) nil)
(v-defun -- (a) "(-- ~a)" (v-number) nil)

(v-defun %+ (a) "(~a)" (v-number) nil)
(v-defun %+ (a b) "(~a + ~a)" (v-number v-number) nil)

(v-defun %+ (a b) "(~a + ~a)" (v-vec2 v-vec2) 0)
(v-defun %+ (a b) "(~a + ~a)" (v-vec3 v-vec3) 0)
(v-defun %+ (a b) "(~a + ~a)" (v-vec4 v-vec4) 0)

(v-defun %+ (a b) "(~a + ~a)" (v-ivec2 v-ivec2) 0)
(v-defun %+ (a b) "(~a + ~a)" (v-ivec3 v-ivec3) 0)
(v-defun %+ (a b) "(~a + ~a)" (v-ivec4 v-ivec4) 0)

(v-defun %+ (a b) "(~a + ~a)" (v-uvec2 v-uvec2) 0)
(v-defun %+ (a b) "(~a + ~a)" (v-uvec3 v-uvec3) 0)
(v-defun %+ (a b) "(~a + ~a)" (v-uvec4 v-uvec4) 0)

(v-defun %+ (a b) "(~a + ~a)" (v-dvec2 v-dvec2) 0)
(v-defun %+ (a b) "(~a + ~a)" (v-dvec3 v-dvec3) 0)
(v-defun %+ (a b) "(~a + ~a)" (v-dvec4 v-dvec4) 0)

(v-defun %- (a b) "(~a - ~a)" (v-number v-number) nil)
(v-defun %- (a) "(- ~a)" (v-number) 0)
(v-defun %- (a b) "(~a - ~a)" (v-vec2 v-vec2) 0)
(v-defun %- (a b) "(~a - ~a)" (v-vec3 v-vec3) 0)
(v-defun %- (a b) "(~a - ~a)" (v-vec4 v-vec4) 0)
(v-defun %- (a) "(-~a)" (v-vec2) 0)
(v-defun %- (a) "(-~a)" (v-vec3) 0)
(v-defun %- (a) "(-~a)" (v-vec4) 0)

(v-defun %- (a b) "(~a - ~a)" (v-ivec2 v-ivec2) 0)
(v-defun %- (a b) "(~a - ~a)" (v-ivec3 v-ivec3) 0)
(v-defun %- (a b) "(~a - ~a)" (v-ivec4 v-ivec4) 0)

(v-defun %- (a b) "(~a - ~a)" (v-uvec2 v-uvec2) 0)
(v-defun %- (a b) "(~a - ~a)" (v-uvec3 v-uvec3) 0)
(v-defun %- (a b) "(~a - ~a)" (v-uvec4 v-uvec4) 0)

(v-defun %- (a b) "(~a - ~a)" (v-dvec2 v-dvec2) 0)
(v-defun %- (a b) "(~a - ~a)" (v-dvec3 v-dvec3) 0)
(v-defun %- (a b) "(~a - ~a)" (v-dvec4 v-dvec4) 0)

(v-defun %* (a b) "(~a * ~a)" (v-number v-number) nil)
(v-defun %* (a b) "(~a * ~a)" (v-vector v-vector) nil)
(v-defun %* (a b) "(~a * ~a)" (v-matrix v-matrix) nil)

(v-defun %* (a b) "(~a * ~a)" (v-mat2 v-vec2) 1)
(v-defun %* (a b) "(~a * ~a)" (v-mat3 v-vec3) 1)
(v-defun %* (a b) "(~a * ~a)" (v-mat4 v-vec4) 1)

(v-defun %* (a b) "(~a * ~a)" (v-mat2 v-float) 0)
(v-defun %* (a b) "(~a * ~a)" (v-mat4 v-float) 0)
(v-defun %* (a b) "(~a * ~a)" (v-mat3 v-float) 0)

(v-defun %* (a b) "(~a * ~a)" (v-vec2 v-float) 0)
(v-defun %* (a b) "(~a * ~a)" (v-vec3 v-float) 0)
(v-defun %* (a b) "(~a * ~a)" (v-vec4 v-float) 0)
(v-defun %* (a b) "(~a * ~a)" (v-float v-vec2) 1)
(v-defun %* (a b) "(~a * ~a)" (v-float v-vec3) 1)
(v-defun %* (a b) "(~a * ~a)" (v-float v-vec4) 1)

(v-defun %/ (a b) "(~a / ~a)" (v-number v-number) nil)
(v-defun %/ (a b) "(~a / ~a)" (v-vector v-number) 0)
(v-defun %/ (a b) "(~a / ~a)" (v-vector v-vector) nil)

(v-defun v! (v) "~a" (v-vec2) v-vec2)
(v-defun v! (v) "~a" (v-vec2) v-vec3)
(v-defun v! (v) "~a" (v-vec4) v-vec4)
(v-defun v! (x y) "vec3(~a,~a)" (v-float v-vec2) v-vec3)
(v-defun v! (x y) "vec3(~a,~a)" (v-vec2 v-float) v-vec3)

(v-defun v! (x y) "vec4(~a,~a)" (v-vec2 v-vec2) v-vec4)
(v-defun v! (x y) "vec4(~a,~a)" (v-float v-vec3) v-vec4)
(v-defun v! (x y) "vec4(~a,~a)" (v-vec3 v-float) v-vec4)

(v-defun v! (x y z) "vec4(~a,~a,~a)" (v-vec2 v-float v-float) v-vec4)
(v-defun v! (x y z) "vec4(~a,~a,~a)" (v-float v-vec2 v-float) v-vec4)
(v-defun v! (x y z) "vec4(~a,~a,~a)" (v-float v-float v-vec2) v-vec4)

(v-defun v! (x y) "vec2(~a,~a)" (v-float v-float) v-vec2)
(v-defun v! (x y z) "vec3(~a,~a,~a)" (v-float v-float v-float) v-vec3)
(v-defun v! (x y z w) "vec4(~a,~a,~a,~a)" (v-float v-float v-float v-float)
         v-vec4)


(v-defun v!bool (x y) "bvec3(~a,~a)" (v-bool v-bvec2) v-bvec3)
(v-defun v!bool (x y) "bvec3(~a,~a)" (v-bvec2 v-bool) v-bvec3)

(v-defun v!bool (x y) "bvec4(~a,~a)" (v-bvec2 v-bvec2) v-bvec4)
(v-defun v!bool (x y) "bvec4(~a,~a)" (v-bool v-bvec3) v-bvec4)
(v-defun v!bool (x y) "bvec4(~a,~a)" (v-bvec3 v-bool) v-bvec4)

(v-defun v!bool (x y z) "bvec4(~a,~a,~a)" (v-bvec2 v-bool v-bool) v-bvec4)
(v-defun v!bool (x y z) "bvec4(~a,~a,~a)" (v-bool v-bvec2 v-bool) v-bvec4)
(v-defun v!bool (x y z) "bvec4(~a,~a,~a)" (v-bool v-bool v-bvec2) v-bvec4)

(v-defun v!bool (x y) "bvec2(~a,~a)" (v-bool v-bool) v-bvec2)
(v-defun v!bool (x y z) "bvec3(~a,~a,~a)" (v-bool v-bool v-bool) v-bvec3)
(v-defun v!bool (x y z w) "bvec4(~a,~a,~a,~a)" (v-bool v-bool v-bool v-bool)
         v-bvec4)


(v-defun v!int (x) "~a" (v-ivec2) v-ivec2)
(v-defun v!int (x) "~a" (v-ivec3) v-ivec3)
(v-defun v!int (x) "~a" (v-ivec4) v-ivec4)

(v-defun v!int (x) "ivec2(~a)" (v-vec2) v-ivec2)
(v-defun v!int (x) "ivec3(~a)" (v-vec3) v-ivec3)
(v-defun v!int (x) "ivec4(~a)" (v-vec4) v-ivec4)

(v-defun v!int (x y) "ivec3(~a,~a)" (v-int v-ivec2) v-ivec3)
(v-defun v!int (x y) "ivec3(~a,~a)" (v-ivec2 v-int) v-ivec3)

(v-defun v!int (x y) "ivec4(~a,~a)" (v-ivec2 v-ivec2) v-ivec4)
(v-defun v!int (x y) "ivec4(~a,~a)" (v-int v-ivec3) v-ivec4)
(v-defun v!int (x y) "ivec4(~a,~a)" (v-ivec3 v-int) v-ivec4)

(v-defun v!int (x y z) "ivec4(~a,~a,~a)" (v-ivec2 v-int v-int) v-ivec4)
(v-defun v!int (x y z) "ivec4(~a,~a,~a)" (v-int v-ivec2 v-int) v-ivec4)
(v-defun v!int (x y z) "ivec4(~a,~a,~a)" (v-int v-int v-ivec2) v-ivec4)

(v-defun v!int (x y) "ivec2(~a,~a)" (v-int v-int) v-ivec2)
(v-defun v!int (x y z) "ivec3(~a,~a,~a)" (v-int v-int v-int) v-ivec3)
(v-defun v!int (x y z w) "ivec4(~a,~a,~a,~a)" (v-int v-int v-int v-int)
         v-ivec4)


(v-defun v!uint (x y) "uvec3(~a,~a)" (v-uint v-uvec2) v-uvec3)
(v-defun v!uint (x y) "uvec3(~a,~a)" (v-uvec2 v-uint) v-uvec3)

(v-defun v!uint (x y) "uvec4(~a,~a)" (v-uvec2 v-uvec2) v-uvec4)
(v-defun v!uint (x y) "uvec4(~a,~a)" (v-uint v-uvec3) v-uvec4)
(v-defun v!uint (x y) "uvec4(~a,~a)" (v-uvec3 v-uint) v-uvec4)

(v-defun v!uint (x y z) "uvec4(~a,~a,~a)" (v-uvec2 v-uint v-uint) v-uvec4)
(v-defun v!uint (x y z) "uvec4(~a,~a,~a)" (v-uint v-uvec2 v-uint) v-uvec4)
(v-defun v!uint (x y z) "uvec4(~a,~a,~a)" (v-uint v-uint v-uvec2) v-uvec4)

(v-defun v!uint (x y) "uvec2(~a,~a)" (v-uint v-uint) v-uvec2)
(v-defun v!uint (x y z) "uvec3(~a,~a,~a)" (v-uint v-uint v-uint) v-uvec3)
(v-defun v!uint (x y z w) "uvec4(~a,~a,~a,~a)" (v-uint v-uint v-uint v-uint)
         v-uvec4)


(v-defun v!double (x y) "dvec3(~a,~a)" (v-double v-dvec2) v-dvec3)
(v-defun v!double (x y) "dvec3(~a,~a)" (v-dvec2 v-double) v-dvec3)

(v-defun v!double (x y) "dvec4(~a,~a)" (v-dvec2 v-dvec2) v-dvec4)
(v-defun v!double (x y) "dvec4(~a,~a)" (v-double v-dvec3) v-dvec4)
(v-defun v!double (x y) "dvec4(~a,~a)" (v-dvec3 v-double) v-dvec4)

(v-defun v!double (x y z) "dvec4(~a,~a,~a)" (v-dvec2 v-double v-double) v-dvec4)
(v-defun v!double (x y z) "dvec4(~a,~a,~a)" (v-double v-dvec2 v-double) v-dvec4)
(v-defun v!double (x y z) "dvec4(~a,~a,~a)" (v-double v-double v-dvec2) v-dvec4)

(v-defun v!double (x y) "dvec2(~a,~a)" (v-double v-double) v-dvec2)
(v-defun v!double (x y z) "dvec3(~a,~a,~a)" (v-double v-double v-double) v-dvec3)
(v-defun v!double (x y z w) "dvec4(~a,~a,~a,~a)" (v-double v-double v-double v-double)
         v-dvec4)




(v-defun m! (a b c d) "mat2(~a,~a,~a,~a)" (v-float v-float v-float v-float)
         v-mat2)
(v-defun m! (a b) "mat2(~a,~a)" (v-vec2 v-vec2) v-mat2)

(v-defun m! (a b c d e f g h i) "mat3(~a,~a,~a,~a,~a,~a,~a,~a,~a)"
         (v-float v-float v-float v-float v-float
                  v-float v-float v-float v-float) v-mat3)
(v-defun m! (a b c) "mat3(~a,~a,~a)" (v-vec3 v-vec3 v-vec3) v-mat3
         )

(v-defun m! (a b c d e f g h i j k l m n o p)
  "mat4(~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a)"
  (v-float v-float v-float v-float v-float v-float v-float v-float v-float
           v-float v-float v-float v-float v-float v-float v-float)
  v-mat4)
(v-defun m! (a b c d)
  "mat4(~a,~a,~a,~a)" (v-vec4 v-vec4 v-vec4 v-vec4) v-mat4)

(v-defun v-not (x &context (:330 :440)) "not(~a)"
         (v-bvector) 0)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-bvector v-bvector)
  0)
(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-float v-float)
  v-bool)
(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-vec2 v-vec2)
  v-bvec2)
(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-vec3 v-vec3)
  v-bvec3)
(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-vec4 v-vec4)
  v-bvec4)

(v-defun v-not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-bvector v-bvector) 0)
(v-defun v-not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-float v-float) v-bool)
(v-defun v-not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-vec2 v-vec2) v-bvec2)
(v-defun v-not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-vec3 v-vec3) v-bvec3)
(v-defun v-not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-vec4 v-vec4) v-bvec4)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-int v-int)
  v-bool)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-ivec2 v-ivec2)
  v-bvec2)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-ivec3 v-ivec3)
  v-bvec3)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-ivec4 v-ivec4)
  v-bvec4)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-uint v-uint)
  v-bool)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-uvec2 v-uvec2)
  v-bvec2)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-uvec3 v-uvec3)
  v-bvec3)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-uvec4 v-uvec4)
  v-bvec4)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-double v-double) v-bool)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-dvec2 v-dvec2)
  v-bvec2)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-dvec3 v-dvec3)
  v-bvec3)

(v-defun v-equal (x y &context (:330 :440))
  "equal(~a,~a)"
  (v-dvec4 v-dvec4)
  v-bvec4)

(v-defun atomic-counter (c &context (:330 :440))
  "atomicCounter(~a)"
  (v-atomic-uint) v-uint)

(v-defun expt (x y) "pow(~a, ~a)" (v-vec4 v-vec4) v-vec4)
(v-defun expt (x y) "pow(~a, ~a)" (v-vec3 v-vec3) v-vec3)
(v-defun expt (x y) "pow(~a, ~a)" (v-vec2 v-vec2) v-vec2)
(v-defun expt (x y) "pow(~a, ~a)" (v-float v-float) v-float)
