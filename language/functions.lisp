(in-package :varjo)

;; (V-DEFUN LDEXP (X TI &CONTEXT (:330 :440)) "ldexp(~a,~a)" (TFD IN) 0)
;; (V-DEFUN BIT-COUNT (VALUE &CONTEXT (:330 :440)) "bitCount(~a)" (TIU) (#'TI 0))
;; (V-DEFUN FIND-L-S-B (VALUE &CONTEXT (:330 :440)) "findLSB(~a)" (TIU) (#'TI 0))
;; (V-DEFUN FIND-M-S-B (VALUE &CONTEXT (:330 :440)) "findMSB(~a)" (TIU) (#'TI 0))
;; (V-DEFUN FLOAT-BITS-TO-INT (VALUE &CONTEXT (:330 :440)) "floatBitsToInt(~a)" (TF) (#'TI 0))
;; (V-DEFUN FLOAT-BITS-TO-UINT (VALUE &CONTEXT (:330 :440)) "floatBitsToUint(~a)" (TF) (#'TU 0))
;; (V-DEFUN NOISEN (X &CONTEXT (:330 :440)) "noisen(~a)" (TF) (#'VECN 0))
;; (V-DEFUN INT-BITS-TO-FLOAT (VALUE &CONTEXT (:330 :440)) "intBitsToFloat(~a)" (TI) &)
;; (V-DEFUN UINT-BITS-TO-FLOAT (VALUE &CONTEXT (:330 :440)) "uintBitsToFloat(~a)" (TF) &)

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

(v-defun clamp (x minval maxval &context (:330 :440))
  "clamp(~a,~a,~a)"
  (v-tiu v-tiu v-tiu) 0 :glsl-spec-matching t)

(v-defun max (x y &context (:330 :440))
  "max(~a,~a)"
  (v-tiu v-tiu) 0
  :glsl-spec-matching t)

(v-defun min (x y &context (:330 :440))
  "min(~a,~a)"
  (v-tiu v-tiu) 0
  :glsl-spec-matching t)

(v-defun bitfield-insert (base insert offset bits &context (:330 :440))
  "bitfieldInsert(~a,~a,~a,~a)"
  (v-tiu v-tiu v-int v-int) 0 :glsl-spec-matching t)

(v-defun clamp (x minval maxval &context (:330 :440))
  "clamp(~a,~a,~a)"
  (v-tfd v-tfd v-tfd) 0 :glsl-spec-matching t)

(v-defun max (x y &context (:330 :440))
  "max(~a,~a)"
  (v-tfd v-tfd) 0
  :glsl-spec-matching t)

(v-defun min (x y &context (:330 :440))
  "min(~a,~a)"
  (v-tfd v-tfd) 0
  :glsl-spec-matching t)

(v-defun mix (x y a &context (:330 :440))
  "mix(~a,~a,~a)"
  (v-tfd v-tfd v-tb) 0 :glsl-spec-matching t)

(v-defun mix (x y a &context (:330 :440))
  "mix(~a,~a,~a)"
  (v-tfd v-tfd v-tfd) 0 :glsl-spec-matching t)

(v-defun mod (x y &context (:330 :440))
  "mod(~a,~a)"
  (v-tfd v-tfd) 0
  :glsl-spec-matching t)

(v-defun faceforward (n i nref &context (:330 :440))
  "faceforward(~a,~a,~a)"
  (v-tfd v-tfd v-tfd) 0 :glsl-spec-matching t)

(v-defun fma (a b c &context (:330 :440))
  "fma(~a,~a,~a)"
  (v-tfd v-tfd v-tfd) 0 :glsl-spec-matching t)

(v-defun reflect (i n &context (:330 :440))
  "reflect(~a,~a)"
  (v-tfd v-tfd)
  0 :glsl-spec-matching t)

(v-defun refract (i n eta &context (:330 :440))
  "refract(~a,~a,~a)"
  (v-tfd v-tfd v-float) 0 :glsl-spec-matching t)

(v-defun smoothstep (edge0 edge1 x &context (:330 :440))
  "smoothstep(~a,~a,~a)"
  (v-tfd v-tfd v-tfd) 0 :glsl-spec-matching t)

(v-defun step (edge x &context (:330 :440))
  "step(~a,~a)"
  (v-tfd v-tfd) 0
  :glsl-spec-matching t)

(v-defun mix (x y a &context (:330 :440))
  "mix(~a,~a,~a)"
  (v-td v-td v-double) 0 :glsl-spec-matching t)

(v-defun mix (x y a &context (:330 :440))
  "mix(~a,~a,~a)"
  (v-tf v-tf v-float) 0 :glsl-spec-matching t)

(v-defun atan (y x &context (:330 :440))
  "atan(~a,~a)"
  (v-tf v-tf) 0
  :glsl-spec-matching t)

(v-defun pow (x y &context (:330 :440))
  "pow(~a,~a)"
  (v-tf v-tf) 0 :glsl-spec-matching t)

(v-defun isinf (x &context (:330 :440))
  "isinf(~a)"
  (v-float) v-bool
  :glsl-spec-matching t)

(v-defun isinf (x &context (:330 :440))
  "isinf(~a)"
  (v-double) v-bool
  :glsl-spec-matching t)

(v-defun isinf (x &context (:330 :440))
  "isinf(~a)"
  (v-vec2) v-bvec2
  :glsl-spec-matching t)

(v-defun isinf (x &context (:330 :440))
  "isinf(~a)"
  (v-dvec2) v-bvec2
  :glsl-spec-matching t)

(v-defun isinf (x &context (:330 :440))
  "isinf(~a)"
  (v-vec3) v-bvec3
  :glsl-spec-matching t)

(v-defun isinf (x &context (:330 :440))
  "isinf(~a)"
  (v-dvec3) v-bvec3
  :glsl-spec-matching t)

(v-defun isinf (x &context (:330 :440))
  "isinf(~a)"
  (v-vec4) v-bvec4
  :glsl-spec-matching t)

(v-defun isinf (x &context (:330 :440))
  "isinf(~a)"
  (v-dvec4) v-bvec4
  :glsl-spec-matching t)

(v-defun isnan (x &context (:330 :440))
  "isnan(~a)"
  (v-float) v-bool
  :glsl-spec-matching t)

(v-defun isnan (x &context (:330 :440))
  "isnan(~a)"
  (v-double) v-bool
  :glsl-spec-matching t)

(v-defun isnan (x &context (:330 :440))
  "isnan(~a)"
  (v-vec2) v-bvec2
  :glsl-spec-matching t)

(v-defun isnan (x &context (:330 :440))
  "isnan(~a)"
  (v-dvec2) v-bvec2
  :glsl-spec-matching t)

(v-defun isnan (x &context (:330 :440))
  "isnan(~a)"
  (v-vec3) v-bvec3
  :glsl-spec-matching t)

(v-defun isnan (x &context (:330 :440))
  "isnan(~a)"
  (v-dvec3) v-bvec3
  :glsl-spec-matching t)

(v-defun isnan (x &context (:330 :440))
  "isnan(~a)"
  (v-vec4) v-bvec4
  :glsl-spec-matching t)

(v-defun isnan (x &context (:330 :440))
  "isnan(~a)"
  (v-dvec4) v-bvec4
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

(v-defun greater-than (x y &context (:330 :440))
  "greaterThan(~a,~a)"
  (v-float v-float) v-bool :glsl-spec-matching t)

(v-defun greater-than (x y &context (:330 :440))
  "greaterThan(~a,~a)"
  (v-vec2 v-vec2) v-bvec2 :glsl-spec-matching t)

(v-defun greater-than (x y &context (:330 :440))
  "greaterThan(~a,~a)"
  (v-vec3 v-vec3) v-bvec3 :glsl-spec-matching t)

(v-defun greater-than (x y &context (:330 :440))
  "greaterThan(~a,~a)"
  (v-vec4 v-vec4) v-bvec4 :glsl-spec-matching t)

(v-defun greater-than-equal (x y &context (:330 :440))
  "greaterThanEqual(~a,~a)"
  (v-float v-float) v-bool :glsl-spec-matching t)

(v-defun greater-than-equal (x y &context (:330 :440))
  "greaterThanEqual(~a,~a)"
  (v-vec2 v-vec2) v-bvec2 :glsl-spec-matching t)

(v-defun greater-than-equal (x y &context (:330 :440))
  "greaterThanEqual(~a,~a)"
  (v-vec3 v-vec3) v-bvec3 :glsl-spec-matching t)

(v-defun greater-than-equal (x y &context (:330 :440))
  "greaterThanEqual(~a,~a)"
  (v-vec4 v-vec4) v-bvec4 :glsl-spec-matching t)

(v-defun less-than (x y &context (:330 :440))
  "lessThan(~a,~a)"
  (v-float v-float) v-bool :glsl-spec-matching t)

(v-defun less-than (x y &context (:330 :440))
  "lessThan(~a,~a)"
  (v-vec2 v-vec2) v-bvec2 :glsl-spec-matching t)

(v-defun less-than (x y &context (:330 :440))
  "lessThan(~a,~a)"
  (v-vec3 v-vec3) v-bvec3 :glsl-spec-matching t)

(v-defun less-than (x y &context (:330 :440))
  "lessThan(~a,~a)"
  (v-vec4 v-vec4) v-bvec4 :glsl-spec-matching t)

(v-defun less-than-equal (x y &context (:330 :440))
  "lessThanEqual(~a,~a)"
  (v-float v-float) v-bool :glsl-spec-matching t)

(v-defun less-than-equal (x y &context (:330 :440))
  "lessThanEqual(~a,~a)"
  (v-vec2 v-vec2) v-bvec2 :glsl-spec-matching t)

(v-defun less-than-equal (x y &context (:330 :440))
  "lessThanEqual(~a,~a)"
  (v-vec3 v-vec3) v-bvec3 :glsl-spec-matching t)

(v-defun less-than-equal (x y &context (:330 :440))
  "lessThanEqual(~a,~a)"
  (v-vec4 v-vec4) v-bvec4 :glsl-spec-matching t)

(v-defun equal (x y &context (:330 :440))
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

(v-defun not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-int v-int) v-bool :glsl-spec-matching t)

(v-defun not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-ivec2 v-ivec2) v-bvec2 :glsl-spec-matching t)

(v-defun not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-ivec3 v-ivec3) v-bvec3 :glsl-spec-matching t)

(v-defun not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-ivec4 v-ivec4) v-bvec4 :glsl-spec-matching t)

(v-defun greater-than (x y &context (:330 :440))
  "greaterThan(~a,~a)"
  (v-int v-int) v-bool :glsl-spec-matching t)

(v-defun greater-than (x y &context (:330 :440))
  "greaterThan(~a,~a)"
  (v-ivec2 v-ivec2) v-bvec2 :glsl-spec-matching t)

(v-defun greater-than (x y &context (:330 :440))
  "greaterThan(~a,~a)"
  (v-ivec3 v-ivec3) v-bvec3 :glsl-spec-matching t)

(v-defun greater-than (x y &context (:330 :440))
  "greaterThan(~a,~a)"
  (v-ivec4 v-ivec4) v-bvec4 :glsl-spec-matching t)

(v-defun greater-than-equal (x y &context (:330 :440))
  "greaterThanEqual(~a,~a)"
  (v-int v-int) v-bool :glsl-spec-matching t)

(v-defun greater-than-equal (x y &context (:330 :440))
  "greaterThanEqual(~a,~a)"
  (v-ivec2 v-ivec2) v-bvec2 :glsl-spec-matching t)

(v-defun greater-than-equal (x y &context (:330 :440))
  "greaterThanEqual(~a,~a)"
  (v-ivec3 v-ivec3) v-bvec3 :glsl-spec-matching t)

(v-defun greater-than-equal (x y &context (:330 :440))
  "greaterThanEqual(~a,~a)"
  (v-ivec4 v-ivec4) v-bvec4 :glsl-spec-matching t)

(v-defun less-than (x y &context (:330 :440))
  "lessThan(~a,~a)"
  (v-int v-int) v-bool :glsl-spec-matching t)

(v-defun less-than (x y &context (:330 :440))
  "lessThan(~a,~a)"
  (v-ivec2 v-ivec2) v-bvec2 :glsl-spec-matching t)

(v-defun less-than (x y &context (:330 :440))
  "lessThan(~a,~a)"
  (v-ivec3 v-ivec3) v-bvec3 :glsl-spec-matching t)

(v-defun less-than (x y &context (:330 :440))
  "lessThan(~a,~a)"
  (v-ivec4 v-ivec4) v-bvec4 :glsl-spec-matching t)

(v-defun less-than-equal (x y &context (:330 :440))
  "lessThanEqual(~a,~a)"
  (v-int v-int) v-bool :glsl-spec-matching t)

(v-defun less-than-equal (x y &context (:330 :440))
  "lessThanEqual(~a,~a)"
  (v-ivec2 v-ivec2) v-bvec2 :glsl-spec-matching t)

(v-defun less-than-equal (x y &context (:330 :440))
  "lessThanEqual(~a,~a)"
  (v-ivec3 v-ivec3) v-bvec3 :glsl-spec-matching t)

(v-defun less-than-equal (x y &context (:330 :440))
  "lessThanEqual(~a,~a)"
  (v-ivec4 v-ivec4) v-bvec4 :glsl-spec-matching t)

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

(v-defun not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-uint v-uint) v-bool :glsl-spec-matching t)

(v-defun not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-uvec2 v-uvec2) v-bvec2 :glsl-spec-matching t)

(v-defun not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-uvec3 v-uvec3) v-bvec3 :glsl-spec-matching t)

(v-defun not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-uvec4 v-uvec4) v-bvec4 :glsl-spec-matching t)

(v-defun greater-than (x y &context (:330 :440))
  "greaterThan(~a,~a)"
  (v-uint v-uint) v-bool :glsl-spec-matching t)

(v-defun greater-than (x y &context (:330 :440))
  "greaterThan(~a,~a)"
  (v-uvec2 v-uvec2) v-bvec2 :glsl-spec-matching t)

(v-defun greater-than (x y &context (:330 :440))
  "greaterThan(~a,~a)"
  (v-uvec3 v-uvec3) v-bvec3 :glsl-spec-matching t)

(v-defun greater-than (x y &context (:330 :440))
  "greaterThan(~a,~a)"
  (v-uvec4 v-uvec4) v-bvec4 :glsl-spec-matching t)

(v-defun greater-than-equal (x y &context (:330 :440))
  "greaterThanEqual(~a,~a)"
  (v-uint v-uint) v-bool :glsl-spec-matching t)

(v-defun greater-than-equal (x y &context (:330 :440))
  "greaterThanEqual(~a,~a)"
  (v-uvec2 v-uvec2) v-bvec2 :glsl-spec-matching t)

(v-defun greater-than-equal (x y &context (:330 :440))
  "greaterThanEqual(~a,~a)"
  (v-uvec3 v-uvec3) v-bvec3 :glsl-spec-matching t)

(v-defun greater-than-equal (x y &context (:330 :440))
  "greaterThanEqual(~a,~a)"
  (v-uvec4 v-uvec4) v-bvec4 :glsl-spec-matching t)

(v-defun less-than (x y &context (:330 :440))
  "lessThan(~a,~a)"
  (v-uint v-uint) v-bool :glsl-spec-matching t)

(v-defun less-than (x y &context (:330 :440))
  "lessThan(~a,~a)"
  (v-uvec2 v-uvec2) v-bvec2 :glsl-spec-matching t)

(v-defun less-than (x y &context (:330 :440))
  "lessThan(~a,~a)"
  (v-uvec3 v-uvec3) v-bvec3 :glsl-spec-matching t)

(v-defun less-than (x y &context (:330 :440))
  "lessThan(~a,~a)"
  (v-uvec4 v-uvec4) v-bvec4 :glsl-spec-matching t)

(v-defun less-than-equal (x y &context (:330 :440))
  "lessThanEqual(~a,~a)"
  (v-uint v-uint) v-bool :glsl-spec-matching t)

(v-defun less-than-equal (x y &context (:330 :440))
  "lessThanEqual(~a,~a)"
  (v-uvec2 v-uvec2) v-bvec2 :glsl-spec-matching t)

(v-defun less-than-equal (x y &context (:330 :440))
  "lessThanEqual(~a,~a)"
  (v-uvec3 v-uvec3) v-bvec3 :glsl-spec-matching t)

(v-defun less-than-equal (x y &context (:330 :440))
  "lessThanEqual(~a,~a)"
  (v-uvec4 v-uvec4) v-bvec4 :glsl-spec-matching t)

(v-defun equal (x y &context (:330 :440))
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

(v-defun not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-double v-double) v-bool :glsl-spec-matching t)

(v-defun not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-dvec2 v-dvec2) v-bvec2 :glsl-spec-matching t)

(v-defun not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-dvec3 v-dvec3) v-bvec3 :glsl-spec-matching t)

(v-defun not-equal (x y &context (:330 :440))
  "notEqual(~a,~a)"
  (v-dvec4 v-dvec4) v-bvec4 :glsl-spec-matching t)

(v-defun greater-than (x y &context (:330 :440))
  "greaterThan(~a,~a)"
  (v-double v-double) v-bool :glsl-spec-matching t)

(v-defun greater-than (x y &context (:330 :440))
  "greaterThan(~a,~a)"
  (v-dvec2 v-dvec2) v-bvec2 :glsl-spec-matching t)

(v-defun greater-than (x y &context (:330 :440))
  "greaterThan(~a,~a)"
  (v-dvec3 v-dvec3) v-bvec3 :glsl-spec-matching t)

(v-defun greater-than (x y &context (:330 :440))
  "greaterThan(~a,~a)"
  (v-dvec4 v-dvec4) v-bvec4 :glsl-spec-matching t)

(v-defun greater-than-equal (x y &context (:330 :440))
  "greaterThanEqual(~a,~a)"
  (v-double v-double) v-bool :glsl-spec-matching t)

(v-defun greater-than-equal (x y &context (:330 :440))
  "greaterThanEqual(~a,~a)"
  (v-dvec2 v-dvec2) v-bvec2 :glsl-spec-matching t)

(v-defun greater-than-equal (x y &context (:330 :440))
  "greaterThanEqual(~a,~a)"
  (v-dvec3 v-dvec3) v-bvec3 :glsl-spec-matching t)

(v-defun greater-than-equal (x y &context (:330 :440))
  "greaterThanEqual(~a,~a)"
  (v-dvec4 v-dvec4) v-bvec4 :glsl-spec-matching t)

(v-defun less-than (x y &context (:330 :440))
  "lessThan(~a,~a)"
  (v-double v-double) v-bool :glsl-spec-matching t)

(v-defun less-than (x y &context (:330 :440))
  "lessThan(~a,~a)"
  (v-dvec2 v-dvec2) v-bvec2 :glsl-spec-matching t)

(v-defun less-than (x y &context (:330 :440))
  "lessThan(~a,~a)"
  (v-dvec3 v-dvec3) v-bvec3 :glsl-spec-matching t)

(v-defun less-than (x y &context (:330 :440))
  "lessThan(~a,~a)"
  (v-dvec4 v-dvec4) v-bvec4 :glsl-spec-matching t)

(v-defun less-than-equal (x y &context (:330 :440))
  "lessThanEqual(~a,~a)"
  (v-double v-double) v-bool :glsl-spec-matching t)

(v-defun less-than-equal (x y &context (:330 :440))
  "lessThanEqual(~a,~a)"
  (v-dvec2 v-dvec2) v-bvec2 :glsl-spec-matching t)

(v-defun less-than-equal (x y &context (:330 :440))
  "lessThanEqual(~a,~a)"
  (v-dvec3 v-dvec3) v-bvec3 :glsl-spec-matching t)

(v-defun less-than-equal (x y &context (:330 :440))
  "lessThanEqual(~a,~a)"
  (v-dvec4 v-dvec4) v-bvec4 :glsl-spec-matching t)

(v-defun abs (x &context (:330 :440))
  "abs(~a)"
  (v-tfd) 0 :glsl-spec-matching t)

(v-defun abs (x &context (:330 :440))
  "abs(~a)"
  (v-ti) 0 :glsl-spec-matching t)

(v-defun acos (x &context (:330 :440))
  "acos(~a)"
  (v-tf) 0 :glsl-spec-matching t)

(v-defun acosh (x &context (:330 :440))
  "acosh(~a)"
  (v-tf) 0 :glsl-spec-matching t)

(v-defun all (x &context (:330 :440))
  "all(~a)"
  (v-bvector) v-bool :glsl-spec-matching t)

(v-defun any (x &context (:330 :440))
  "any(~a)"
  (v-bvector) v-bool :glsl-spec-matching t)

(v-defun asin (x &context (:330 :440))
  "asin(~a)"
  (v-tf) 0 :glsl-spec-matching t)

(v-defun asinh (x &context (:330 :440))
  "asinh(~a)"
  (v-tf) 0 :glsl-spec-matching t)

(v-defun atan (y_over_x &context (:330 :440))
  "atan(~a)"
  (v-tf) 0 :glsl-spec-matching t)

(v-defun atanh (x &context (:330 :440))
  "atanh(~a)"
  (v-tf) 0 :glsl-spec-matching t)

(v-defun atomic-counter (c &context (:330 :440))
  "atomicCounter(~a)"
  (v-atomic_uint) v-uint :glsl-spec-matching t)

(v-defun atomic-counter-decrement (c &context (:330 :440))
  "atomicCounterDecrement(~a)"
  (v-atomic_uint) v-uint :glsl-spec-matching t)

(v-defun atomic-counter-increment (c &context (:330 :440))
  "atomicCounterIncrement(~a)"
  (v-atomic_uint) v-uint :glsl-spec-matching t)

(v-defun barrier (&context (:330 :440))
  "barrier()"
  nil v-void :glsl-spec-matching t)

(v-defun bitfield-extract (value offset bits &context (:330 :440))
  "bitfieldExtract(~a,~a,~a)"
  (v-tiu v-int v-int) 0 :glsl-spec-matching t)

(v-defun bitfield-reverse (value &context (:330 :440))
  "bitfieldReverse(~a)"
  (v-tiu) 0 :glsl-spec-matching t)

(v-defun ceil (x &context (:330 :440))
  "ceil(~a)"
  (v-tfd) 0 :glsl-spec-matching t)

(v-defun clamp (x minval maxval &context (:330 :440))
  "clamp(~a,~a,~a)"
  (v-td v-double v-double) 0 :glsl-spec-matching t)

(v-defun clamp (x minval maxval &context (:330 :440))
  "clamp(~a,~a,~a)"
  (v-tf v-float v-float) 0 :glsl-spec-matching t)

(v-defun clamp (x minval maxval &context (:330 :440))
  "clamp(~a,~a,~a)"
  (v-ti v-int v-int) 0 :glsl-spec-matching t)

(v-defun clamp (x minval maxval &context (:330 :440))
  "clamp(~a,~a,~a)"
  (v-tu v-uint v-uint) 0 :glsl-spec-matching t)

(v-defun cos (angle &context (:330 :440))
  "cos(~a)"
  (v-tf) 0 :glsl-spec-matching t)

(v-defun cosh (x &context (:330 :440))
  "cosh(~a)"
  (v-tf) 0 :glsl-spec-matching t)

(v-defun cross (x y &context (:330 :440))
  "cross(~a,~a)"
  (v-dvec3 v-dvec3)
  v-dvec3 :glsl-spec-matching t)

(v-defun cross (x y &context (:330 :440))
  "cross(~a,~a)"
  (v-vec3 v-vec3)
  v-vec3 :glsl-spec-matching t)

(v-defun d-fdx (p &context (:330 :440))
  "dFdx(~a)"
  (v-tf) 0 :glsl-spec-matching t)

(v-defun d-fdy (p &context (:330 :440))
  "dFdy(~a)"
  (v-tf) 0 :glsl-spec-matching t)

(v-defun degrees (radians &context (:330 :440))
  "degrees(~a)"
  (v-tf) 0
  :glsl-spec-matching t)

(v-defun determinant (m &context (:330 :440))
  "determinant(~a)"
  (v-dmatn)
  v-double :glsl-spec-matching t)

(v-defun determinant (m &context (:330 :440))
  "determinant(~a)"
  (v-matn)
  v-float :glsl-spec-matching t)

(v-defun distance (p0 p1 &context (:330 :440))
  "distance(~a,~a)"
  (v-td v-td) v-double :glsl-spec-matching t)

(v-defun distance (p0 p1 &context (:330 :440))
  "distance(~a,~a)"
  (v-tf v-tf) v-float :glsl-spec-matching t)

(v-defun dot (x y &context (:330 :440))
  "dot(~a,~a)"
  (v-td v-td) v-double
  :glsl-spec-matching t)

(v-defun dot (x y &context (:330 :440))
  "dot(~a,~a)"
  (v-tf v-tf) v-float
  :glsl-spec-matching t)

(v-defun emit-stream-vertex (stream &context (:330 :440))
  "EmitStreamVertex(~a)"
  (v-int) v-void :glsl-spec-matching t)

(v-defun emit-vertex (&context (:330 :440))
  "EmitVertex()"
  nil v-void
  :glsl-spec-matching t)

(v-defun end-primitive (&context (:330 :440))
  "EndPrimitive()"
  nil v-void
  :glsl-spec-matching t)

(v-defun end-stream-primitive (stream &context (:330 :440))
  "EndStreamPrimitive(~a)"
  (v-int) v-void :glsl-spec-matching t)

(v-defun exp (x &context (:330 :440))
  "exp(~a)"
  (v-tf) 0 :glsl-spec-matching t)

(v-defun exp-2 (x &context (:330 :440))
  "exp2(~a)"
  (v-tf) 0 :glsl-spec-matching t)

(v-defun floor (x &context (:330 :440))
  "floor(~a)"
  (v-tfd) 0 :glsl-spec-matching t)

(v-defun fract (x &context (:330 :440))
  "fract(~a)"
  (v-tfd) 0 :glsl-spec-matching t)

(v-defun fwidth (p &context (:330 :440))
  "fwidth(~a)"
  (v-tf) 0 :glsl-spec-matching t)

(v-defun group-memory-barrier (&context (:330 :440))
  "groupMemoryBarrier()"
  nil v-void :glsl-spec-matching t)

(v-defun image-size (image &context (:330 :440))
  "imageSize(~a)"
  (v-gimage1d) v-int :glsl-spec-matching t)

(v-defun image-size (image &context (:330 :440))
  "imageSize(~a)"
  (v-gimagebuffer) v-int :glsl-spec-matching t)

(v-defun image-size (image &context (:330 :440))
  "imageSize(~a)"
  (v-gimage2d) v-ivec2 :glsl-spec-matching t)

(v-defun image-size (image &context (:330 :440))
  "imageSize(~a)"
  (v-gimagecube) v-ivec2 :glsl-spec-matching t)

(v-defun image-size (image &context (:330 :440))
  "imageSize(~a)"
  (v-gimagerect) v-ivec2 :glsl-spec-matching t)

(v-defun image-size (image &context (:330 :440))
  "imageSize(~a)"
  (v-gimage1darray) v-ivec2 :glsl-spec-matching t)

(v-defun image-size (image &context (:330 :440))
  "imageSize(~a)"
  (v-gimage2dms) v-ivec3 :glsl-spec-matching t)

(v-defun image-size (image &context (:330 :440))
  "imageSize(~a)"
  (v-gimage2darray) v-ivec3 :glsl-spec-matching t)

(v-defun image-size (image &context (:330 :440))
  "imageSize(~a)"
  (v-gimagecube2darray) v-ivec3 :glsl-spec-matching t)

(v-defun image-size (image &context (:330 :440))
  "imageSize(~a)"
  (v-gimage2dmsarray) v-ivec3 :glsl-spec-matching t)

(v-defun image-size (image &context (:330 :440))
  "imageSize(~a)"
  (v-gimage3d) v-vec3 :glsl-spec-matching t)

(v-defun interpolate-at-centroid (interpolant &context (:330 :440))
  "interpolateAtCentroid(~a)"
  (v-tf) 0 :glsl-spec-matching t)

(v-defun interpolate-at-offset (interpolant offset &context (:330 :440))
  "interpolateAtOffset(~a,~a)"
  (v-tf v-vec2) 0 :glsl-spec-matching t)

(v-defun interpolate-at-sample (interpolant sample &context (:330 :440))
  "interpolateAtSample(~a,~a)"
  (v-tf v-int) 0 :glsl-spec-matching t)

(v-defun inverse (m &context (:330 :440))
  "inverse(~a)"
  (v-dmatn) 0 :glsl-spec-matching t)

(v-defun inverse (m &context (:330 :440))
  "inverse(~a)"
  (v-matn) 0 :glsl-spec-matching t)

(v-defun inversesqrt (x &context (:330 :440))
  "inversesqrt(~a)"
  (v-tfd) 0
  :glsl-spec-matching t)

(v-defun length (x &context (:330 :440))
  "length(~a)"
  (v-td) v-double
  :glsl-spec-matching t)

(v-defun length (x &context (:330 :440))
  "length(~a)"
  (v-tf) v-float
  :glsl-spec-matching t)

(v-defun log (x &context (:330 :440))
  "log(~a)"
  (v-tf) 0 :glsl-spec-matching t)

(v-defun log-2 (x &context (:330 :440))
  "log2(~a)"
  (v-tf) 0 :glsl-spec-matching t)

(v-defun matrix-comp-mult (x y &context (:330 :440))
  "matrixCompMult(~a,~a)"
  (v-dmatn v-dmatn) v-dmatn :glsl-spec-matching t)

(v-defun matrix-comp-mult (x y &context (:330 :440))
  "matrixCompMult(~a,~a)"
  (v-matn v-matn) v-matn :glsl-spec-matching t)

(v-defun max (x y &context (:330 :440))
  "max(~a,~a)"
  (v-td v-double) 0
  :glsl-spec-matching t)

(v-defun max (x y &context (:330 :440))
  "max(~a,~a)"
  (v-tf v-float) 0
  :glsl-spec-matching t)

(v-defun max (x y &context (:330 :440))
  "max(~a,~a)"
  (v-ti v-int) 0 :glsl-spec-matching t)

(v-defun max (x y &context (:330 :440))
  "max(~a,~a)"
  (v-tu v-uint) 0
  :glsl-spec-matching t)

(v-defun memory-barrier (&context (:330 :440))
  "memoryBarrier()"
  nil
  v-void :glsl-spec-matching t)

(v-defun memory-barrier-atomic-counter (&context (:330 :440))
  "memoryBarrierAtomicCounter()"
  nil v-void :glsl-spec-matching t)

(v-defun memory-barrier-buffer (&context (:330 :440))
  "memoryBarrierBuffer()"
  nil v-void :glsl-spec-matching t)

(v-defun memory-barrier-image (&context (:330 :440))
  "memoryBarrierImage()"
  nil v-void :glsl-spec-matching t)

(v-defun memory-barrier-shared (&context (:330 :440))
  "memoryBarrierShared()"
  nil v-void :glsl-spec-matching t)

(v-defun min (x y &context (:330 :440))
  "min(~a,~a)"
  (v-td v-double) 0
  :glsl-spec-matching t)

(v-defun min (x y &context (:330 :440))
  "min(~a,~a)"
  (v-tf v-float) 0
  :glsl-spec-matching t)

(v-defun min (x y &context (:330 :440))
  "min(~a,~a)"
  (v-ti v-int) 0 :glsl-spec-matching t)

(v-defun min (x y &context (:330 :440))
  "min(~a,~a)"
  (v-tu v-uint) 0
  :glsl-spec-matching t)

(v-defun mod (x y &context (:330 :440))
  "mod(~a,~a)"
  (v-td v-double) 0
  :glsl-spec-matching t)

(v-defun mod (x y &context (:330 :440))
  "mod(~a,~a)"
  (v-tf v-float) 0
  :glsl-spec-matching t)

(v-defun noise-1 (x &context (:330 :440))
  "noise1(~a)"
  (v-tf) v-float
  :glsl-spec-matching t)

(v-defun normalize (x &context (:330 :440))
  "normalize(~a)"
  (v-tfd) 0
  :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-vec2 v-vec2) v-mat2 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-vec3 v-vec3) v-mat3 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-vec4 v-vec4) v-mat4 :glsl-spec-matching t)

(v-defun pack-double-2x-3-2 (v &context (:330 :440))
  "packDouble2x32(~a)"
  (v-uvec2) v-double :glsl-spec-matching t)

(v-defun pack-half-2x-1-6 (v &context (:330 :440))
  "packHalf2x16(~a)"
  (v-vec2) v-uint :glsl-spec-matching t)

(v-defun pack-snorm-2x-1-6 (v &context (:330 :440))
  "packSnorm2x16(~a)"
  (v-vec2) v-uint :glsl-spec-matching t)

(v-defun pack-snorm-4x-8 (v &context (:330 :440))
  "packSnorm4x8(~a)"
  (v-vec4) v-uint :glsl-spec-matching t)

(v-defun pack-unorm-2x-1-6 (v &context (:330 :440))
  "packUnorm2x16(~a)"
  (v-vec2) v-uint :glsl-spec-matching t)

(v-defun pack-unorm-4x-8 (v &context (:330 :440))
  "packUnorm4x8(~a)"
  (v-vec4) v-uint :glsl-spec-matching t)

(v-defun radians (degrees &context (:330 :440))
  "radians(~a)"
  (v-tf) 0
  :glsl-spec-matching t)

(v-defun round (x &context (:330 :440))
  "round(~a)"
  (v-tfd) 0 :glsl-spec-matching t)

(v-defun round-even (x &context (:330 :440))
  "roundEven(~a)"
  (v-tfd) 0
  :glsl-spec-matching t)

(v-defun sign (x &context (:330 :440))
  "sign(~a)"
  (v-tfd) 0 :glsl-spec-matching t)

(v-defun sign (x &context (:330 :440))
  "sign(~a)"
  (v-ti) 0 :glsl-spec-matching t)

(v-defun sin (angle &context (:330 :440))
  "sin(~a)"
  (v-tf) 0 :glsl-spec-matching t)

(v-defun sinh (x &context (:330 :440))
  "sinh(~a)"
  (v-tf) 0 :glsl-spec-matching t)

(v-defun smoothstep (edge0 edge1 x &context (:330 :440))
  "smoothstep(~a,~a,~a)"
  (v-double v-double v-td) 2 :glsl-spec-matching t)

(v-defun smoothstep (edge0 edge1 x &context (:330 :440))
  "smoothstep(~a,~a,~a)"
  (v-float v-float v-tf) 2 :glsl-spec-matching t)

(v-defun sqrt (x &context (:330 :440))
  "sqrt(~a)"
  (v-tfd) 0 :glsl-spec-matching t)

(v-defun step (edge x &context (:330 :440))
  "step(~a,~a)"
  (v-double v-td)
  1 :glsl-spec-matching t)

(v-defun step (edge x &context (:330 :440))
  "step(~a,~a)"
  (v-float v-tf) 1
  :glsl-spec-matching t)

(v-defun tan (angle &context (:330 :440))
  "tan(~a)"
  (v-tf) 0 :glsl-spec-matching t)

(v-defun tanh (x &context (:330 :440))
  "tanh(~a)"
  (v-tf) 0 :glsl-spec-matching t)

(v-defun trunc (x &context (:330 :440))
  "trunc(~a)"
  (v-tfd) 0 :glsl-spec-matching t)

(v-defun unpack-double-2x-3-2 (v &context (:330 :440))
  "unpackDouble2x32(~a)"
  (v-double) v-uvec2 :glsl-spec-matching t)

(v-defun unpack-half-2x-1-6 (v &context (:330 :440))
  "unpackHalf2x16(~a)"
  (v-uint) v-vec2 :glsl-spec-matching t)

(v-defun unpack-snorm-2x-1-6 (p &context (:330 :440))
  "unpackSnorm2x16(~a)"
  (v-uint) v-vec2 :glsl-spec-matching t)

(v-defun unpack-snorm-4x-8 (p &context (:330 :440))
  "unpackSnorm4x8(~a)"
  (v-uint) v-vec4 :glsl-spec-matching t)

(v-defun unpack-unorm-2x-1-6 (p &context (:330 :440))
  "unpackUnorm2x16(~a)"
  (v-uint) v-vec2 :glsl-spec-matching t)

(v-defun unpack-unorm-4x-8 (p &context (:330 :440))
  "unpackUnorm4x8(~a)"
  (v-uint) v-vec4 :glsl-spec-matching t)

(v-defun transpose (m &context (:330 :440))
  "transpose(~a)"
  (v-matn) 0
  :glsl-spec-matching t)

(v-defun transpose (m &context (:330 :440))
  "transpose(~a)"
  (v-dmatn) 0
  :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-dvec2 v-dvec2) v-dmat2 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-dvec3 v-dvec3) v-dmat3 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-dvec4 v-dvec4) v-dmat4 :glsl-spec-matching t)

(v-defun transpose (m &context (:330 :440))
  "transpose(~a)"
  (v-mat2x2)
  v-mat2x2 :glsl-spec-matching t)

(v-defun transpose (m &context (:330 :440))
  "transpose(~a)"
  (v-mat3x2)
  v-mat2x3 :glsl-spec-matching t)

(v-defun transpose (m &context (:330 :440))
  "transpose(~a)"
  (v-mat4x2)
  v-mat2x4 :glsl-spec-matching t)

(v-defun transpose (m &context (:330 :440))
  "transpose(~a)"
  (v-mat2x3)
  v-mat3x2 :glsl-spec-matching t)

(v-defun transpose (m &context (:330 :440))
  "transpose(~a)"
  (v-mat3x3)
  v-mat3x3 :glsl-spec-matching t)

(v-defun transpose (m &context (:330 :440))
  "transpose(~a)"
  (v-mat4x3)
  v-mat3x4 :glsl-spec-matching t)

(v-defun transpose (m &context (:330 :440))
  "transpose(~a)"
  (v-mat2x4)
  v-mat4x2 :glsl-spec-matching t)

(v-defun transpose (m &context (:330 :440))
  "transpose(~a)"
  (v-mat3x4)
  v-mat4x3 :glsl-spec-matching t)

(v-defun transpose (m &context (:330 :440))
  "transpose(~a)"
  (v-mat4x4)
  v-mat4x4 :glsl-spec-matching t)

(v-defun transpose (m &context (:330 :440))
  "transpose(~a)"
  (v-dmat2x2)
  v-dmat2x2 :glsl-spec-matching t)

(v-defun transpose (m &context (:330 :440))
  "transpose(~a)"
  (v-dmat3x2)
  v-dmat2x3 :glsl-spec-matching t)

(v-defun transpose (m &context (:330 :440))
  "transpose(~a)"
  (v-dmat4x2)
  v-dmat2x4 :glsl-spec-matching t)

(v-defun transpose (m &context (:330 :440))
  "transpose(~a)"
  (v-dmat2x3)
  v-dmat3x2 :glsl-spec-matching t)

(v-defun transpose (m &context (:330 :440))
  "transpose(~a)"
  (v-dmat3x3)
  v-dmat3x3 :glsl-spec-matching t)

(v-defun transpose (m &context (:330 :440))
  "transpose(~a)"
  (v-dmat4x3)
  v-dmat3x4 :glsl-spec-matching t)

(v-defun transpose (m &context (:330 :440))
  "transpose(~a)"
  (v-dmat2x4)
  v-dmat4x2 :glsl-spec-matching t)

(v-defun transpose (m &context (:330 :440))
  "transpose(~a)"
  (v-dmat3x4)
  v-dmat4x3 :glsl-spec-matching t)

(v-defun transpose (m &context (:330 :440))
  "transpose(~a)"
  (v-dmat4x4)
  v-dmat4x4 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-vec2 v-vec2) v-mat2x2 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-vec3 v-vec2) v-mat3x2 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-vec4 v-vec2) v-mat4x2 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-vec2 v-vec3) v-mat2x3 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-vec3 v-vec3) v-mat3x3 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-vec4 v-vec3) v-mat4x3 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-vec2 v-vec4) v-mat2x4 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-vec3 v-vec4) v-mat3x4 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-vec4 v-vec4) v-mat4x4 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-dvec2 v-dvec2) v-dmat2x2 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-dvec3 v-dvec2) v-dmat3x2 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-dvec4 v-dvec2) v-dmat4x2 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-dvec2 v-dvec3) v-dmat2x3 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-dvec3 v-dvec3) v-dmat3x3 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-dvec4 v-dvec3) v-dmat4x3 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-dvec2 v-dvec4) v-dmat2x4 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-dvec3 v-dvec4) v-dmat3x4 :glsl-spec-matching t)

(v-defun outer-product (c r &context (:330 :440))
  "outerProduct(~a,~a)"
  (v-dvec4 v-dvec4) v-dmat4x4 :glsl-spec-matching t)

(v-defun image-atomic-add (image p data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimage1d v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-add (image p data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimage1d v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-and (image p data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimage1d v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-and (image p data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimage1d v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimage1d v-int v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimage1d v-int v-uint v-uint) v-uint
  :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimage1d v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimage1d v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-max (image p data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimage1d v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-max (image p data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimage1d v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-min (image p data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimage1d v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-min (image p data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimage1d v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-or (image p data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimage1d v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-or (image p data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimage1d v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-xor (image p data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimage1d v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-xor (image p data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimage1d v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-load (image p &context (:330 :440))
  "imageLoad(~a)"
  (v-gimage1d v-int) v-gvec4 :glsl-spec-matching t)

(v-defun image-store (image p data &context (:330 :440))
  "imageStore(~a,~a)"
  (v-gimage1d v-int v-gvec4) v-void :glsl-spec-matching t)

(v-defun image-atomic-add (image p data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimage2d v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-add (image p data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimage2d v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-and (image p data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimage2d v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-and (image p data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimage2d v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimage2d v-ivec2 v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimage2d v-ivec2 v-uint v-uint) v-uint
  :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimage2d v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimage2d v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-max (image p data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimage2d v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-max (image p data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimage2d v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-min (image p data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimage2d v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-min (image p data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimage2d v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-or (image p data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimage2d v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-or (image p data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimage2d v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-xor (image p data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimage2d v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-xor (image p data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimage2d v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-load (image p &context (:330 :440))
  "imageLoad(~a)"
  (v-gimage2d v-ivec2) v-gvec4 :glsl-spec-matching t)

(v-defun image-store (image p data &context (:330 :440))
  "imageStore(~a,~a)"
  (v-gimage2d v-ivec2 v-gvec4) v-void :glsl-spec-matching t)

(v-defun image-atomic-add (image p data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimage3d v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-add (image p data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimage3d v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-and (image p data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimage3d v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-and (image p data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimage3d v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimage3d v-ivec3 v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimage3d v-ivec3 v-uint v-uint) v-uint
  :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimage3d v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimage3d v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-max (image p data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimage3d v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-max (image p data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimage3d v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-min (image p data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimage3d v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-min (image p data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimage3d v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-or (image p data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimage3d v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-or (image p data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimage3d v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-xor (image p data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimage3d v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-xor (image p data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimage3d v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-load (image p &context (:330 :440))
  "imageLoad(~a)"
  (v-gimage3d v-ivec3) v-gvec4 :glsl-spec-matching t)

(v-defun image-store (image p data &context (:330 :440))
  "imageStore(~a,~a)"
  (v-gimage3d v-ivec3 v-gvec4) v-void :glsl-spec-matching t)

(v-defun image-atomic-add (image p data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimage2drect v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-add (image p data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimage2drect v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-and (image p data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimage2drect v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-and (image p data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimage2drect v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimage2drect v-ivec2 v-int v-int) v-int
  :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimage2drect v-ivec2 v-uint v-uint) v-uint
  :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimage2drect v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimage2drect v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-max (image p data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimage2drect v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-max (image p data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimage2drect v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-min (image p data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimage2drect v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-min (image p data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimage2drect v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-or (image p data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimage2drect v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-or (image p data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimage2drect v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-xor (image p data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimage2drect v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-xor (image p data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimage2drect v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-load (image p &context (:330 :440))
  "imageLoad(~a)"
  (v-gimage2drect v-ivec2) v-gvec4 :glsl-spec-matching t)

(v-defun image-store (image p data &context (:330 :440))
  "imageStore(~a,~a)"
  (v-gimage2drect v-ivec2 v-gvec4) v-void :glsl-spec-matching t)

(v-defun image-atomic-add (image p data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimagecube v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-add (image p data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimagecube v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-and (image p data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimagecube v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-and (image p data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimagecube v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimagecube v-ivec3 v-int v-int) v-int
  :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimagecube v-ivec3 v-uint v-uint) v-uint
  :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimagecube v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimagecube v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-max (image p data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimagecube v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-max (image p data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimagecube v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-min (image p data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimagecube v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-min (image p data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimagecube v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-or (image p data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimagecube v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-or (image p data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimagecube v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-xor (image p data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimagecube v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-xor (image p data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimagecube v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-load (image p &context (:330 :440))
  "imageLoad(~a)"
  (v-gimagecube v-ivec3) v-gvec4 :glsl-spec-matching t)

(v-defun image-store (image p data &context (:330 :440))
  "imageStore(~a,~a)"
  (v-gimagecube v-ivec3 v-gvec4) v-void :glsl-spec-matching t)

(v-defun image-atomic-add (image p data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimagebuffer v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-add (image p data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimagebuffer v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-and (image p data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimagebuffer v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-and (image p data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimagebuffer v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimagebuffer v-int v-int v-int) v-int
  :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimagebuffer v-int v-uint v-uint) v-uint
  :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimagebuffer v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimagebuffer v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-max (image p data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimagebuffer v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-max (image p data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimagebuffer v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-min (image p data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimagebuffer v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-min (image p data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimagebuffer v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-or (image p data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimagebuffer v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-or (image p data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimagebuffer v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-xor (image p data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimagebuffer v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-xor (image p data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimagebuffer v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-load (image p &context (:330 :440))
  "imageLoad(~a)"
  (v-gimagebuffer v-int) v-gvec4 :glsl-spec-matching t)

(v-defun image-store (image p data &context (:330 :440))
  "imageStore(~a,~a)"
  (v-gimagebuffer v-int v-gvec4) v-void :glsl-spec-matching t)

(v-defun image-atomic-add (image p data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimage1darray v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-add (image p data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimage1darray v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-and (image p data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimage1darray v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-and (image p data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimage1darray v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimage1darray v-ivec2 v-int v-int) v-int
  :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimage1darray v-ivec2 v-uint v-uint)
  v-uint :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimage1darray v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimage1darray v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-max (image p data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimage1darray v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-max (image p data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimage1darray v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-min (image p data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimage1darray v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-min (image p data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimage1darray v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-or (image p data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimage1darray v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-or (image p data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimage1darray v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-xor (image p data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimage1darray v-ivec2 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-xor (image p data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimage1darray v-ivec2 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-load (image p &context (:330 :440))
  "imageLoad(~a)"
  (v-gimage1darray v-ivec2) v-gvec4 :glsl-spec-matching t)

(v-defun image-store (image p data &context (:330 :440))
  "imageStore(~a,~a)"
  (v-gimage1darray v-ivec2 v-gvec4) v-void :glsl-spec-matching t)

(v-defun image-atomic-add (image p data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimage2darray v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-add (image p data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimage2darray v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-and (image p data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimage2darray v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-and (image p data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimage2darray v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimage2darray v-ivec3 v-int v-int) v-int
  :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimage2darray v-ivec3 v-uint v-uint)
  v-uint :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimage2darray v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimage2darray v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-max (image p data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimage2darray v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-max (image p data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimage2darray v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-min (image p data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimage2darray v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-min (image p data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimage2darray v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-or (image p data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimage2darray v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-or (image p data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimage2darray v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-xor (image p data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimage2darray v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-xor (image p data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimage2darray v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-load (image p &context (:330 :440))
  "imageLoad(~a)"
  (v-gimage2darray v-ivec3) v-gvec4 :glsl-spec-matching t)

(v-defun image-store (image p data &context (:330 :440))
  "imageStore(~a,~a)"
  (v-gimage2darray v-ivec3 v-gvec4) v-void :glsl-spec-matching t)

(v-defun image-atomic-add (image p data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimagecubearray v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-add (image p data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimagecubearray v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-and (image p data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimagecubearray v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-and (image p data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimagecubearray v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimagecubearray v-ivec3 v-int v-int) v-int
  :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimagecubearray v-ivec3 v-uint v-uint)
  v-uint :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimagecubearray v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimagecubearray v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-max (image p data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimagecubearray v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-max (image p data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimagecubearray v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-min (image p data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimagecubearray v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-min (image p data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimagecubearray v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-or (image p data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimagecubearray v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-or (image p data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimagecubearray v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-xor (image p data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimagecubearray v-ivec3 v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-xor (image p data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimagecubearray v-ivec3 v-uint) v-uint :glsl-spec-matching t)

(v-defun image-load (image p &context (:330 :440))
  "imageLoad(~a)"
  (v-gimagecubearray v-ivec3) v-gvec4 :glsl-spec-matching t)

(v-defun image-store (image p data &context (:330 :440))
  "imageStore(~a,~a)"
  (v-gimagecubearray v-ivec3 v-gvec4) v-void :glsl-spec-matching t)

(v-defun image-atomic-add (image p sample data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimage2dms v-ivec2 v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-add (image p sample data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimage2dms v-ivec2 v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-and (image p sample data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimage2dms v-ivec2 v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-and (image p sample data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimage2dms v-ivec2 v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p sample compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimage2dms v-ivec2 v-int v-int v-int)
  v-int :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p sample compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimage2dms v-ivec2 v-int v-uint v-uint)
  v-uint :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p sample data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimage2dms v-ivec2 v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p sample data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimage2dms v-ivec2 v-int v-uint) v-uint
  :glsl-spec-matching t)

(v-defun image-atomic-max (image p sample data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimage2dms v-ivec2 v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-max (image p sample data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimage2dms v-ivec2 v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-min (image p sample data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimage2dms v-ivec2 v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-min (image p sample data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimage2dms v-ivec2 v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-or (image p sample data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimage2dms v-ivec2 v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-or (image p sample data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimage2dms v-ivec2 v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-xor (image p sample data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimage2dms v-ivec2 v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-xor (image p sample data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimage2dms v-ivec2 v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-load (image p sample &context (:330 :440))
  "imageLoad(~a)"
  (v-gimage2dms v-ivec2 v-int) v-gvec4 :glsl-spec-matching t)

(v-defun image-store (image p sample data &context (:330 :440))
  "imageStore(~a,~a)"
  (v-gimage2dms v-ivec2 v-int v-gvec4) v-void :glsl-spec-matching t)

(v-defun image-atomic-add (image p sample data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimage2dmsarray v-ivec3 v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-add (image p sample data &context (:330 :440))
  "imageAtomicAdd(~a,~a)"
  (v-gimage2dmsarray v-ivec3 v-int v-uint) v-uint
  :glsl-spec-matching t)

(v-defun image-atomic-and (image p sample data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimage2dmsarray v-ivec3 v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-and (image p sample data &context (:330 :440))
  "imageAtomicAnd(~a,~a)"
  (v-gimage2dmsarray v-ivec3 v-int v-uint) v-uint
  :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p sample compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimage2dmsarray v-ivec3 v-int v-int v-int)
  v-int :glsl-spec-matching t)

(v-defun image-atomic-comp-swap
    (image p sample compare data &context (:330 :440))
  "imageAtomicCompSwap(~a,~a,~a)"
  (v-gimage2dmsarray v-ivec3 v-int v-uint v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p sample data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimage2dmsarray v-ivec3 v-int v-int) v-int
  :glsl-spec-matching t)

(v-defun image-atomic-exchange (image p sample data &context (:330 :440))
  "imageAtomicExchange(~a,~a)"
  (v-gimage2dmsarray v-ivec3 v-int v-uint) v-uint
  :glsl-spec-matching t)

(v-defun image-atomic-max (image p sample data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimage2dmsarray v-ivec3 v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-max (image p sample data &context (:330 :440))
  "imageAtomicMax(~a,~a)"
  (v-gimage2dmsarray v-ivec3 v-int v-uint) v-uint
  :glsl-spec-matching t)

(v-defun image-atomic-min (image p sample data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimage2dmsarray v-ivec3 v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-min (image p sample data &context (:330 :440))
  "imageAtomicMin(~a,~a)"
  (v-gimage2dmsarray v-ivec3 v-int v-uint) v-uint
  :glsl-spec-matching t)

(v-defun image-atomic-or (image p sample data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimage2dmsarray v-ivec3 v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-or (image p sample data &context (:330 :440))
  "imageAtomicOr(~a,~a)"
  (v-gimage2dmsarray v-ivec3 v-int v-uint) v-uint :glsl-spec-matching t)

(v-defun image-atomic-xor (image p sample data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimage2dmsarray v-ivec3 v-int v-int) v-int :glsl-spec-matching t)

(v-defun image-atomic-xor (image p sample data &context (:330 :440))
  "imageAtomicXor(~a,~a)"
  (v-gimage2dmsarray v-ivec3 v-int v-uint) v-uint
  :glsl-spec-matching t)

(v-defun image-load (image p sample &context (:330 :440))
  "imageLoad(~a)"
  (v-gimage2dmsarray v-ivec3 v-int) v-gvec4 :glsl-spec-matching t)

(v-defun image-store (image p sample data &context (:330 :440))
  "imageStore(~a,~a)"
  (v-gimage2dmsarray v-ivec3 v-int v-gvec4) v-void :glsl-spec-matching t)
