;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; [TODO] setf coudl change type, how do we handle this?
(in-package :varjo)

;; (V-DEFUN :LDEXP (X TI &CONTEXT (:330 :440)) "ldexp(~a,~a)" (TFD IN) 0 :PLACE NIL) 
;; (V-DEFUN :BIT-COUNT (VALUE &CONTEXT (:330 :440)) "bitCount(~a)" (TIU) (#'TI 0) :PLACE NIL) 
;; (V-DEFUN :FIND-L-S-B (VALUE &CONTEXT (:330 :440)) "findLSB(~a)" (TIU) (#'TI 0) :PLACE NIL) 
;; (V-DEFUN :FIND-M-S-B (VALUE &CONTEXT (:330 :440)) "findMSB(~a)" (TIU) (#'TI 0) :PLACE NIL) 
;; (V-DEFUN :FLOAT-BITS-TO-INT (VALUE &CONTEXT (:330 :440)) "floatBitsToInt(~a)" (TF) (#'TI 0) :PLACE NIL) 
;; (V-DEFUN :FLOAT-BITS-TO-UINT (VALUE &CONTEXT (:330 :440)) "floatBitsToUint(~a)" (TF) (#'TU 0) :PLACE NIL) 
;; (V-DEFUN :NOISEN (X &CONTEXT (:330 :440)) "noisen(~a)" (TF) (#'VECN 0) :PLACE NIL)
;; (V-DEFUN :INT-BITS-TO-FLOAT (VALUE &CONTEXT (:330 :440)) "intBitsToFloat(~a)" (TI) & :PLACE NIL) 
;; (V-DEFUN :UINT-BITS-TO-FLOAT (VALUE &CONTEXT (:330 :440)) "uintBitsToFloat(~a)" (TF) & :PLACE NIL) 

(v-defun aref (x) "~a[~a]" (v-array v-int) (:element 0))
(v-defun aref (x) "~a[~a]" (v-vector v-int) (:element 0))

(v-defun :break () "break" () v-void :glsl-spec-matching t)

(v-defun :int (x) "int(~a)" (v-uint) v-int :glsl-spec-matching t)
(v-defun :int (x) "int(~a)" (v-bool) v-int :glsl-spec-matching t)
(v-defun :int (x) "int(~a)" (v-float) v-int :glsl-spec-matching t)
(v-defun :int (x) "int(~a)" (v-double)  v-int :glsl-spec-matching t)
(v-defun :uint (x) "uint(~a)" (v-int) v-uint :glsl-spec-matching t)
(v-defun :uint (x) "uint(~a)" (v-bool) v-uint :glsl-spec-matching t)
(v-defun :uint (x) "uint(~a)" (v-float)  v-uint :glsl-spec-matching t)
(v-defun :uint (x) "uint(~a)" (v-double)  v-uint :glsl-spec-matching t)
(v-defun :bool (x) "bool(~a)" (v-int) v-bool :glsl-spec-matching t)
(v-defun :bool (x) "bool(~a)" (v-uint) v-bool :glsl-spec-matching t)
(v-defun :bool (x) "bool(~a)" (v-float)  v-bool :glsl-spec-matching t)
(v-defun :bool (x) "bool(~a)" (v-double)  v-bool :glsl-spec-matching t)
(v-defun :float (x) "float(~a)" (v-int) v-float :glsl-spec-matching t)
(v-defun :float (x) "float(~a)" (v-uint)  v-float :glsl-spec-matching t)
(v-defun :float (x) "float(~a)" (v-bool)  v-float :glsl-spec-matching t)
(v-defun :float (x) "float(~a)" (v-double) v-float :glsl-spec-matching t)
(v-defun :double (x) "double(~a)" (v-int)  v-double :glsl-spec-matching t)
(v-defun :double (x) "double(~a)" (v-uint)  v-double :glsl-spec-matching t)
(v-defun :double (x) "double(~a)" (v-bool)  v-double :glsl-spec-matching t)
(v-defun :double (x) "double(~a)" (v-float) v-double :glsl-spec-matching t)

(v-defun :x (a) "~a.x" (v-vector) (:element 0) :glsl-spec-matching t)
(v-defun :y (a) "~a.y" (v-vector) (:element 0) :glsl-spec-matching t)
(v-defun :z (a) "~a.z" (v-vec3)  (:element 0) :glsl-spec-matching t)
(v-defun :z (a) "~a.z" (v-bvec3) (:element 0) :glsl-spec-matching t)
(v-defun :z (a) "~a.z" (v-ivec3) (:element 0) :glsl-spec-matching t)
(v-defun :z (a) "~a.z" (v-uvec3) (:element 0) :glsl-spec-matching t)
(v-defun :z (a) "~a.z" (v-dvec3) (:element 0) :glsl-spec-matching t)
(v-defun :z (a) "~a.z" (v-vec4)  (:element 0) :glsl-spec-matching t)
(v-defun :z (a) "~a.z" (v-bvec4) (:element 0) :glsl-spec-matching t)
(v-defun :z (a) "~a.z" (v-ivec4) (:element 0) :glsl-spec-matching t)
(v-defun :z (a) "~a.z" (v-uvec4) (:element 0) :glsl-spec-matching t)
(v-defun :z (a) "~a.z" (v-dvec4) (:element 0) :glsl-spec-matching t)
(v-defun :w (a) "~a.w" (v-vec4) (:element 0) :glsl-spec-matching t)
(v-defun :w (a) "~a.w" (v-bvec4) (:element 0) :glsl-spec-matching t)
(v-defun :w (a) "~a.w" (v-ivec4) (:element 0) :glsl-spec-matching t)
(v-defun :w (a) "~a.w" (v-uvec4) (:element 0) :glsl-spec-matching t)
(v-defun :w (a) "~a.w" (v-dvec4) (:element 0) :glsl-spec-matching t)

(v-defun :not (a) "(~a == true)" (v-bool) v-bool :glsl-spec-matching t)
(v-defun :not (a) "false" (v-type) v-bool :glsl-spec-matching t)

(v-defun :%< (a b) "(~a < ~a)" (v-number v-number) v-bool :glsl-spec-matching t)
(v-defun :%> (a b) "(~a > ~a)" (v-number v-number) v-bool :glsl-spec-matching t)
(v-defun :%<= (a b) "(~a <= ~a)" (v-number v-number) v-bool :glsl-spec-matching t)
(v-defun :%>= (a b) "(~a >= ~a)" (v-number v-number) v-bool :glsl-spec-matching t)

(v-defun :%equal (a b) "(~a == ~a)" (v-number v-number) v-bool :glsl-spec-matching t)
(v-defun :%eql (a b) "(~a == ~a)" (v-number v-number) v-bool :glsl-spec-matching t)
(v-defun :%= (a b) "(~a == ~a)" (v-number v-number) v-bool :glsl-spec-matching t)

(v-defun :++ (a) "(++ ~a)" (v-number) nil :glsl-spec-matching t)
(v-defun :-- (a) "(-- ~a)" (v-number) nil :glsl-spec-matching t)

(v-defun :%+ (a) "(~a)" (v-number) nil :glsl-spec-matching t)
(v-defun :%+ (a b) "(~a + ~a)" (v-number v-number) nil :glsl-spec-matching t)

(v-defun :%+ (a b) "(~a + ~a)" (v-vec2 v-vec2) 0 :glsl-spec-matching t)
(v-defun :%+ (a b) "(~a + ~a)" (v-vec3 v-vec3) 0 :glsl-spec-matching t)
(v-defun :%+ (a b) "(~a + ~a)" (v-vec4 v-vec4) 0 :glsl-spec-matching t)

(v-defun :%+ (a b) "(~a + ~a)" (v-ivec2 v-ivec2) 0 :glsl-spec-matching t)
(v-defun :%+ (a b) "(~a + ~a)" (v-ivec3 v-ivec3) 0 :glsl-spec-matching t)
(v-defun :%+ (a b) "(~a + ~a)" (v-ivec4 v-ivec4) 0 :glsl-spec-matching t)

(v-defun :%+ (a b) "(~a + ~a)" (v-uvec2 v-uvec2) 0 :glsl-spec-matching t)
(v-defun :%+ (a b) "(~a + ~a)" (v-uvec3 v-uvec3) 0 :glsl-spec-matching t)
(v-defun :%+ (a b) "(~a + ~a)" (v-uvec4 v-uvec4) 0 :glsl-spec-matching t)

(v-defun :%+ (a b) "(~a + ~a)" (v-dvec2 v-dvec2) 0 :glsl-spec-matching t)
(v-defun :%+ (a b) "(~a + ~a)" (v-dvec3 v-dvec3) 0 :glsl-spec-matching t)
(v-defun :%+ (a b) "(~a + ~a)" (v-dvec4 v-dvec4) 0 :glsl-spec-matching t)

(v-defun :%- (a b) "(~a - ~a)" (v-number v-number) nil :glsl-spec-matching t)
(v-defun :%- (a) "(- ~a)" (v-number) 0 :glsl-spec-matching t)
(v-defun :%- (a b) "(~a - ~a)" (v-vec2 v-vec2) 0 :glsl-spec-matching t)
(v-defun :%- (a b) "(~a - ~a)" (v-vec3 v-vec3) 0 :glsl-spec-matching t)
(v-defun :%- (a b) "(~a - ~a)" (v-vec4 v-vec4) 0 :glsl-spec-matching t)

(v-defun :%- (a b) "(~a - ~a)" (v-ivec2 v-ivec2) 0 :glsl-spec-matching t)
(v-defun :%- (a b) "(~a - ~a)" (v-ivec3 v-ivec3) 0 :glsl-spec-matching t)
(v-defun :%- (a b) "(~a - ~a)" (v-ivec4 v-ivec4) 0 :glsl-spec-matching t)

(v-defun :%- (a b) "(~a - ~a)" (v-uvec2 v-uvec2) 0 :glsl-spec-matching t)
(v-defun :%- (a b) "(~a - ~a)" (v-uvec3 v-uvec3) 0 :glsl-spec-matching t)
(v-defun :%- (a b) "(~a - ~a)" (v-uvec4 v-uvec4) 0 :glsl-spec-matching t)

(v-defun :%- (a b) "(~a - ~a)" (v-dvec2 v-dvec2) 0 :glsl-spec-matching t)
(v-defun :%- (a b) "(~a - ~a)" (v-dvec3 v-dvec3) 0 :glsl-spec-matching t)
(v-defun :%- (a b) "(~a - ~a)" (v-dvec4 v-dvec4) 0 :glsl-spec-matching t)

(v-defun :%* (a b) "(~a * ~a)" (v-number v-number) nil :glsl-spec-matching t)
(v-defun :%* (a b) "(~a * ~a)" (v-vector v-vector) nil :glsl-spec-matching t)
(v-defun :%* (a b) "(~a * ~a)" (v-matrix v-matrix) nil :glsl-spec-matching t)

(v-defun :%* (a b) "(~a * ~a)" (v-mat2 v-vec2) 1 :glsl-spec-matching t)
(v-defun :%* (a b) "(~a * ~a)" (v-mat3 v-vec3) 1 :glsl-spec-matching t)
(v-defun :%* (a b) "(~a * ~a)" (v-mat4 v-vec4) 1 :glsl-spec-matching t)

(v-defun :%* (a b) "(~a * ~a)" (v-mat2 v-float) 0 :glsl-spec-matching t)
(v-defun :%* (a b) "(~a * ~a)" (v-mat4 v-float) 0 :glsl-spec-matching t)
(v-defun :%* (a b) "(~a * ~a)" (v-mat3 v-float) 0 :glsl-spec-matching t)

(v-defun :%* (a b) "(~a * ~a)" (v-vec2 v-float) 0 :glsl-spec-matching t)
(v-defun :%* (a b) "(~a * ~a)" (v-vec3 v-float) 0 :glsl-spec-matching t)
(v-defun :%* (a b) "(~a * ~a)" (v-vec4 v-float) 0 :glsl-spec-matching t)

(v-defun :%/ (a b) "(~a / ~a)" (v-number v-number) nil :glsl-spec-matching t)

(v-defun :v! (x y) "vec3(~a,~a)" (v-float v-vec2) v-vec3 :glsl-spec-matching t)
(v-defun :v! (x y) "vec3(~a,~a)" (v-vec2 v-float) v-vec3 :glsl-spec-matching t)

(v-defun :v! (x y) "vec4(~a,~a)" (v-vec2 v-vec2) v-vec4 :glsl-spec-matching t)
(v-defun :v! (x y) "vec4(~a,~a)" (v-float v-vec3) v-vec4 :glsl-spec-matching t)
(v-defun :v! (x y) "vec4(~a,~a)" (v-vec3 v-float) v-vec4 :glsl-spec-matching t)

(v-defun :v! (x y z) "vec4(~a,~a,~a)" (v-vec2 v-float v-float) v-vec4 :glsl-spec-matching t)
(v-defun :v! (x y z) "vec4(~a,~a,~a)" (v-float v-vec2 v-float) v-vec4 :glsl-spec-matching t)
(v-defun :v! (x y z) "vec4(~a,~a,~a)" (v-float v-float v-vec2) v-vec4 :glsl-spec-matching t)

(v-defun :v! (x y) "vec2(~a,~a)" (v-float v-float) v-vec2 :glsl-spec-matching t)
(v-defun :v! (x y z) "vec3(~a,~a,~a)" (v-float v-float v-float) v-vec3 :glsl-spec-matching t)
(v-defun :v! (x y z w) "vec4(~a,~a,~a,~a)" (v-float v-float v-float v-float) 
         v-vec4 :glsl-spec-matching t)

(v-defun :m! (a b c d) "mat2(~a,~a,~a,~a)" (v-float v-float v-float v-float)
         v-mat2 :glsl-spec-matching t)
(v-defun :m! (a b) "mat2(~a,~a)" (v-vec2 v-vec2) v-mat2 :glsl-spec-matching t)

(v-defun :m! (a b c d e f g h i) "mat3(~a,~a,~a,~a,~a,~a,~a,~a,~a)" 
         (v-float v-float v-float v-float v-float
                  v-float v-float v-float v-float) v-mat3 :glsl-spec-matching t)
(v-defun :m! (a b c) "mat3(~a,~a,~a)" (v-vec3 v-vec3 v-vec3) v-mat3
         :glsl-spec-matching t)

(v-defun :m! (a b c d e f g h i j k l m n o p) 
  "mat4(~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a)" 
  (v-float v-float v-float v-float v-float v-float v-float v-float v-float
           v-float v-float v-float v-float v-float v-float v-float v-float) 
  v-mat4 :glsl-spec-matching t)
(v-defun :m! (a b c d) 
  "mat4(~a,~a,~a,~a)" (v-vec4 v-vec4 v-vec4 v-vec4) v-mat4
  :glsl-spec-matching t)

(V-DEFUN :CLAMP (X MINVAL MAXVAL &CONTEXT (:330 :440))
         "clamp(~a,~a,~a)"
         (V-TIU V-TIU V-TIU) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MAX (X Y &CONTEXT (:330 :440))
         "max(~a,~a)"
         (V-TIU V-TIU) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MIN (X Y &CONTEXT (:330 :440))
         "min(~a,~a)"
         (V-TIU V-TIU) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :BITFIELD-INSERT (BASE INSERT OFFSET BITS &CONTEXT (:330 :440))
         "bitfieldInsert(~a,~a,~a,~a)"
         (V-TIU V-TIU V-INT V-INT) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :CLAMP (X MINVAL MAXVAL &CONTEXT (:330 :440))
         "clamp(~a,~a,~a)"
         (V-TFD V-TFD V-TFD) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MAX (X Y &CONTEXT (:330 :440))
         "max(~a,~a)"
         (V-TFD V-TFD) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MIN (X Y &CONTEXT (:330 :440))
         "min(~a,~a)"
         (V-TFD V-TFD) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MIX (X Y A &CONTEXT (:330 :440))
         "mix(~a,~a,~a)"
         (V-TFD V-TFD V-TB) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MIX (X Y A &CONTEXT (:330 :440))
         "mix(~a,~a,~a)"
         (V-TFD V-TFD V-TFD) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MOD (X Y &CONTEXT (:330 :440))
         "mod(~a,~a)"
         (V-TFD V-TFD) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :FACEFORWARD (N I NREF &CONTEXT (:330 :440))
         "faceforward(~a,~a,~a)"
         (V-TFD V-TFD V-TFD) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :FMA (A B C &CONTEXT (:330 :440))
         "fma(~a,~a,~a)"
         (V-TFD V-TFD V-TFD) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :REFLECT (I N &CONTEXT (:330 :440))
         "reflect(~a,~a)"
         (V-TFD V-TFD)
         0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :REFRACT (I N ETA &CONTEXT (:330 :440))
         "refract(~a,~a,~a)"
         (V-TFD V-TFD V-FLOAT) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :SMOOTHSTEP (EDGE0 EDGE1 X &CONTEXT (:330 :440))
         "smoothstep(~a,~a,~a)"
         (V-TFD V-TFD V-TFD) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :STEP (EDGE X &CONTEXT (:330 :440))
         "step(~a,~a)"
         (V-TFD V-TFD) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MIX (X Y A &CONTEXT (:330 :440))
         "mix(~a,~a,~a)"
         (V-TD V-TD V-DOUBLE) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MIX (X Y A &CONTEXT (:330 :440))
         "mix(~a,~a,~a)"
         (V-TF V-TF V-FLOAT) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ATAN (Y X &CONTEXT (:330 :440))
         "atan(~a,~a)"
         (V-TF V-TF) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :POW (X Y &CONTEXT (:330 :440))
         "pow(~a,~a)"
         (V-TF V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ISINF (X &CONTEXT (:330 :440))
         "isinf(~a)"
         (V-FLOAT) V-BOOL
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ISINF (X &CONTEXT (:330 :440))
         "isinf(~a)"
         (V-DOUBLE) V-BOOL
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ISINF (X &CONTEXT (:330 :440))
         "isinf(~a)"
         (V-VEC2) V-BVEC2
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ISINF (X &CONTEXT (:330 :440))
         "isinf(~a)"
         (V-DVEC2) V-BVEC2
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ISINF (X &CONTEXT (:330 :440))
         "isinf(~a)"
         (V-VEC3) V-BVEC3
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ISINF (X &CONTEXT (:330 :440))
         "isinf(~a)"
         (V-DVEC3) V-BVEC3
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ISINF (X &CONTEXT (:330 :440))
         "isinf(~a)"
         (V-VEC4) V-BVEC4
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ISINF (X &CONTEXT (:330 :440))
         "isinf(~a)"
         (V-DVEC4) V-BVEC4
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ISNAN (X &CONTEXT (:330 :440))
         "isnan(~a)"
         (V-FLOAT) V-BOOL
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ISNAN (X &CONTEXT (:330 :440))
         "isnan(~a)"
         (V-DOUBLE) V-BOOL
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ISNAN (X &CONTEXT (:330 :440))
         "isnan(~a)"
         (V-VEC2) V-BVEC2
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ISNAN (X &CONTEXT (:330 :440))
         "isnan(~a)"
         (V-DVEC2) V-BVEC2
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ISNAN (X &CONTEXT (:330 :440))
         "isnan(~a)"
         (V-VEC3) V-BVEC3
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ISNAN (X &CONTEXT (:330 :440))
         "isnan(~a)"
         (V-DVEC3) V-BVEC3
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ISNAN (X &CONTEXT (:330 :440))
         "isnan(~a)"
         (V-VEC4) V-BVEC4
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ISNAN (X &CONTEXT (:330 :440))
         "isnan(~a)"
         (V-DVEC4) V-BVEC4
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :V-NOT (X &CONTEXT (:330 :440)) "not(~a)" (V-BVECTOR) 0 :PLACE NIL
         :GLSL-SPEC-MATCHING T)

(V-DEFUN :V-EQUAL (X Y &CONTEXT (:330 :440))
         "equal(~a,~a)"
         (V-BVECTOR V-BVECTOR)
         0 :PLACE NIL :GLSL-SPEC-MATCHING T)
(V-DEFUN :V-EQUAL (X Y &CONTEXT (:330 :440))
         "equal(~a,~a)"
         (V-FLOAT V-FLOAT)
         V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)
(V-DEFUN :V-EQUAL (X Y &CONTEXT (:330 :440))
         "equal(~a,~a)"
         (V-VEC2 V-VEC2)
         V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)
(V-DEFUN :V-EQUAL (X Y &CONTEXT (:330 :440))
         "equal(~a,~a)"
         (V-VEC3 V-VEC3)
         V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)
(V-DEFUN :V-EQUAL (X Y &CONTEXT (:330 :440))
         "equal(~a,~a)"
         (V-VEC4 V-VEC4)
         V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :V-NOT-EQUAL (X Y &CONTEXT (:330 :440))
         "notEqual(~a,~a)"
         (V-BVECTOR V-BVECTOR) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)
(V-DEFUN :V-NOT-EQUAL (X Y &CONTEXT (:330 :440))
         "notEqual(~a,~a)"
         (V-FLOAT V-FLOAT) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)
(V-DEFUN :V-NOT-EQUAL (X Y &CONTEXT (:330 :440))
         "notEqual(~a,~a)"
         (V-VEC2 V-VEC2) V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)
(V-DEFUN :V-NOT-EQUAL (X Y &CONTEXT (:330 :440))
         "notEqual(~a,~a)"
         (V-VEC3 V-VEC3) V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)
(V-DEFUN :V-NOT-EQUAL (X Y &CONTEXT (:330 :440))
         "notEqual(~a,~a)"
         (V-VEC4 V-VEC4) V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN (X Y &CONTEXT (:330 :440))
         "greaterThan(~a,~a)"
         (V-FLOAT V-FLOAT) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN (X Y &CONTEXT (:330 :440))
         "greaterThan(~a,~a)"
         (V-VEC2 V-VEC2) V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN (X Y &CONTEXT (:330 :440))
         "greaterThan(~a,~a)"
         (V-VEC3 V-VEC3) V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN (X Y &CONTEXT (:330 :440))
         "greaterThan(~a,~a)"
         (V-VEC4 V-VEC4) V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "greaterThanEqual(~a,~a)"
         (V-FLOAT V-FLOAT) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "greaterThanEqual(~a,~a)"
         (V-VEC2 V-VEC2) V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "greaterThanEqual(~a,~a)"
         (V-VEC3 V-VEC3) V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "greaterThanEqual(~a,~a)"
         (V-VEC4 V-VEC4) V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN (X Y &CONTEXT (:330 :440))
         "lessThan(~a,~a)"
         (V-FLOAT V-FLOAT) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN (X Y &CONTEXT (:330 :440))
         "lessThan(~a,~a)"
         (V-VEC2 V-VEC2) V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN (X Y &CONTEXT (:330 :440))
         "lessThan(~a,~a)"
         (V-VEC3 V-VEC3) V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN (X Y &CONTEXT (:330 :440))
         "lessThan(~a,~a)"
         (V-VEC4 V-VEC4) V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "lessThanEqual(~a,~a)"
         (V-FLOAT V-FLOAT) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "lessThanEqual(~a,~a)"
         (V-VEC2 V-VEC2) V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "lessThanEqual(~a,~a)"
         (V-VEC3 V-VEC3) V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "lessThanEqual(~a,~a)"
         (V-VEC4 V-VEC4) V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :EQUAL (X Y &CONTEXT (:330 :440))
         "equal(~a,~a)"
         (V-INT V-INT)
         V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :V-EQUAL (X Y &CONTEXT (:330 :440))
         "equal(~a,~a)"
         (V-IVEC2 V-IVEC2)
         V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :V-EQUAL (X Y &CONTEXT (:330 :440))
         "equal(~a,~a)"
         (V-IVEC3 V-IVEC3)
         V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :V-EQUAL (X Y &CONTEXT (:330 :440))
         "equal(~a,~a)"
         (V-IVEC4 V-IVEC4)
         V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :NOT-EQUAL (X Y &CONTEXT (:330 :440))
         "notEqual(~a,~a)"
         (V-INT V-INT) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :NOT-EQUAL (X Y &CONTEXT (:330 :440))
         "notEqual(~a,~a)"
         (V-IVEC2 V-IVEC2) V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :NOT-EQUAL (X Y &CONTEXT (:330 :440))
         "notEqual(~a,~a)"
         (V-IVEC3 V-IVEC3) V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :NOT-EQUAL (X Y &CONTEXT (:330 :440))
         "notEqual(~a,~a)"
         (V-IVEC4 V-IVEC4) V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN (X Y &CONTEXT (:330 :440))
         "greaterThan(~a,~a)"
         (V-INT V-INT) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN (X Y &CONTEXT (:330 :440))
         "greaterThan(~a,~a)"
         (V-IVEC2 V-IVEC2) V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN (X Y &CONTEXT (:330 :440))
         "greaterThan(~a,~a)"
         (V-IVEC3 V-IVEC3) V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN (X Y &CONTEXT (:330 :440))
         "greaterThan(~a,~a)"
         (V-IVEC4 V-IVEC4) V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "greaterThanEqual(~a,~a)"
         (V-INT V-INT) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "greaterThanEqual(~a,~a)"
         (V-IVEC2 V-IVEC2) V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "greaterThanEqual(~a,~a)"
         (V-IVEC3 V-IVEC3) V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "greaterThanEqual(~a,~a)"
         (V-IVEC4 V-IVEC4) V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN (X Y &CONTEXT (:330 :440))
         "lessThan(~a,~a)"
         (V-INT V-INT) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN (X Y &CONTEXT (:330 :440))
         "lessThan(~a,~a)"
         (V-IVEC2 V-IVEC2) V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN (X Y &CONTEXT (:330 :440))
         "lessThan(~a,~a)"
         (V-IVEC3 V-IVEC3) V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN (X Y &CONTEXT (:330 :440))
         "lessThan(~a,~a)"
         (V-IVEC4 V-IVEC4) V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "lessThanEqual(~a,~a)"
         (V-INT V-INT) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "lessThanEqual(~a,~a)"
         (V-IVEC2 V-IVEC2) V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "lessThanEqual(~a,~a)"
         (V-IVEC3 V-IVEC3) V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "lessThanEqual(~a,~a)"
         (V-IVEC4 V-IVEC4) V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :V-EQUAL (X Y &CONTEXT (:330 :440))
         "equal(~a,~a)"
         (V-UINT V-UINT)
         V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :V-EQUAL (X Y &CONTEXT (:330 :440))
         "equal(~a,~a)"
         (V-UVEC2 V-UVEC2)
         V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :V-EQUAL (X Y &CONTEXT (:330 :440))
         "equal(~a,~a)"
         (V-UVEC3 V-UVEC3)
         V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :V-EQUAL (X Y &CONTEXT (:330 :440))
         "equal(~a,~a)"
         (V-UVEC4 V-UVEC4)
         V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :NOT-EQUAL (X Y &CONTEXT (:330 :440))
         "notEqual(~a,~a)"
         (V-UINT V-UINT) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :NOT-EQUAL (X Y &CONTEXT (:330 :440))
         "notEqual(~a,~a)"
         (V-UVEC2 V-UVEC2) V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :NOT-EQUAL (X Y &CONTEXT (:330 :440))
         "notEqual(~a,~a)"
         (V-UVEC3 V-UVEC3) V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :NOT-EQUAL (X Y &CONTEXT (:330 :440))
         "notEqual(~a,~a)"
         (V-UVEC4 V-UVEC4) V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN (X Y &CONTEXT (:330 :440))
         "greaterThan(~a,~a)"
         (V-UINT V-UINT) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN (X Y &CONTEXT (:330 :440))
         "greaterThan(~a,~a)"
         (V-UVEC2 V-UVEC2) V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN (X Y &CONTEXT (:330 :440))
         "greaterThan(~a,~a)"
         (V-UVEC3 V-UVEC3) V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN (X Y &CONTEXT (:330 :440))
         "greaterThan(~a,~a)"
         (V-UVEC4 V-UVEC4) V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "greaterThanEqual(~a,~a)"
         (V-UINT V-UINT) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "greaterThanEqual(~a,~a)"
         (V-UVEC2 V-UVEC2) V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "greaterThanEqual(~a,~a)"
         (V-UVEC3 V-UVEC3) V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "greaterThanEqual(~a,~a)"
         (V-UVEC4 V-UVEC4) V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN (X Y &CONTEXT (:330 :440))
         "lessThan(~a,~a)"
         (V-UINT V-UINT) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN (X Y &CONTEXT (:330 :440))
         "lessThan(~a,~a)"
         (V-UVEC2 V-UVEC2) V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN (X Y &CONTEXT (:330 :440))
         "lessThan(~a,~a)"
         (V-UVEC3 V-UVEC3) V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN (X Y &CONTEXT (:330 :440))
         "lessThan(~a,~a)"
         (V-UVEC4 V-UVEC4) V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "lessThanEqual(~a,~a)"
         (V-UINT V-UINT) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "lessThanEqual(~a,~a)"
         (V-UVEC2 V-UVEC2) V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "lessThanEqual(~a,~a)"
         (V-UVEC3 V-UVEC3) V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "lessThanEqual(~a,~a)"
         (V-UVEC4 V-UVEC4) V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :EQUAL (X Y &CONTEXT (:330 :440))
         "equal(~a,~a)"
         (V-DOUBLE V-DOUBLE) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :V-EQUAL (X Y &CONTEXT (:330 :440))
         "equal(~a,~a)"
         (V-DVEC2 V-DVEC2)
         V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :V-EQUAL (X Y &CONTEXT (:330 :440))
         "equal(~a,~a)"
         (V-DVEC3 V-DVEC3)
         V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :V-EQUAL (X Y &CONTEXT (:330 :440))
         "equal(~a,~a)"
         (V-DVEC4 V-DVEC4)
         V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :NOT-EQUAL (X Y &CONTEXT (:330 :440))
         "notEqual(~a,~a)"
         (V-DOUBLE V-DOUBLE) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :NOT-EQUAL (X Y &CONTEXT (:330 :440))
         "notEqual(~a,~a)"
         (V-DVEC2 V-DVEC2) V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :NOT-EQUAL (X Y &CONTEXT (:330 :440))
         "notEqual(~a,~a)"
         (V-DVEC3 V-DVEC3) V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :NOT-EQUAL (X Y &CONTEXT (:330 :440))
         "notEqual(~a,~a)"
         (V-DVEC4 V-DVEC4) V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN (X Y &CONTEXT (:330 :440))
         "greaterThan(~a,~a)"
         (V-DOUBLE V-DOUBLE) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN (X Y &CONTEXT (:330 :440))
         "greaterThan(~a,~a)"
         (V-DVEC2 V-DVEC2) V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN (X Y &CONTEXT (:330 :440))
         "greaterThan(~a,~a)"
         (V-DVEC3 V-DVEC3) V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN (X Y &CONTEXT (:330 :440))
         "greaterThan(~a,~a)"
         (V-DVEC4 V-DVEC4) V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "greaterThanEqual(~a,~a)"
         (V-DOUBLE V-DOUBLE) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "greaterThanEqual(~a,~a)"
         (V-DVEC2 V-DVEC2) V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "greaterThanEqual(~a,~a)"
         (V-DVEC3 V-DVEC3) V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GREATER-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "greaterThanEqual(~a,~a)"
         (V-DVEC4 V-DVEC4) V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN (X Y &CONTEXT (:330 :440))
         "lessThan(~a,~a)"
         (V-DOUBLE V-DOUBLE) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN (X Y &CONTEXT (:330 :440))
         "lessThan(~a,~a)"
         (V-DVEC2 V-DVEC2) V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN (X Y &CONTEXT (:330 :440))
         "lessThan(~a,~a)"
         (V-DVEC3 V-DVEC3) V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN (X Y &CONTEXT (:330 :440))
         "lessThan(~a,~a)"
         (V-DVEC4 V-DVEC4) V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "lessThanEqual(~a,~a)"
         (V-DOUBLE V-DOUBLE) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "lessThanEqual(~a,~a)"
         (V-DVEC2 V-DVEC2) V-BVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "lessThanEqual(~a,~a)"
         (V-DVEC3 V-DVEC3) V-BVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LESS-THAN-EQUAL (X Y &CONTEXT (:330 :440))
         "lessThanEqual(~a,~a)"
         (V-DVEC4 V-DVEC4) V-BVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ABS (X &CONTEXT (:330 :440))
         "abs(~a)"
         (V-TFD) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ABS (X &CONTEXT (:330 :440))
         "abs(~a)"
         (V-TI) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ACOS (X &CONTEXT (:330 :440))
         "acos(~a)"
         (V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ACOSH (X &CONTEXT (:330 :440))
         "acosh(~a)"
         (V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ALL (X &CONTEXT (:330 :440))
         "all(~a)"
         (V-BVECTOR) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ANY (X &CONTEXT (:330 :440))
         "any(~a)"
         (V-BVECTOR) V-BOOL :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ASIN (X &CONTEXT (:330 :440))
         "asin(~a)"
         (V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ASINH (X &CONTEXT (:330 :440))
         "asinh(~a)"
         (V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ATAN (Y_OVER_X &CONTEXT (:330 :440))
         "atan(~a)"
         (V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ATANH (X &CONTEXT (:330 :440))
         "atanh(~a)"
         (V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ATOMIC-COUNTER (C &CONTEXT (:330 :440))
         "atomicCounter(~a)"
         (V-ATOMIC_UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ATOMIC-COUNTER-DECREMENT (C &CONTEXT (:330 :440))
         "atomicCounterDecrement(~a)"
         (V-ATOMIC_UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ATOMIC-COUNTER-INCREMENT (C &CONTEXT (:330 :440))
         "atomicCounterIncrement(~a)"
         (V-ATOMIC_UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :BARRIER (&CONTEXT (:330 :440))
         "barrier()" NIL V-VOID :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :BITFIELD-EXTRACT (VALUE OFFSET BITS &CONTEXT (:330 :440))
         "bitfieldExtract(~a,~a,~a)"
         (V-TIU V-INT V-INT) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :BITFIELD-REVERSE (VALUE &CONTEXT (:330 :440))
         "bitfieldReverse(~a)"
         (V-TIU) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :CEIL (X &CONTEXT (:330 :440))
         "ceil(~a)"
         (V-TFD) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :CLAMP (X MINVAL MAXVAL &CONTEXT (:330 :440))
         "clamp(~a,~a,~a)"
         (V-TD V-DOUBLE V-DOUBLE) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :CLAMP (X MINVAL MAXVAL &CONTEXT (:330 :440))
         "clamp(~a,~a,~a)"
         (V-TF V-FLOAT V-FLOAT) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :CLAMP (X MINVAL MAXVAL &CONTEXT (:330 :440))
         "clamp(~a,~a,~a)"
         (V-TI V-INT V-INT) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :CLAMP (X MINVAL MAXVAL &CONTEXT (:330 :440))
         "clamp(~a,~a,~a)"
         (V-TU V-UINT V-UINT) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :COS (ANGLE &CONTEXT (:330 :440))
         "cos(~a)"
         (V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :COSH (X &CONTEXT (:330 :440))
         "cosh(~a)"
         (V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :CROSS (X Y &CONTEXT (:330 :440))
         "cross(~a,~a)"
         (V-DVEC3 V-DVEC3)
         V-DVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :CROSS (X Y &CONTEXT (:330 :440))
         "cross(~a,~a)"
         (V-VEC3 V-VEC3)
         V-VEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :D-FDX (P &CONTEXT (:330 :440))
         "dFdx(~a)"
         (V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :D-FDY (P &CONTEXT (:330 :440))
         "dFdy(~a)"
         (V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :DEGREES (RADIANS &CONTEXT (:330 :440))
         "degrees(~a)"
         (V-TF) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :DETERMINANT (M &CONTEXT (:330 :440))
         "determinant(~a)"
         (V-DMATN)
         V-DOUBLE :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :DETERMINANT (M &CONTEXT (:330 :440))
         "determinant(~a)"
         (V-MATN)
         V-FLOAT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :DISTANCE (P0 P1 &CONTEXT (:330 :440))
         "distance(~a,~a)"
         (V-TD V-TD) V-DOUBLE :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :DISTANCE (P0 P1 &CONTEXT (:330 :440))
         "distance(~a,~a)"
         (V-TF V-TF) V-FLOAT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :DOT (X Y &CONTEXT (:330 :440))
         "dot(~a,~a)"
         (V-TD V-TD) V-DOUBLE
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :DOT (X Y &CONTEXT (:330 :440))
         "dot(~a,~a)"
         (V-TF V-TF) V-FLOAT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :EMIT-STREAM-VERTEX (STREAM &CONTEXT (:330 :440))
         "EmitStreamVertex(~a)"
         (V-INT) V-VOID :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :EMIT-VERTEX (&CONTEXT (:330 :440))
         "EmitVertex()" NIL V-VOID
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :END-PRIMITIVE (&CONTEXT (:330 :440))
         "EndPrimitive()" NIL V-VOID
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :END-STREAM-PRIMITIVE (STREAM &CONTEXT (:330 :440))
         "EndStreamPrimitive(~a)"
         (V-INT) V-VOID :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :EXP (X &CONTEXT (:330 :440))
         "exp(~a)"
         (V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :EXP-2 (X &CONTEXT (:330 :440))
         "exp2(~a)"
         (V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :FLOOR (X &CONTEXT (:330 :440))
         "floor(~a)"
         (V-TFD) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :FRACT (X &CONTEXT (:330 :440))
         "fract(~a)"
         (V-TFD) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :FWIDTH (P &CONTEXT (:330 :440))
         "fwidth(~a)"
         (V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :GROUP-MEMORY-BARRIER (&CONTEXT (:330 :440))
         "groupMemoryBarrier()" NIL V-VOID :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-SIZE (IMAGE &CONTEXT (:330 :440))
         "imageSize(~a)"
         (V-GIMAGE1D) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-SIZE (IMAGE &CONTEXT (:330 :440))
         "imageSize(~a)"
         (V-GIMAGEBUFFER) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-SIZE (IMAGE &CONTEXT (:330 :440))
         "imageSize(~a)"
         (V-GIMAGE2D) V-IVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-SIZE (IMAGE &CONTEXT (:330 :440))
         "imageSize(~a)"
         (V-GIMAGECUBE) V-IVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-SIZE (IMAGE &CONTEXT (:330 :440))
         "imageSize(~a)"
         (V-GIMAGERECT) V-IVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-SIZE (IMAGE &CONTEXT (:330 :440))
         "imageSize(~a)"
         (V-GIMAGE1DARRAY) V-IVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-SIZE (IMAGE &CONTEXT (:330 :440))
         "imageSize(~a)"
         (V-GIMAGE2DMS) V-IVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-SIZE (IMAGE &CONTEXT (:330 :440))
         "imageSize(~a)"
         (V-GIMAGE2DARRAY) V-IVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-SIZE (IMAGE &CONTEXT (:330 :440))
         "imageSize(~a)"
         (V-GIMAGECUBE2DARRAY) V-IVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-SIZE (IMAGE &CONTEXT (:330 :440))
         "imageSize(~a)"
         (V-GIMAGE2DMSARRAY) V-IVEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-SIZE (IMAGE &CONTEXT (:330 :440))
         "imageSize(~a)"
         (V-GIMAGE3D) V-VEC3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :INTERPOLATE-AT-CENTROID (INTERPOLANT &CONTEXT (:330 :440))
         "interpolateAtCentroid(~a)"
         (V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :INTERPOLATE-AT-OFFSET (INTERPOLANT OFFSET &CONTEXT (:330 :440))
         "interpolateAtOffset(~a,~a)"
         (V-TF V-VEC2) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :INTERPOLATE-AT-SAMPLE (INTERPOLANT SAMPLE &CONTEXT (:330 :440))
         "interpolateAtSample(~a,~a)"
         (V-TF V-INT) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :INVERSE (M &CONTEXT (:330 :440))
         "inverse(~a)"
         (V-DMATN) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :INVERSE (M &CONTEXT (:330 :440))
         "inverse(~a)"
         (V-MATN) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :INVERSESQRT (X &CONTEXT (:330 :440))
         "inversesqrt(~a)"
         (V-TFD) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LENGTH (X &CONTEXT (:330 :440))
         "length(~a)"
         (V-TD) V-DOUBLE
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LENGTH (X &CONTEXT (:330 :440))
         "length(~a)"
         (V-TF) V-FLOAT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LOG (X &CONTEXT (:330 :440))
         "log(~a)"
         (V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :LOG-2 (X &CONTEXT (:330 :440))
         "log2(~a)"
         (V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MATRIX-COMP-MULT (X Y &CONTEXT (:330 :440))
         "matrixCompMult(~a,~a)"
         (V-DMATN V-DMATN) V-DMATN :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MATRIX-COMP-MULT (X Y &CONTEXT (:330 :440))
         "matrixCompMult(~a,~a)"
         (V-MATN V-MATN) V-MATN :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MAX (X Y &CONTEXT (:330 :440))
         "max(~a,~a)"
         (V-TD V-DOUBLE) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MAX (X Y &CONTEXT (:330 :440))
         "max(~a,~a)"
         (V-TF V-FLOAT) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MAX (X Y &CONTEXT (:330 :440))
         "max(~a,~a)"
         (V-TI V-INT) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MAX (X Y &CONTEXT (:330 :440))
         "max(~a,~a)"
         (V-TU V-UINT) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MEMORY-BARRIER (&CONTEXT (:330 :440))
         "memoryBarrier()" NIL
         V-VOID :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MEMORY-BARRIER-ATOMIC-COUNTER (&CONTEXT (:330 :440))
         "memoryBarrierAtomicCounter()" NIL V-VOID :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MEMORY-BARRIER-BUFFER (&CONTEXT (:330 :440))
         "memoryBarrierBuffer()" NIL V-VOID :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MEMORY-BARRIER-IMAGE (&CONTEXT (:330 :440))
         "memoryBarrierImage()" NIL V-VOID :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MEMORY-BARRIER-SHARED (&CONTEXT (:330 :440))
         "memoryBarrierShared()" NIL V-VOID :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MIN (X Y &CONTEXT (:330 :440))
         "min(~a,~a)"
         (V-TD V-DOUBLE) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MIN (X Y &CONTEXT (:330 :440))
         "min(~a,~a)"
         (V-TF V-FLOAT) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MIN (X Y &CONTEXT (:330 :440))
         "min(~a,~a)"
         (V-TI V-INT) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MIN (X Y &CONTEXT (:330 :440))
         "min(~a,~a)"
         (V-TU V-UINT) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MOD (X Y &CONTEXT (:330 :440))
         "mod(~a,~a)"
         (V-TD V-DOUBLE) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :MOD (X Y &CONTEXT (:330 :440))
         "mod(~a,~a)"
         (V-TF V-FLOAT) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :NOISE-1 (X &CONTEXT (:330 :440))
         "noise1(~a)"
         (V-TF) V-FLOAT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :NORMALIZE (X &CONTEXT (:330 :440))
         "normalize(~a)"
         (V-TFD) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-VEC2 V-VEC2) V-MAT2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-VEC3 V-VEC3) V-MAT3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-VEC4 V-VEC4) V-MAT4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :PACK-DOUBLE-2X-3-2 (V &CONTEXT (:330 :440))
         "packDouble2x32(~a)"
         (V-UVEC2) V-DOUBLE :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :PACK-HALF-2X-1-6 (V &CONTEXT (:330 :440))
         "packHalf2x16(~a)"
         (V-VEC2) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :PACK-SNORM-2X-1-6 (V &CONTEXT (:330 :440))
         "packSnorm2x16(~a)"
         (V-VEC2) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :PACK-SNORM-4X-8 (V &CONTEXT (:330 :440))
         "packSnorm4x8(~a)"
         (V-VEC4) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :PACK-UNORM-2X-1-6 (V &CONTEXT (:330 :440))
         "packUnorm2x16(~a)"
         (V-VEC2) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :PACK-UNORM-4X-8 (V &CONTEXT (:330 :440))
         "packUnorm4x8(~a)"
         (V-VEC4) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :RADIANS (DEGREES &CONTEXT (:330 :440))
         "radians(~a)"
         (V-TF) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ROUND (X &CONTEXT (:330 :440))
         "round(~a)"
         (V-TFD) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :ROUND-EVEN (X &CONTEXT (:330 :440))
         "roundEven(~a)"
         (V-TFD) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :SIGN (X &CONTEXT (:330 :440))
         "sign(~a)"
         (V-TFD) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :SIGN (X &CONTEXT (:330 :440))
         "sign(~a)"
         (V-TI) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :SIN (ANGLE &CONTEXT (:330 :440))
         "sin(~a)"
         (V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :SINH (X &CONTEXT (:330 :440))
         "sinh(~a)"
         (V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :SMOOTHSTEP (EDGE0 EDGE1 X &CONTEXT (:330 :440))
         "smoothstep(~a,~a,~a)"
         (V-DOUBLE V-DOUBLE V-TD) 2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :SMOOTHSTEP (EDGE0 EDGE1 X &CONTEXT (:330 :440))
         "smoothstep(~a,~a,~a)"
         (V-FLOAT V-FLOAT V-TF) 2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :SQRT (X &CONTEXT (:330 :440))
         "sqrt(~a)"
         (V-TFD) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :STEP (EDGE X &CONTEXT (:330 :440))
         "step(~a,~a)"
         (V-DOUBLE V-TD)
         1 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :STEP (EDGE X &CONTEXT (:330 :440))
         "step(~a,~a)"
         (V-FLOAT V-TF) 1
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TAN (ANGLE &CONTEXT (:330 :440))
         "tan(~a)"
         (V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TANH (X &CONTEXT (:330 :440))
         "tanh(~a)"
         (V-TF) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRUNC (X &CONTEXT (:330 :440))
         "trunc(~a)"
         (V-TFD) 0 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :UNPACK-DOUBLE-2X-3-2 (V &CONTEXT (:330 :440))
         "unpackDouble2x32(~a)"
         (V-DOUBLE) V-UVEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :UNPACK-HALF-2X-1-6 (V &CONTEXT (:330 :440))
         "unpackHalf2x16(~a)"
         (V-UINT) V-VEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :UNPACK-SNORM-2X-1-6 (P &CONTEXT (:330 :440))
         "unpackSnorm2x16(~a)"
         (V-UINT) V-VEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :UNPACK-SNORM-4X-8 (P &CONTEXT (:330 :440))
         "unpackSnorm4x8(~a)"
         (V-UINT) V-VEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :UNPACK-UNORM-2X-1-6 (P &CONTEXT (:330 :440))
         "unpackUnorm2x16(~a)"
         (V-UINT) V-VEC2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :UNPACK-UNORM-4X-8 (P &CONTEXT (:330 :440))
         "unpackUnorm4x8(~a)"
         (V-UINT) V-VEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRANSPOSE (M &CONTEXT (:330 :440))
         "transpose(~a)"
         (V-MATN) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRANSPOSE (M &CONTEXT (:330 :440))
         "transpose(~a)"
         (V-DMATN) 0
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-DVEC2 V-DVEC2) V-DMAT2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-DVEC3 V-DVEC3) V-DMAT3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-DVEC4 V-DVEC4) V-DMAT4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRANSPOSE (M &CONTEXT (:330 :440))
         "transpose(~a)"
         (V-MAT2X2)
         V-MAT2X2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRANSPOSE (M &CONTEXT (:330 :440))
         "transpose(~a)"
         (V-MAT3X2)
         V-MAT2X3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRANSPOSE (M &CONTEXT (:330 :440))
         "transpose(~a)"
         (V-MAT4X2)
         V-MAT2X4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRANSPOSE (M &CONTEXT (:330 :440))
         "transpose(~a)"
         (V-MAT2X3)
         V-MAT3X2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRANSPOSE (M &CONTEXT (:330 :440))
         "transpose(~a)"
         (V-MAT3X3)
         V-MAT3X3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRANSPOSE (M &CONTEXT (:330 :440))
         "transpose(~a)"
         (V-MAT4X3)
         V-MAT3X4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRANSPOSE (M &CONTEXT (:330 :440))
         "transpose(~a)"
         (V-MAT2X4)
         V-MAT4X2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRANSPOSE (M &CONTEXT (:330 :440))
         "transpose(~a)"
         (V-MAT3X4)
         V-MAT4X3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRANSPOSE (M &CONTEXT (:330 :440))
         "transpose(~a)"
         (V-MAT4X4)
         V-MAT4X4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRANSPOSE (M &CONTEXT (:330 :440))
         "transpose(~a)"
         (V-DMAT2X2)
         V-DMAT2X2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRANSPOSE (M &CONTEXT (:330 :440))
         "transpose(~a)"
         (V-DMAT3X2)
         V-DMAT2X3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRANSPOSE (M &CONTEXT (:330 :440))
         "transpose(~a)"
         (V-DMAT4X2)
         V-DMAT2X4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRANSPOSE (M &CONTEXT (:330 :440))
         "transpose(~a)"
         (V-DMAT2X3)
         V-DMAT3X2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRANSPOSE (M &CONTEXT (:330 :440))
         "transpose(~a)"
         (V-DMAT3X3)
         V-DMAT3X3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRANSPOSE (M &CONTEXT (:330 :440))
         "transpose(~a)"
         (V-DMAT4X3)
         V-DMAT3X4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRANSPOSE (M &CONTEXT (:330 :440))
         "transpose(~a)"
         (V-DMAT2X4)
         V-DMAT4X2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRANSPOSE (M &CONTEXT (:330 :440))
         "transpose(~a)"
         (V-DMAT3X4)
         V-DMAT4X3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :TRANSPOSE (M &CONTEXT (:330 :440))
         "transpose(~a)"
         (V-DMAT4X4)
         V-DMAT4X4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-VEC2 V-VEC2) V-MAT2X2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-VEC3 V-VEC2) V-MAT3X2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-VEC4 V-VEC2) V-MAT4X2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-VEC2 V-VEC3) V-MAT2X3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-VEC3 V-VEC3) V-MAT3X3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-VEC4 V-VEC3) V-MAT4X3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-VEC2 V-VEC4) V-MAT2X4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-VEC3 V-VEC4) V-MAT3X4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-VEC4 V-VEC4) V-MAT4X4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-DVEC2 V-DVEC2) V-DMAT2X2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-DVEC3 V-DVEC2) V-DMAT3X2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-DVEC4 V-DVEC2) V-DMAT4X2 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-DVEC2 V-DVEC3) V-DMAT2X3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-DVEC3 V-DVEC3) V-DMAT3X3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-DVEC4 V-DVEC3) V-DMAT4X3 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-DVEC2 V-DVEC4) V-DMAT2X4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-DVEC3 V-DVEC4) V-DMAT3X4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :OUTER-PRODUCT (C R &CONTEXT (:330 :440))
         "outerProduct(~a,~a)"
         (V-DVEC4 V-DVEC4) V-DMAT4X4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGE1D V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGE1D V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGE1D V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGE1D V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGE1D V-INT V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGE1D V-INT V-UINT V-UINT) V-UINT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGE1D V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGE1D V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGE1D V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGE1D V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGE1D V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGE1D V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGE1D V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGE1D V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGE1D V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGE1D V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-LOAD (IMAGE P &CONTEXT (:330 :440))
         "imageLoad(~a)"
         (V-GIMAGE1D V-INT) V-GVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-STORE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageStore(~a,~a)"
         (V-GIMAGE1D V-INT V-GVEC4) V-VOID :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGE2D V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGE2D V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGE2D V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGE2D V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGE2D V-IVEC2 V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGE2D V-IVEC2 V-UINT V-UINT) V-UINT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGE2D V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGE2D V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGE2D V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGE2D V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGE2D V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGE2D V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGE2D V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGE2D V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGE2D V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGE2D V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-LOAD (IMAGE P &CONTEXT (:330 :440))
         "imageLoad(~a)"
         (V-GIMAGE2D V-IVEC2) V-GVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-STORE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageStore(~a,~a)"
         (V-GIMAGE2D V-IVEC2 V-GVEC4) V-VOID :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGE3D V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGE3D V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGE3D V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGE3D V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGE3D V-IVEC3 V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGE3D V-IVEC3 V-UINT V-UINT) V-UINT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGE3D V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGE3D V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGE3D V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGE3D V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGE3D V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGE3D V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGE3D V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGE3D V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGE3D V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGE3D V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-LOAD (IMAGE P &CONTEXT (:330 :440))
         "imageLoad(~a)"
         (V-GIMAGE3D V-IVEC3) V-GVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-STORE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageStore(~a,~a)"
         (V-GIMAGE3D V-IVEC3 V-GVEC4) V-VOID :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGE2DRECT V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGE2DRECT V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGE2DRECT V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGE2DRECT V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGE2DRECT V-IVEC2 V-INT V-INT) V-INT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGE2DRECT V-IVEC2 V-UINT V-UINT) V-UINT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGE2DRECT V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGE2DRECT V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGE2DRECT V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGE2DRECT V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGE2DRECT V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGE2DRECT V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGE2DRECT V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGE2DRECT V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGE2DRECT V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGE2DRECT V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-LOAD (IMAGE P &CONTEXT (:330 :440))
         "imageLoad(~a)"
         (V-GIMAGE2DRECT V-IVEC2) V-GVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-STORE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageStore(~a,~a)"
         (V-GIMAGE2DRECT V-IVEC2 V-GVEC4) V-VOID :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGECUBE V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGECUBE V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGECUBE V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGECUBE V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGECUBE V-IVEC3 V-INT V-INT) V-INT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGECUBE V-IVEC3 V-UINT V-UINT) V-UINT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGECUBE V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGECUBE V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGECUBE V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGECUBE V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGECUBE V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGECUBE V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGECUBE V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGECUBE V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGECUBE V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGECUBE V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-LOAD (IMAGE P &CONTEXT (:330 :440))
         "imageLoad(~a)"
         (V-GIMAGECUBE V-IVEC3) V-GVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-STORE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageStore(~a,~a)"
         (V-GIMAGECUBE V-IVEC3 V-GVEC4) V-VOID :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGEBUFFER V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGEBUFFER V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGEBUFFER V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGEBUFFER V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGEBUFFER V-INT V-INT V-INT) V-INT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGEBUFFER V-INT V-UINT V-UINT) V-UINT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGEBUFFER V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGEBUFFER V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGEBUFFER V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGEBUFFER V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGEBUFFER V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGEBUFFER V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGEBUFFER V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGEBUFFER V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGEBUFFER V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGEBUFFER V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-LOAD (IMAGE P &CONTEXT (:330 :440))
         "imageLoad(~a)"
         (V-GIMAGEBUFFER V-INT) V-GVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-STORE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageStore(~a,~a)"
         (V-GIMAGEBUFFER V-INT V-GVEC4) V-VOID :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGE1DARRAY V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGE1DARRAY V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGE1DARRAY V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGE1DARRAY V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGE1DARRAY V-IVEC2 V-INT V-INT) V-INT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGE1DARRAY V-IVEC2 V-UINT V-UINT)
         V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGE1DARRAY V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGE1DARRAY V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGE1DARRAY V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGE1DARRAY V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGE1DARRAY V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGE1DARRAY V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGE1DARRAY V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGE1DARRAY V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGE1DARRAY V-IVEC2 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGE1DARRAY V-IVEC2 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-LOAD (IMAGE P &CONTEXT (:330 :440))
         "imageLoad(~a)"
         (V-GIMAGE1DARRAY V-IVEC2) V-GVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-STORE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageStore(~a,~a)"
         (V-GIMAGE1DARRAY V-IVEC2 V-GVEC4) V-VOID :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGE2DARRAY V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGE2DARRAY V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGE2DARRAY V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGE2DARRAY V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGE2DARRAY V-IVEC3 V-INT V-INT) V-INT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGE2DARRAY V-IVEC3 V-UINT V-UINT)
         V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGE2DARRAY V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGE2DARRAY V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGE2DARRAY V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGE2DARRAY V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGE2DARRAY V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGE2DARRAY V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGE2DARRAY V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGE2DARRAY V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGE2DARRAY V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGE2DARRAY V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-LOAD (IMAGE P &CONTEXT (:330 :440))
         "imageLoad(~a)"
         (V-GIMAGE2DARRAY V-IVEC3) V-GVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-STORE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageStore(~a,~a)"
         (V-GIMAGE2DARRAY V-IVEC3 V-GVEC4) V-VOID :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGECUBEARRAY V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGECUBEARRAY V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGECUBEARRAY V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGECUBEARRAY V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGECUBEARRAY V-IVEC3 V-INT V-INT) V-INT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGECUBEARRAY V-IVEC3 V-UINT V-UINT)
         V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGECUBEARRAY V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGECUBEARRAY V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGECUBEARRAY V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGECUBEARRAY V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGECUBEARRAY V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGECUBEARRAY V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGECUBEARRAY V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGECUBEARRAY V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGECUBEARRAY V-IVEC3 V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGECUBEARRAY V-IVEC3 V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-LOAD (IMAGE P &CONTEXT (:330 :440))
         "imageLoad(~a)"
         (V-GIMAGECUBEARRAY V-IVEC3) V-GVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-STORE (IMAGE P DATA &CONTEXT (:330 :440))
         "imageStore(~a,~a)"
         (V-GIMAGECUBEARRAY V-IVEC3 V-GVEC4) V-VOID :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGE2DMS V-IVEC2 V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGE2DMS V-IVEC2 V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGE2DMS V-IVEC2 V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGE2DMS V-IVEC2 V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P SAMPLE COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGE2DMS V-IVEC2 V-INT V-INT V-INT)
         V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P SAMPLE COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGE2DMS V-IVEC2 V-INT V-UINT V-UINT)
         V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGE2DMS V-IVEC2 V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGE2DMS V-IVEC2 V-INT V-UINT) V-UINT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGE2DMS V-IVEC2 V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGE2DMS V-IVEC2 V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGE2DMS V-IVEC2 V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGE2DMS V-IVEC2 V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGE2DMS V-IVEC2 V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGE2DMS V-IVEC2 V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGE2DMS V-IVEC2 V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGE2DMS V-IVEC2 V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-LOAD (IMAGE P SAMPLE &CONTEXT (:330 :440))
         "imageLoad(~a)"
         (V-GIMAGE2DMS V-IVEC2 V-INT) V-GVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-STORE (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageStore(~a,~a)"
         (V-GIMAGE2DMS V-IVEC2 V-INT V-GVEC4) V-VOID :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGE2DMSARRAY V-IVEC3 V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-ADD (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicAdd(~a,~a)"
         (V-GIMAGE2DMSARRAY V-IVEC3 V-INT V-UINT) V-UINT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGE2DMSARRAY V-IVEC3 V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-AND (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicAnd(~a,~a)"
         (V-GIMAGE2DMSARRAY V-IVEC3 V-INT V-UINT) V-UINT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P SAMPLE COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGE2DMSARRAY V-IVEC3 V-INT V-INT V-INT)
         V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-COMP-SWAP
         (IMAGE P SAMPLE COMPARE DATA &CONTEXT (:330 :440))
         "imageAtomicCompSwap(~a,~a,~a)"
         (V-GIMAGE2DMSARRAY V-IVEC3 V-INT V-UINT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGE2DMSARRAY V-IVEC3 V-INT V-INT) V-INT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-EXCHANGE (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicExchange(~a,~a)"
         (V-GIMAGE2DMSARRAY V-IVEC3 V-INT V-UINT) V-UINT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGE2DMSARRAY V-IVEC3 V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MAX (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicMax(~a,~a)"
         (V-GIMAGE2DMSARRAY V-IVEC3 V-INT V-UINT) V-UINT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGE2DMSARRAY V-IVEC3 V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-MIN (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicMin(~a,~a)"
         (V-GIMAGE2DMSARRAY V-IVEC3 V-INT V-UINT) V-UINT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGE2DMSARRAY V-IVEC3 V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-OR (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicOr(~a,~a)"
         (V-GIMAGE2DMSARRAY V-IVEC3 V-INT V-UINT) V-UINT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGE2DMSARRAY V-IVEC3 V-INT V-INT) V-INT :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-ATOMIC-XOR (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageAtomicXor(~a,~a)"
         (V-GIMAGE2DMSARRAY V-IVEC3 V-INT V-UINT) V-UINT
         :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-LOAD (IMAGE P SAMPLE &CONTEXT (:330 :440))
         "imageLoad(~a)"
         (V-GIMAGE2DMSARRAY V-IVEC3 V-INT) V-GVEC4 :PLACE NIL :GLSL-SPEC-MATCHING T)

(V-DEFUN :IMAGE-STORE (IMAGE P SAMPLE DATA &CONTEXT (:330 :440))
         "imageStore(~a,~a)"
         (V-GIMAGE2DMSARRAY V-IVEC3 V-INT V-GVEC4) V-VOID :PLACE NIL :GLSL-SPEC-MATCHING T)
