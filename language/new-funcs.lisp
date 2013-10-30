
(v-defun %VOID NIL
  ""
  NIL :VOID)

(v-defun F-TRANSFORM (&CONTEXT (:|330|))
  "ftransform()"
  NIL :VEC4)

(v-defun DISCARD (&CONTEXT :FRAGMENT)
  "discard()"
  NIL :NONE)

(v-defun BREAK (&CONTEXT (:|330|))
  "break"
  NIL :NONE)

(v-defun CONTINUE (&CONTEXT (:|330|))
  "continue"
  NIL :NONE)

(v-defun EMIT-VERTEX (&CONTEXT :GEOMETRY)
  "EmitVertex()"
  NIL :VOID)

(v-defun END-PRIMITIVE (&CONTEXT :GEOMETRY)
  "EndPrimitive()"
  NIL :VOID)

(v-defun X (VEC &CONTEXT (:|330|))
  "~a.x"
  (V-VECTOR) (:ELEMENT 0))

(v-defun Y (VEC &CONTEXT (:|330|))
  "~a.y"
  (V-VECTOR) :BOOL)

(v-defun Z (VEC &CONTEXT (:|330|))
  "~a.z"
  (V-VEC3-UP) :BOOL)

(v-defun W (VEC &CONTEXT (:|330|))
  "~a.w"
  (V-VEC4-UP) :BOOL)

(v-defun BOOL (X &CONTEXT (:|330|))
  "bool(~a)"
  (((:DOUBLE :FLOAT :INT :UINT :BOOL :BVEC2 :BVEC3 :BVEC4))) :BOOL)

(v-defun DOUBLE (X &CONTEXT (:|330|))
  "double(~a)"
  (((:BOOL :FLOAT :INT :UINT :DOUBLE))) :DOUBLE)

(v-defun FLOAT (X &CONTEXT (:|330|))
  "float(~a)"
  (((:BOOL :DOUBLE :INT :UINT :FLOAT :VEC2 :VEC3 :VEC4))) :FLOAT)

(v-defun INT (X &CONTEXT (:|330|))
  "int(~a)"
  (((:BOOL :DOUBLE :FLOAT :UINT :INT :IVEC2 :IVEC3 :IVEC4))) :INT)

(v-defun UINT (X &CONTEXT (:|330|))
  "uint(~a)"
  (((:BOOL :DOUBLE :FLOAT :INT :UINT :UVEC2 :UVEC3 :UVEC4))) :UINT)

(v-defun UINT (X &CONTEXT (:|330|))
  "uint(~a)"
  (((:BOOL :DOUBLE :FLOAT :INT :UINT :UVEC2 :UVEC3 :UVEC4))) :UINT)

(v-defun DEGREES (RADIANS &CONTEXT (:|330|))
  "degrees(~a)"
  (ALL-FLOATS) 0)

(v-defun RADIANS (DEGREES &CONTEXT (:|330|))
  "radians(~a)"
  (ALL-FLOATS) 0)

(v-defun SIN (ANGLE &CONTEXT (:|330|))
  "sin(~a)"
  (ALL-FLOATS) 0)

(v-defun COS (ANGLE &CONTEXT (:|330|))
  "cos(~a)"
  (ALL-FLOATS) 0)

(v-defun TAN (ANGLE &CONTEXT (:|330|))
  "tan(~a)"
  (ALL-FLOATS) 0)

(v-defun ASIN (X &CONTEXT (:|330|))
  "asin(~a)"
  (ALL-FLOATS) 0)

(v-defun ACOS (X &CONTEXT (:|330|))
  "acos(~a)"
  (ALL-FLOATS) 0)

(v-defun ATAN (Y X &CONTEXT (:|330|))
  "atan(~a, ~a)"
  (ALL-FLOATS ALL-FLOATS) 0)

(v-defun ATAN (Y-OVER-X &CONTEXT (:|330|))
  "atan(~a)"
  (ALL-FLOATS) 0)

(v-defun SINH (ANGLE &CONTEXT (:|330|))
  "sinh(~a)"
  (ALL-FLOATS) 0)

(v-defun COSH (ANGLE &CONTEXT (:|330|))
  "cosh(~a)"
  (ALL-FLOATS) 0)

(v-defun TANH (ANGLE &CONTEXT (:|330|))
  "tanh(~a)"
  (ALL-FLOATS) 0)

(v-defun ASINH (ANGLE &CONTEXT (:|330|))
  "asinh(~a)"
  (ALL-FLOATS) 0)

(v-defun ACOSH (ANGLE &CONTEXT (:|330|))
  "acosh(~a)"
  (ALL-FLOATS) 0)

(v-defun ATANH (ANGLE &CONTEXT (:|330|))
  "atanh(~a)"
  (ALL-FLOATS) 0)

(v-defun EXP (X &CONTEXT (:|330|))
  "exp(~a)"
  (ALL-FLOATS) 0)

(v-defun LOG (X &CONTEXT (:|330|))
  "log(~a)"
  (ALL-FLOATS) 0)

(v-defun EXP2 (X &CONTEXT (:|330|))
  "exp2(~a)"
  (ALL-FLOATS) 0)

(v-defun LOG2 (X &CONTEXT (:|330|))
  "log2(~a)"
  (ALL-FLOATS) 0)

(v-defun SQRT (X &CONTEXT (:|330|))
  "exp(~a)"
  (ALL-FLOATS) 0)

(v-defun INVERSESQRT (X &CONTEXT (:|330|))
  "inversesqrt(~a)"
  (ALL-FLOATS) 0)

(v-defun ABS (X &CONTEXT (:|330|))
  "abs(~a)"
  (ALL-FLOATS-AND-INTS) 0)

(v-defun SIGN (X &CONTEXT (:|330|))
  "sign(~a)"
  (ALL-FLOATS-AND-INTS)
  (:FLOAT NIL))

(v-defun FLOOR (X &CONTEXT (:|330|))
  "floor(~a)"
  (ALL-FLOATS) (:INT NIL))

(v-defun TRUNC (X &CONTEXT (:|330|))
  "trunc(~a)"
  (ALL-FLOATS) (:INT NIL))

(v-defun ROUND (X &CONTEXT (:|330|))
  "round(~a)"
  (ALL-FLOATS) (:INT NIL))

(v-defun ROUND-EVEN (X &CONTEXT (:|330|))
  "roundEven(~a)"
  (ALL-FLOATS)
  (:INT NIL))

(v-defun CEIL (X &CONTEXT (:|330|))
  "ceil(~a)"
  (ALL-FLOATS) (:INT NIL))

(v-defun FRACT (X &CONTEXT (:|330|))
  "fract(~a)"
  (ALL-FLOATS) 0)

(v-defun IS-NAN (X &CONTEXT (:|330|))
  "isnan(~a, ~a, ~a)"
  (:FLOAT) V-BOOL)

(v-defun IS-NAN (X &CONTEXT (:|330|))
  "isnan(~a, ~a, ~a)"
  (:VEC2) (:BVEC2 NIL))

(v-defun IS-NAN (X &CONTEXT (:|330|))
  "isnan(~a, ~a, ~a)"
  (:VEC3) (:BVEC3 NIL))

(v-defun IS-NAN (X &CONTEXT (:|330|))
  "isnan(~a, ~a, ~a)"
  (:VEC4) (:BVEC4 NIL))

(v-defun LENGTH (X &CONTEXT (:|330|))
  "length(~a)"
  (ALL-FLOATS) :FLOAT)

(v-defun NORMALIZE (X &CONTEXT (:|330|))
  "normalize(~a)"
  (ALL-FLOATS) 0)

(v-defun TRANSPOSE (M &CONTEXT (:|330|))
  "transpose(~a)"
  (V-SQUARE) 0)

(v-defun TRANSPOSE (M)
  "transpose(~a)"
  (V-MAT2X3) V-MAT3X2)

(v-defun TRANSPOSE (M &CONTEXT (:|330|))
  "transpose(~a)"
  (V-MAT2X4) V-MAT4X2)

(v-defun TRANSPOSE (M)
  "transpose(~a)"
  (V-MAT3X2) V-MAT2X3)

(v-defun TRANSPOSE (M &CONTEXT (:|330|))
  "transpose(~a)"
  (V-MAT3X4) V-MAT4X3)

(v-defun TRANSPOSE (M)
  "transpose(~a)"
  (V-MAT4X3) V-MAT3X4)

(v-defun TRANSPOSE (M &CONTEXT (:|330|))
  "transpose(~a)"
  (V-MAT4X2) V-MAT2X4)

(v-defun DETERMINANT (M &CONTEXT (:|330|))
  "determinant(~a)"
  (V-MATRIX) :FLOAT)

(v-defun INVERSE (M &CONTEXT (:|330|))
  "inverse(~a)"
  (V-SQUARE) 0)

(v-defun B-ANY (X &CONTEXT (:|330|))
  "any(~a)"
  (V-BVECTOR) :BOOL)

(v-defun B-NOT (X &CONTEXT (:|330|))
  "not(~a)"
  (V-BVECTOR) 0)

(v-defun DFDX (I &CONTEXT :FRAGMENT)
  "dFdx(~a)"
  (ALL-FLOATS) 0)

(v-defun DFDY (I &CONTEXT :FRAGMENT)
  "dFdy(~a)"
  (ALL-FLOATS) 0)

(v-defun F-WIDTH (I &CONTEXT :FRAGMENT)
  "fwidth(~a)"
  (ALL-FLOATS) 0)

(v-defun NOISE-1 (I &CONTEXT (:|330|))
  "noise1(~a)"
  (ALL-FLOATS) :FLOAT)

(v-defun NOISE-2 (I &CONTEXT (:|330|))
  "noise2(~a)"
  (ALL-FLOATS) :VEC2)

(v-defun NOISE-3 (I &CONTEXT (:|330|))
  "noise3(~a)"
  (ALL-FLOATS) :VEC2)

(v-defun NOISE-4 (I &CONTEXT (:|330|))
  "noise4(~a)"
  (ALL-FLOATS) :VEC2)

(v-defun INCF (X &CONTEXT (:|330|))
  "(~a++)"
  ((FLOAT-AND-INT NIL NIL)) 0)

(v-defun DECF (X &CONTEXT (:|330|))
  "(~a--)"
  ((FLOAT-AND-INT NIL NIL)) 0)

(v-defun ++ (X &CONTEXT (:|330|))
  "(++~a)"
  ((FLOAT-AND-INT NIL NIL)) 0)

(v-defun -- (X &CONTEXT (:|330|))
  "(--~a)"
  ((FLOAT-AND-INT NIL NIL)) 0)

(v-defun ! (A &CONTEXT (:|330|))
  "(! ~a)"
  (V-BOOL) V-BOOL)

(v-defun ~ (A &CONTEXT (:|330|))
  "(~ ~a)"
  (((:INT :UINT :IVEC2 :IVEC3 :IVEC4) NIL)) 0)

(v-defun B-ALL (X &CONTEXT (:|330|))
  "all(~a)"
  (V-BVECTOR) :BOOL)
