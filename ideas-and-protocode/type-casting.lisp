;; Within any one function, type sizes and dimensionality must correspond after implicit type
;; conversions. For example, float round(float) is supported, but float round(vec4) is not.

;; When performing implicit conversion for binary operators, there may be multiple data types
;; to which the two operands can be converted. For example, when adding an int value to a uint
;; value, both values can be implicitly converted to uint, float, and double. In such cases, 
;; a floating-point type is chosen if either operand has a floating-point type. Otherwise, an
;; unsigned integer type is chosen if either operand has an unsigned integer type. Otherwise, a
;; signed integer type is chosen. If operands can be implicitly converted to multiple data types
;; deriving from the same base data type, the type with the smallest component size is used.

;; Tf=float, vecn 
;; Tb=bool, bvecn
;; Ti=int, ivecn
;; Tu=uint, uvecn
;; Td= double, dvecn
;; - - - - - - - - -
;; Tfd= Tf, Td 
;; Tiu= Ti, Tu

;; Tiu clamp(Tiu x, Tiu minVal, Tiu maxVal)
;; Tfd frexp(Tfd x, out Ti exp)

;; frexp(float, ivec2)

;; find all non core types in spec,
;; these must match afterwards

;; take the instersection of all the casts and primary types

;; ------------------------------

;; * do all generic typed args have the same length. if no then fail. else
;; * if args match exactly then woo score 0, othewise
;; * if all can cast then woo record the first cast that works.

;; (f d v2 v3 v4 d2 d3 d4), (i i2 i3 i4)
