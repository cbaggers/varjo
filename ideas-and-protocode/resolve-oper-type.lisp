(defconstant *implicit-type-casts* '((:float :int :uint)
                                     (:vec2 :ivec2 :uvec2)
                                     (:vec3 :ivec3 :uvec3)
                                     (:vec4 :ivec4 :uvec4)))

(defparameter *types-blah* '(:void :bool :int :uint :float :bvec2 :bvec3 :bvec4 :uvec2 :uvec3 :uvec4 :ivec2 :ivec3 :ivec4 :vec2 :vec3 :vec4))

(defun glsl-castablep (minor-type major-type)
  "Returns whether the type minor-type can be cast up to type major-type"
  (not (null (find minor-type (assoc major-type *implicit-type-casts*)))))

(defun superior-type (&rest types)
  "find the superior type, types are defined in order or superiority."
  (elt *types-blah* 
       (apply #'max (mapcar (lambda (x) (position x *types-blah*))
                            types))))

(defun oper-types-compatible (&rest types)
  "Make sure every type is or can be cast up to the superior type"
  (let ((superior (apply #'superior-type types)))
    (every #'(lambda (x) (glsl-castablep x superior)) types)))

;; The Expressions Section of the GLSL Spec (5.9) is the bible on conversions
;; and what makes an operator use valid.

;; [TODO] Add the following to the language spec
;; bool(float)
;; bool(int)
;; bool(uint)
;; float(bool)
;; float(int)
;; float(uint)
;; int(bool)
;; int(float)
;; int(uint)
;; uint(bool)
;; uint(float)
;; uint(int)
;; also see the constructors section of the glsl spec, will need macros for that

;; [TODO] Add way of specifying type for litertal numbers 1.4f and 1u or whatever glsl has
;; [TODO] Add bool literal 
;; [TODO] find out what matrix order glsl functions operate on
;; [TODO] add invariance predicate, used in function type 
;;        specification


;; The arithmetic binary operators add (+), subtract (-), multiply (*), and divide (/) operate on integer and
;; floating-point scalars, vectors, and matrices.

;; + - / * both scalar - same type
;;         scalar & vec or mat - vec or mat
;;         vec & vec  - vec
;;         if op + - / then if mat with same dimen then mat
;;         if * and mat & mat or mat & vec then if col(a) == row(b) then result = thing with dimen (row(a),col(b))
;; % only on int or uint or vec of those types
;; < > >= <= only on uint int float  result is bool
;; == and != work on all types and return bool
;; && || ^^ ! only operate on bools
;; , is like progn returns type of last expression
;; ?: takes bool and two expr that must have compatible type
;; ~ int or uint or ivec result type same
;; >> << int uint or ivec one can be signed while other unsigned 
;;       if one scalar then other must be scalar 
;;       if first is vec then second must be vec or scalar
;; & | ^ int or uint or ivec* of same size      
;; 
