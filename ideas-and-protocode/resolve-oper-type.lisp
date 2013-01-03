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

;; treat % , << >> & | ^ && || ^^ as function
;; maybe most of these can be treated as macros which result in functions
;; e.g. (|| 1 2 3) can result in (%|| 1 (%|| 2 3))
;;   or (+ 2 3 4) can result in (%+ 2 (%+ 3 4)
;; then the operators can be functions which only need to be concerned with
;; left and right operands. The result will be syntactically correct

;; sequence operator does not like literals...don't know why see 4.20 glsl spec..WRONG!!!
;;  it is totally cool to do this but it cannot be used as a CONSTANT expression 
;;  (read up on them), basically it means we couldnt define a glsl array with (2,3) as the
;;  length.

;; ok so now the final one...do we force + - * / to take a max of two args?
;;      pros: easy to implement/control
;;            removes another layer of complexity from compiler
;;       
;; negatives: unnecesary parens (hehe worrying about parens in lisp hehe)

;; '(making (making games) a game)

;; fiddly fecker test
(let ((a 1)
      (b 2.4))
  (let (c (* a b))
    (setf b 3.0)
    (setf c (+ b 1.0))
    (% c 2))
  (jam 1 2))

;; int a = 1;
;; float b = 2.4;
;; float c = a * b;
;; (((b = 3.0),
;;   (c = (b + 1.0)),
;;   (c % 2)),
;;  jam(1, 2));

;; what if we don't allow progn?
;; seems drastic we should test this first

(jam (let ((a 4))
       (setf a (* a 2))
       (let ((b a))
         (thing b))))
;; int a = 4;
;; a = (a * 2);
;; int b = a;
;; jam(thing(b));


(jam (let ((a 4))
       (setf a (* a 2))
       (setf c (let ((b a)) (thing b)))))
;; so for function compiling, if code has multiple lines then pick last one
;; let type based on last expression
;; need %block for this.

(jam (let ((a 1)) 
       (man (setf a (+ a 1)) 2)))

;; int a = 1;
;; a = a + 1;
;; jam(man(a 2));

;; add this to utils - its from ansi common lisp
(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v))
                rest
                :initial-value (apply fn1 args)))))


;; block logic
;; if only one arg then just passthrough
;; else make a list of each args block code follwed by line-code
;; but only include block of last arg
;; '(arg-1-block arg-1-code arg-2-block arg-2-code.. arg-n-block)
;;  ^^ this is new block
;; arg-n-code is now the current line and type.
;; This covers progn but others are more tricky

;;this is ok
(let ((meh 1))
  1
  2
  3)

(progn (%typeify (setf meh 1))
       1
       2
       3)

;;let obviously replaces tokens where needed
(let ((a 4.2))
  (toast (let ((a 2)) (jam a))
         a))

(progn (%typeify (setf a 4.2))
       (toast (progn (%typeify (setf _a 2))
                     (jam _a))
              a))
;; that'll work too

(man (setf a (+ a 1)) 2)
;; if we are strict then the a=a+1 should be block and line should 
;; be man (a,2).... not too urgent though


;; [TODO] make the %typify function. type is void only adds type to line code.
;; [TODO] add strict mode where setf's type if void
;; [TODO] rename code and to-block to current-line and ?cache?.hmmm
;; [TODO] make progn special
;; [TODO] remove all parts of function spec which deal with block...its no
;;        longer relevant

