(in-package :vari.cl)

;;------------------------------------------------------------

(v-def-glsl-template-fun symbolp (a) "false" (v-type) v-bool)
(v-def-glsl-template-fun keywordp (a) "false" (v-type) v-bool)
(v-def-glsl-template-fun vectorp (a) "true" (v-array) v-bool)
(v-def-glsl-template-fun array-rank (a) "1" (v-array) v-int)
(v-def-glsl-template-fun array-row-major-index (a i) "~a" (v-array v-int) v-int)

(v-def-glsl-template-fun adjustable-array-p (a) "false" (v-array) v-bool)
(v-def-glsl-template-fun array-has-fill-pointer-p (a) "false" (v-array) v-bool)

(v-def-glsl-template-fun bit-vector-p (a) "false" (v-array) v-bool)
(v-def-glsl-template-fun compiled-function-p (a) "true" (v-function-type) v-bool)


(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-real) 0 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-vector) 0 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-matrix) 0 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-real) 0 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-vector) 0 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-matrix) 0 :pure t)

;; not really accurate as loses side effects of prototype
;; we use the compiler macro to patch this up
(v-def-glsl-template-fun float (x p) "float(~a)" (v-real v-float) 0 :pure t)
(v-def-glsl-template-fun float (x p) "double(~a)" (v-real v-double) 0 :pure t)

(v-define-compiler-macro float ((x v-real) (p v-float))
  (if (numberp p)
      `(float ,x)
      `(progn ,p (float ,x))))

(v-define-compiler-macro float ((x v-real) (p v-double))
  (if (numberp p)
      `(double ,x)
      `(progn ,p (double ,x))))


(macrolet ((define-type-pred (func-name &rest v-types)
             `(progn
                (v-def-glsl-template-fun ,func-name (x) "<invalid>" (v-type)
                                         v-bool :pure t)
                (v-define-compiler-macro ,func-name ((x v-type)
                                                     &environment env)
                  (let ((type (varjo:argument-type 'x env)))
                    (list 'progn
                          x
                          (or ,@(loop :for v-type :in v-types :collect
                                   `(v-typep type ',v-type)))))))))
  (define-type-pred arrayp v-array)
  (define-type-pred simple-vector-p v-array)
  (define-type-pred vectorp v-array)
  (define-type-pred functionp v-function-type)
  (define-type-pred complexp v-complex)
  (define-type-pred numberp v-number)
  (define-type-pred realp v-real)
  (define-type-pred floatp v-float v-double)
  (define-type-pred integerp v-integer)
  (define-type-pred rationalp v-rational))


(v-def-glsl-template-fun simple-bit-vector-p (a) "false" (v-type) v-bool)
(v-def-glsl-template-fun random-state-p (a) "false" (v-type) v-bool)



;; Could use v-integer for both of these when we add that
(v-def-glsl-template-fun integer-length (x) "32" (v-int) 0 :pure t)
(v-define-compiler-macro integer-length ((x v-int))
  (if (numberp x)
      32
      `(progn ,x 32)))

(v-def-glsl-template-fun integer-length (x) "32" (v-uint) 0 :pure t)
(v-define-compiler-macro integer-length ((x v-uint))
  (if (numberp x)
      32
      `(progn ,x 32)))


(v-def-glsl-template-fun signum (x) "sign(~a)" (v-real) 0 :pure t)
(v-def-glsl-template-fun evenp (x) "(~a % 2 == 0)" (v-int) v-bool :pure t)
(v-def-glsl-template-fun evenp (x) "(~a % 2 == 0)" (v-uint) v-bool :pure t)
(v-def-glsl-template-fun oddp (x) "(~a % 2 != 0)" (v-int) v-bool :pure t)
(v-def-glsl-template-fun oddp (x) "(~a % 2 != 0)" (v-uint) v-bool :pure t)

(v-def-glsl-template-fun minusp (x) "(~a < 0)" (v-real) v-bool :pure t)
(v-def-glsl-template-fun plusp (x) "(~a > 0)" (v-real) v-bool :pure t)

(v-defmacro dotimes ((var count &optional result) &body body)
  (assert (not result) () "Varjo: Currently we do not support the result form")
  (assert (symbolp var) () "Varjo: the var for dotimes was not a symbol")
  (let ((gcount (gensym "count")))
    `(let ((,gcount ,count))
       (for (,var 0) (< ,var ,gcount) (++ ,var)
            ,@body))))

(v-def-glsl-template-fun array-total-size (x) "~a.length()" (v-array)
                         v-int :pure t)

(v-define-compiler-macro array-total-size
    (&environment env &whole whole (x v-array))
  (let* ((type (varjo:argument-type 'x env))
         (len (first (v-dimensions type))))
    (if (numberp len)
        `(progn
           ,x
           ,len)
        whole)))

;; reading
;;
;; phase
;; isqrt
;; logand
;; logandc1
;; logandc2
;; logcount
;; logeqv
;; logior
;; lognand
;; lognor
;; lognot
;; logorc1
;; logorc2
;; logtest
;; logxor
;; make-random-state (could be a seed for some rand func?)
;; with-accessors
;; rotatef
;; shiftf
;; multiple-value-setq
;; multiple-value-call
;; multiple-value-prog1
;; nth-value
;; function
;; scale-float

;; harder
;; log (with optional arg)
;; complement (returns a function)

;; rem
;; logbitp
;; typecase

;;------------------------------------------------------------
