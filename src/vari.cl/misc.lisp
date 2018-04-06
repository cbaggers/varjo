(in-package :vari.cl)

;;------------------------------------------------------------

(defmacro define-type-pred (func-name func-arg pred-func args
                            &optional (then t) (else nil))
  (destructuring-bind (arg-name arg-type) func-arg
    (let ((args (or (uiop:ensure-list args)
                    '(nil)))
          (glsl (format nil "£-~a-£(~~a)" func-name)))
      `(progn
         (v-def-glsl-template-fun ,func-name (,arg-name) ,glsl
                                  (,arg-type)
                                  v-bool :pure t)
         (v-define-compiler-macro ,func-name (,func-arg
                                              &environment env)
           (declare (ignorable ,arg-name))
           ,(if (equal then else)
                then
                `(let ((type (varjo:argument-type ',arg-name env)))
                   (list 'progn
                         ,arg-name
                         (if (or ,@(loop :for arg :in args :collect
                                      `(,pred-func type ',arg)))
                             ,then
                             ,else)))))))))

(define-type-pred arrayp (x v-type) v-typep v-array)
(define-type-pred simple-vector-p (x v-type) v-typep v-array)
(define-type-pred vectorp (x v-type) v-typep v-array)
(define-type-pred functionp (x v-type) v-typep v-function-type)
(define-type-pred complexp (x v-type) v-typep v-complex)
(define-type-pred numberp (x v-type) v-typep v-number)
(define-type-pred realp (x v-type) v-typep v-real)
(define-type-pred floatp (x v-type) v-typep (v-float v-double))
(define-type-pred integerp (x v-type) v-typep v-integer)
(define-type-pred rationalp (x v-type) v-typep v-rational)
(define-type-pred symbolp (x v-type) typep nil)
(define-type-pred keywordp (x v-type) typep nil)
(define-type-pred simple-bit-vector-p (x v-type) typep nil)
(define-type-pred random-state-p (x v-type) typep nil)
(define-type-pred vectorp (x v-type) v-typep v-vector)
(define-type-pred adjustable-array-p (x v-array) nil nil nil nil)
(define-type-pred array-has-fill-pointer-p (x v-array) nil nil nil nil)
(define-type-pred bit-vector-p (x v-array) nil nil nil nil)
(define-type-pred array-rank (x v-array) nil nil 1 1)
(define-type-pred compiled-function-p (x v-function-type) nil nil t t)

(v-def-glsl-template-fun array-row-major-index (a i) "~a" (v-array v-int) v-int)

;;------------------------------------------------------------

(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-double) v-double :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-short-float) v-short-float :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-float) v-float :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (:int) :int :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (:uint) :uint :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-rational) v-rational :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-dvec4) v-dvec4 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-dvec3) v-dvec3 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-dvec2) v-dvec2 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-ivec4) v-ivec4 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-ivec3) v-ivec3 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-ivec2) v-ivec2 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-uvec4) v-uvec4 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-uvec3) v-uvec3 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-uvec2) v-uvec2 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-bvec4) v-bvec4 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-bvec3) v-bvec3 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-bvec2) v-bvec2 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-vec4) v-vec4 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-vec3) v-vec3 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-vec2) v-vec2 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-mat4x4) v-mat4x4 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-mat4x3) v-mat4x3 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-mat4x2) v-mat4x2 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-mat3x4) v-mat3x4 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-mat3x3) v-mat3x3 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-mat3x2) v-mat3x2 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-mat2x4) v-mat2x4 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-mat2x3) v-mat2x3 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-mat2x2) v-mat2x2 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-mat4) v-mat4 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-mat3) v-mat3 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-mat2) v-mat2 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-dmat4x4) v-dmat4x4 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-dmat4x3) v-dmat4x3 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-dmat4x2) v-dmat4x2 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-dmat3x4) v-dmat3x4 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-dmat3x3) v-dmat3x3 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-dmat3x2) v-dmat3x2 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-dmat2x4) v-dmat2x4 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-dmat2x3) v-dmat2x3 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-dmat2x2) v-dmat2x2 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-dmat4) v-dmat4 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-dmat3) v-dmat3 :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + 1)" (v-dmat2) v-dmat2 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-double) v-double :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-short-float) v-short-float :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-float) v-float :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (:int) :int :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (:uint) :uint :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-rational) v-rational :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-dvec4) v-dvec4 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-dvec3) v-dvec3 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-dvec2) v-dvec2 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-ivec4) v-ivec4 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-ivec3) v-ivec3 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-ivec2) v-ivec2 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-uvec4) v-uvec4 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-uvec3) v-uvec3 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-uvec2) v-uvec2 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-bvec4) v-bvec4 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-bvec3) v-bvec3 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-bvec2) v-bvec2 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-vec4) v-vec4 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-vec3) v-vec3 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-vec2) v-vec2 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-mat4x4) v-mat4x4 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-mat4x3) v-mat4x3 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-mat4x2) v-mat4x2 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-mat3x4) v-mat3x4 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-mat3x3) v-mat3x3 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-mat3x2) v-mat3x2 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-mat2x4) v-mat2x4 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-mat2x3) v-mat2x3 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-mat2x2) v-mat2x2 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-mat4) v-mat4 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-mat3) v-mat3 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-mat2) v-mat2 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-dmat4x4) v-dmat4x4 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-dmat4x3) v-dmat4x3 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-dmat4x2) v-dmat4x2 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-dmat3x4) v-dmat3x4 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-dmat3x3) v-dmat3x3 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-dmat3x2) v-dmat3x2 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-dmat2x4) v-dmat2x4 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-dmat2x3) v-dmat2x3 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-dmat2x2) v-dmat2x2 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-dmat4) v-dmat4 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-dmat3) v-dmat3 :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - 1)" (v-dmat2) v-dmat3 :pure t)

;; not really accurate as loses side effects of prototype
;; we use the compiler macro to patch this up
(v-def-glsl-template-fun float (x p) "float(~a)" (v-real v-float) v-float :pure t)
(v-def-glsl-template-fun float (x p) "double(~a)" (v-real v-double) v-double :pure t)

(v-define-compiler-macro float ((x v-real) (p v-float))
  (if (numberp p)
      `(float ,x)
      `(progn ,p (float ,x))))

(v-define-compiler-macro float ((x v-real) (p v-double))
  (if (numberp p)
      `(double ,x)
      `(progn ,p (double ,x))))


;; Could use v-integer for both of these when we add that

(v-def-glsl-template-fun integer-length (x) "32" (v-int) v-int :pure t)
(v-define-compiler-macro integer-length ((x v-int))
  (if (numberp x)
      32
      `(progn ,x 32)))

(v-def-glsl-template-fun integer-length (x) "32" (v-uint) v-int :pure t)
(v-define-compiler-macro integer-length ((x v-uint))
  (if (numberp x)
      32
      `(progn ,x 32)))


;; TODO: Incorrect, signum type should be based on argument
(v-def-glsl-template-fun signum (a) "sign(~a)" (v-double) v-double :pure t)
(v-def-glsl-template-fun signum (a) "sign(~a)" (v-short-float) v-short-float :pure t)
(v-def-glsl-template-fun signum (a) "sign(~a)" (v-float) v-float :pure t)
(v-def-glsl-template-fun signum (a) "sign(~a)" (:int) :int :pure t)
(v-def-glsl-template-fun signum (a) "sign(~a)" (:uint) :uint :pure t)
(v-def-glsl-template-fun signum (a) "sign(~a)" (v-rational) v-rational :pure t)
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
    `(locally
         (let ((,gcount ,count))
         (for (,var 0) (< ,var ,gcount) (++ ,var)
              ,@body)))))

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

(v-def-glsl-template-fun isqrt (x) "floor(sqrt(~a))" (v-uint)
                         v-uint :pure t)

;; natural log for 1 arg already defined in glsl
(v-def-glsl-template-fun log (x b) "(log2(~a) / log2(~a))"
                         (v-float v-float) v-float :pure t)

;;------------------------------------------------------------
;; Limited Complement

(v-def-glsl-template-fun complement (x) "|complement|(~a)" (v-function-type)
                         v-function-type :pure t)
(v-define-compiler-macro complement
    (&environment env (func v-function-type))
  (let ((type (varjo:argument-type 'func env)))
    (if (typep type 'v-any-one-of)
        (complement-v-any-one-of)
        (complement-single-func func type))))

(defun complement-single-func (func type)
  (let* ((arg-spec (v-argument-spec type))
         (ret-spec (v-return-spec type))
         (names (loop :for i :below (length arg-spec) :collect (gensym)))
         (primary-bool-p (v-typep (elt ret-spec 0) 'v-bool))
         (new-args (map 'list (lambda (n s) (list n (type->type-spec s)))
                        names arg-spec))
         (gfunc (gensym "func")))
    `(let ((,gfunc ,func))
       ,(if primary-bool-p
            `(lambda ,new-args
               (not (funcall ,gfunc ,@names)))
            `(lambda ,new-args
               (funcall ,gfunc ,@names)
               nil)))))

(defun complement-v-any-one-of ()
  (error "Varjo: Cannot yet make the complement in cases where there are many possible overloads.
Try qualifying the types in order to pass complement a specific overload."))

(v-defun scale-float ((f v-float)
                      (i v-int))
  (* f (expt 2f0 i)))

;;------------------------------------------------------------

(v-def-glsl-template-fun float-sign (x) "sign(~a)" (v-float) v-float
                         :pure t)

;;------------------------------------------------------------

(v-def-glsl-template-fun float-radix (x) "£-float-radix-£(~a)"
                         (v-float) v-float :pure t)

(v-define-compiler-macro float-radix ((a v-float))
  (if (floatp a)
      2
      `(progn ,a 2)))

(v-def-glsl-template-fun float-digits (x) "£-float-digits-£(~a)"
                         (v-float) v-float :pure t)

(v-define-compiler-macro float-digits ((a v-float))
  (if (floatp a)
      24
      `(progn ,a 24)))

;;------------------------------------------------------------

(v-def-glsl-template-fun length (arr) "~a.length()" (v-array) :int :pure t)

;;------------------------------------------------------------

(v-defmacro do (var-list end-list &body body)
  `(locally
       (let ,(loop :for (var val) :in var-list :collect
                (list var val))
         (while (not ,(first end-list))
           ,@body
           ,@(loop :for (var nil update) :in var-list :collect
                `(setq ,var ,update)))
         ,@(rest end-list))))

;;------------------------------------------------------------

;; ## Spec clash issues .. Actually the f* variants are the answer!
;;                         we will use the glsl version by default and
;;                         let the user turn to f* when they want the cl
;;                         behaviour! yay!
;; floor (check api)
;; fceiling
;; ffloor
;; fround
;; ftruncate
;; rem
;; round
;; truncate
;; ceiling (check api)

;; ## Harder
;; constantp (issue is that running constantp on a non-const form could have compile time side-effects)
;; random
;; make-random-state (could be a seed for some rand func. however cl's random hard on gpu)
;; do
;; do*
;; setf expanders
;; rotatef
;; shiftf
;; decode-float
;; float-precision
;; integer-decode-float


;; ## Crazy Town
;; every
;; notany
;; notevery
;; some
;; constantly
;; copy-seq
;; make-sequence
;; subseq
;; concatenate
;; elt
;; map
;; reduce
;; reverse
;; length
;; search
;; mismatch
;; find
;; find-if
;; find-if-not
;; count
;; count-if
;; count-if-not
;; position
;; position-if
;; position-if-not
;; remove
;; remove-duplicates
;; remove-if
;; remove-if-not
;; substitute
;; substitute-if
;; substitute-if-not
;; delete
;; delete-duplicates
;; delete-if
;; delete-if-not
;; nsubstitute
;; nsubstitute-if
;; nsubstitute-if-not
;; nreverse
;; replace
;; stable-sort
;; sort
;; map-into
;; merge
;; fill

;; ## Wont Implement
;; rational (uses gcd)
;; rationalize (uses gcd)

;;------------------------------------------------------------

;; a 'do' form can be explained as
;;
;; (block nil
;;   (let ((var1 init1)
;;         (var2 init2)
;;         ...
;;         (varn initn))
;;     declarations
;;     (loop
;;        (when end-test (return (progn . result)))
;;        (tagbody . tagbody)
;;        (psetq var1 step1
;;               var2 step2
;;               ...
;;               varn stepn))))

#||
var result;
var a = init-a();
var b = init-b();
...
while(true)
{
    if (endtest-0())
    {
        result = calc-result-0();
        break;
    }
    if (endtest-1())
    {
        result = calc-result-1();
        break;
    }
    ...
    a = step-a();
    b = step-b();
    ...
}
||#

;;------------------------------------------------------------
