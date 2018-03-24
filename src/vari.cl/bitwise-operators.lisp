(in-package :vari.cl)

(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-uint v-uint) v-uint :pure t)
(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-int v-int) v-int :pure t)
(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-uvec2 v-uvec2) v-uvec2 :pure t)
(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-uvec3 v-uvec3) v-uvec3 :pure t)
(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-uvec4 v-uvec4) v-uvec4 :pure t)
(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-ivec2 v-ivec2) v-ivec2 :pure t)
(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-ivec3 v-ivec3) v-ivec3 :pure t)
(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-ivec4 v-ivec4) v-ivec4 :pure t)
(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-uint v-uvec2) v-uvec2 :pure t)
(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-uint v-uvec3) v-uvec3 :pure t)
(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-uint v-uvec4) v-uvec4 :pure t)
(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-int v-ivec2) v-ivec2 :pure t)
(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-int v-ivec3) v-ivec3 :pure t)
(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-int v-ivec4) v-ivec4 :pure t)
(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-uvec2 v-uint) v-uvec2 :pure t)
(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-uvec3 v-uint) v-uvec3 :pure t)
(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-uvec4 v-uint) v-uvec4 :pure t)
(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-ivec2 v-int) v-ivec2 :pure t)
(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-ivec3 v-int) v-ivec3 :pure t)
(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-ivec4 v-int) v-ivec4 :pure t)

(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-uint v-uint) v-uint :pure t)
(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-int v-int) v-int :pure t)
(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-uvec2 v-uvec2) v-uvec2 :pure t)
(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-uvec3 v-uvec3) v-uvec3 :pure t)
(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-uvec4 v-uvec4) v-uvec4 :pure t)
(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-ivec2 v-ivec2) v-ivec2 :pure t)
(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-ivec3 v-ivec3) v-ivec3 :pure t)
(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-ivec4 v-ivec4) v-ivec4 :pure t)
(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-uint v-uvec2) v-uvec2 :pure t)
(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-uint v-uvec3) v-uvec3 :pure t)
(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-uint v-uvec4) v-uvec4 :pure t)
(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-int v-ivec2) v-ivec2 :pure t)
(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-int v-ivec3) v-ivec3 :pure t)
(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-int v-ivec4) v-ivec4 :pure t)
(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-uvec2 v-uint) v-uvec2 :pure t)
(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-uvec3 v-uint) v-uvec3 :pure t)
(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-uvec4 v-uint) v-uvec4 :pure t)
(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-ivec2 v-int) v-ivec2 :pure t)
(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-ivec3 v-int) v-ivec3 :pure t)
(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-ivec4 v-int) v-ivec4 :pure t)

;;------------------------------------------------------------

(define-vari-macro bit-and (x y)
  (declare (ignore x y))
  (error "Varjo: bit-and was previous incorrectly defined, please use logand"))

(define-vari-macro bit-ior (x y)
  (declare (ignore x y))
  (error "Varjo: bit-and was previous incorrectly defined, please use logand"))

(define-vari-macro bit-xor (x y)
  (declare (ignore x y))
  (error "Varjo: bit-and was previous incorrectly defined, please use logand"))

;;------------------------------------------------------------

(v-def-glsl-template-fun logand (a) "-1" (v-integer) 0 :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:int :int) :int :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:int :ivec2) :ivec2 :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:int :ivec3) :ivec3 :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:int :ivec4) :ivec4 :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:ivec2 :int) :ivec2 :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:ivec2 :ivec2) :ivec2 :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:ivec3 :int) :ivec3 :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:ivec3 :ivec3) :ivec3 :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:ivec4 :int) :ivec4 :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:ivec4 :ivec4) :ivec4 :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:uint :uint) :uint :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:uint :uvec2) :uvec2 :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:uint :uvec3) :uvec3 :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:uint :uvec4) :uvec4 :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:uvec2 :uint) :uvec2 :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:uvec2 :uvec2) :uvec2 :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:uvec3 :uint) :uvec3 :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:uvec3 :uvec3) :uvec3 :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:uvec4 :uint) :uvec4 :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:uvec4 :uvec4) :uvec4 :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:int :int) :int :pure t)

(v-def-glsl-template-fun logand (a b c &rest c) "logand(~a, ~a, ~a, ~{, ~a~})"
                         (t t t &rest t) 0 :pure t)

(v-define-compiler-macro logand ((a t) (b t) (c t) &rest (d t))
  `(logand ,a (logand ,b (logand ,c ,@d))))

(v-define-compiler-macro logand ((a v-integer)) `(progn ,a -1))

;;------------------------------------------------------------

(v-def-glsl-template-fun logxor (a) "0" (v-integer) 0 :pure t)
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-int v-int) v-int :pure t)
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-int v-ivec2) v-ivec2 :pure t)
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-int v-ivec3) v-ivec3 :pure t)
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-int v-ivec4) v-ivec4 :pure t)
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-ivec2 v-int) v-ivec2 :pure t)
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-ivec2 v-ivec2) v-ivec2 :pure t)
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-ivec3 v-int) v-ivec3 :pure t)
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-ivec3 v-ivec3) v-ivec3 :pure t)
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-ivec4 v-int) v-ivec4 :pure t)
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-ivec4 v-ivec4) v-ivec4 :pure t)
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-uint v-uint) v-uint :pure t)
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-uint v-uvec2) v-uvec2 :pure t)
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-uint v-uvec3) v-uvec3 :pure t)
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-uint v-uvec4) v-uvec4 :pure t)
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-uvec2 v-uint) v-uvec2 :pure t)
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-uvec2 v-uvec2) v-uvec2 :pure t)
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-uvec3 v-uint) v-uvec3 :pure t)
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-uvec3 v-uvec3) v-uvec3 :pure t)
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-uvec4 v-uint) v-uvec4 :pure t)
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-uvec4 v-uvec4) v-uvec4 :pure t)



(v-def-glsl-template-fun logxor (a b c &rest c) "logxor(~a, ~a, ~a, ~{, ~a~})"
                         (t t t &rest t) 0 :pure t)

(v-define-compiler-macro logxor ((a t) (b t) (c t) &rest (d t))
  `(logand ,a (logand ,b (logand ,c ,@d))))

(v-define-compiler-macro logxor ((a v-integer))
  `(progn ,a 0))

;;------------------------------------------------------------

(v-def-glsl-template-fun logior (a) "0" (v-integer) 0 :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-int v-int) v-int :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-int v-ivec2) v-ivec2 :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-int v-ivec3) v-ivec3 :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-int v-ivec4) v-ivec4 :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-ivec2 v-int) v-ivec2 :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-ivec2 v-ivec2) v-ivec2 :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-ivec3 v-int) v-ivec3 :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-ivec3 v-ivec3) v-ivec3 :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-ivec4 v-int) v-ivec4 :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-ivec4 v-ivec4) v-ivec4 :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-uint v-uint) v-uint :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-uint v-uvec2) v-uvec2 :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-uint v-uvec3) v-uvec3 :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-uint v-uvec4) v-uvec4 :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-uvec2 v-uint) v-uvec2 :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-uvec2 v-uvec2) v-uvec2 :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-uvec3 v-uint) v-uvec3 :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-uvec3 v-uvec3) v-uvec3 :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-uvec4 v-uint) v-uvec4 :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-uvec4 v-uvec4) v-uvec4 :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-uint v-uint) v-uint :pure t)
(v-def-glsl-template-fun logior (a b) "(~a | ~a)" (v-int v-int) v-int :pure t)

(v-def-glsl-template-fun logior (a b c &rest c) "logior(~a, ~a, ~a, ~{, ~a~})"
                         (t t t &rest t) 0 :pure t)
(v-define-compiler-macro logior ((a t) (b t) (c t) &rest (d t))
  `(logior ,a (logior ,b (logior ,c ,@d))))

(v-define-compiler-macro logior ((a v-integer))
  `(progn ,a 0))

;;------------------------------------------------------------

(v-def-glsl-template-fun logeql (a b) "~~(~a ^ ~a)" (v-uint v-uint)
                         v-uint :pure t)
(v-def-glsl-template-fun logeql (a b) "~~(~a ^ ~a)" (v-int v-int)
                         v-int :pure t)
(v-def-glsl-template-fun logeql (a b c &rest c) "logeql(~a, ~a, ~a, ~{, ~a~})"
                         (t t t &rest t) 0 :pure t)
(v-define-compiler-macro logeql ((a t) (b t) (c t) &rest (d t))
  `(lognot ,a (logxor ,b ,c ,@d)))
(v-def-glsl-template-fun logeql (a) "-1" (v-integer) 0 :pure t)
(v-define-compiler-macro logeql ((a v-integer))
  `(progn ,a -1))

;;------------------------------------------------------------

(v-def-glsl-template-fun lognot (a) "(~~~a)" (v-integer) 0 :pure t)

;;------------------------------------------------------------

(v-defun lognor ((a v-int) (b v-int)) (lognot (logor a b)))
(v-defun lognor ((a v-uint) (b v-uint)) (lognot (logor a b)))

;;------------------------------------------------------------

(v-defun lognand ((a v-int) (b v-int)) (lognot (logand a b)))
(v-defun lognand ((a v-uint) (b v-uint)) (lognot (logand a b)))

;;------------------------------------------------------------

(v-defun logorc1 ((a v-int) (b v-int)) (logor (lognot a) b))
(v-defun logorc1 ((a v-uint) (b v-uint)) (logor (lognot a) b))
(v-defun logorc2 ((a v-int) (b v-int)) (logor a (lognot b)))
(v-defun logorc2 ((a v-uint) (b v-uint)) (logor a (lognot b)))

;;------------------------------------------------------------

(v-defun logandc1 ((a v-int) (b v-int)) (logand (lognot a) b))
(v-defun logandc1 ((a v-uint) (b v-uint)) (logand (lognot a) b))
(v-defun logandc2 ((a v-int) (b v-int)) (logand a (lognot b)))
(v-defun logandc2 ((a v-uint) (b v-uint)) (logand a (lognot b)))

;;------------------------------------------------------------

(v-def-glsl-template-fun logcount (x) "bitCount(~a)" (v-integer) 0 :pure t)

;;------------------------------------------------------------

(v-defun logtest ((a v-int) (b v-int))
  (not (zerop (logand x y))))

(v-defun logtest ((a v-uint) (b v-uint))
  (not (zerop (logand x y))))

;;------------------------------------------------------------
