(in-package :vari.cl)

;; We dont support rational complex.
;; Doing that correctly would be slow as the spec requires that any
;; (complex <some-rational> 0) is just <some-rational>, by which I mean
;; you can never have a zerop rational imagpart.

;;----------------------------------------------------------------------
;; constructors

(v-def-glsl-template-fun complex (a b) "vec2(~a, ~a)" (v-float v-float)
                         v-single-complex :pure t)

(v-def-glsl-template-fun complex (a) "vec2(~a, 0.0f)" (v-float)
                         v-single-complex :pure t)

(v-def-glsl-template-fun complex (a b) "vec2(~a, ~a)" (v-double v-double)
                         v-double-complex :pure t)

(v-def-glsl-template-fun complex (a) "vec2(~a, 0.0LF)" (v-double)
                         v-double-complex :pure t)


;;----------------------------------------------------------------------
;; components

(v-def-glsl-template-fun realpart (a) "~a.x" (v-single-complex)
                         v-float :pure t)

(v-def-glsl-template-fun imagpart (a) "~a.y" (v-single-complex)
                         v-float :pure t)

(v-def-glsl-template-fun realpart (a) "~a.x" (v-double-complex)
                         v-double :pure t)

(v-def-glsl-template-fun imagpart (a) "~a.y" (v-double-complex)
                         v-double :pure t)

;;----------------------------------------------------------------------
;; conjugate

(v-def-glsl-template-fun conjugate (x) "~a" (v-real) 0 :pure t)

(v-defun conjugate ((a v-single-complex))
  (complex (realpart a) (- (imagpart a))))

(v-defun conjugate ((a v-double-complex))
  (complex (realpart a) (- (imagpart a))))

;;----------------------------------------------------------------------
;; cis

(v-defun cis ((a v-float))
  (complex (cos a) (sin a)))

(v-defun cis ((a v-double))
  (complex (cos a) (sin a)))

;;----------------------------------------------------------------------
;; equality

(v-def-glsl-template-fun
 = (a b) "(~a == ~a)" (v-single-complex v-single-complex) v-bool :pure t)
(v-def-glsl-template-fun
 = (a b) "(~a == ~a)" (v-double-complex v-double-complex) v-bool :pure t)


(v-def-glsl-template-fun
 eql (a b) "(~a == ~a)" (v-single-complex v-single-complex) v-bool)
(v-def-glsl-template-fun
 eql (a b) "(~a == ~a)" (v-double-complex v-double-complex) v-bool)


(v-def-glsl-template-fun
 equal (a b) "(~a == ~a)" (v-single-complex v-single-complex) v-bool)
(v-def-glsl-template-fun
 equal (a b) "(~a == ~a)" (v-double-complex v-double-complex) v-bool)


(v-def-glsl-template-fun
 /= (a b) "(~a != ~a)" (v-single-complex v-single-complex) v-bool :pure t)
(v-def-glsl-template-fun
 /= (a b) "(~a != ~a)" (v-double-complex v-double-complex) v-bool :pure t)


(v-def-glsl-template-fun
 = (a b) "(~a == vec2(~a, 0.0f))" (v-single-complex v-float) v-bool :pure t)
(v-def-glsl-template-fun
 = (a b) "(~a == dvec2(~a, 0.0LF))" (v-double-complex v-double) v-bool :pure t)
(v-def-glsl-template-fun
 = (a b) "(vec2(~a, 0.0f) == ~a)" (v-float v-single-complex) v-bool :pure t)
(v-def-glsl-template-fun
 = (a b) "(dvec2(~a, 0.0LF) == ~a)" (v-double v-double-complex) v-bool :pure t)

;; (v-defun = ((a v-single-complex) (b v-float))
;;   (and (= (imagpart a) 0f0)
;;        (= (realpart a) b)))
;; (v-defun = ((a v-float) (b v-single-complex))
;;   (and (= (imagpart b) 0f0)
;;        (= (realpart b) a)))
;; (v-defun = ((a v-double-complex) (b v-double))
;;   (and (= (imagpart a) 0f0)
;;        (= (realpart a) b)))
;; (v-defun = ((a v-double) (b v-double-complex))
;;   (and (= (imagpart b) 0f0)
;;        (= (realpart b) a)))

(v-def-glsl-template-fun
 /= (a b) "(~a != vec2(~a, 0.0f))" (v-single-complex v-float) v-bool :pure t)
(v-def-glsl-template-fun
 /= (a b) "(~a != dvec2(~a, 0.0LF))" (v-double-complex v-double) v-bool :pure t)
(v-def-glsl-template-fun
 /= (a b) "(vec2(~a, 0.0f) != ~a)" (v-float v-single-complex) v-bool :pure t)
(v-def-glsl-template-fun
 /= (a b) "(dvec2(~a, 0.0LF) != ~a)" (v-double v-double-complex) v-bool :pure t)

;; (v-defun /= ((a v-single-complex) (b v-float))
;;   (not (= a b)))
;; (v-defun /= ((a v-float) (b v-single-complex))
;;   (not (= a b)))
;; (v-defun /= ((a v-double-complex) (b v-double))
;;   (not (= a b)))
;; (v-defun /= ((a v-double) (b v-double-complex))
;;   (not (= a b)))

;;----------------------------------------------------------------------
;; addition

(v-def-glsl-template-fun + (a b) "(~a + ~a)" (v-single-complex v-single-complex)
                         v-single-complex :pure t)
(v-def-glsl-template-fun + (a b) "(~a + ~a)" (v-double-complex v-double-complex)
                         v-double-complex :pure t)

(v-def-glsl-template-fun + (a b) "(~a + vec2(~a, 0.0f))"
                         (v-single-complex v-float) v-single-complex :pure t)
(v-def-glsl-template-fun + (a b) "(~a + dvec2(~a, 0.0LF))"
                         (v-double-complex v-double) v-double-complex :pure t)

(v-def-glsl-template-fun + (a b) "(vec2(~a, 0.0f) + ~a)"
                         (v-float v-single-complex) v-single-complex :pure t)
(v-def-glsl-template-fun + (a b) "(dvec2(~a, 0.0LF) + ~a)"
                         (v-double v-double-complex) v-double-complex :pure t)

;;----------------------------------------------------------------------
;; subtract

(v-def-glsl-template-fun - (a b) "(~a - ~a)" (v-single-complex v-single-complex)
                         v-single-complex :pure t)
(v-def-glsl-template-fun - (a b) "(~a - ~a)" (v-double-complex v-double-complex)
                         v-double-complex :pure t)

(v-def-glsl-template-fun - (a b) "(~a - vec2(~a, 0.0f))"
                         (v-single-complex v-float) v-single-complex :pure t)
(v-def-glsl-template-fun - (a b) "(~a - dvec2(~a, 0.0LF))"
                         (v-double-complex v-double) v-double-complex :pure t)

(v-def-glsl-template-fun - (a b) "(vec2(~a, 0.0f) - ~a)"
                         (v-float v-single-complex) v-single-complex :pure t)
(v-def-glsl-template-fun - (a b) "(dvec2(~a, 0.0LF) - ~a)"
                         (v-double v-double-complex) v-double-complex :pure t)

;;----------------------------------------------------------------------
;; negate

(v-def-glsl-template-fun - (a) "(-~a)" (v-single-complex) v-single-complex :pure t)
(v-def-glsl-template-fun - (a) "(-~a)" (v-double-complex) v-double-complex :pure t)

(v-def-glsl-template-fun negate (a) "(-~a)" (v-single-complex) v-single-complex
                         :pure t)
(v-def-glsl-template-fun negate (a) "(-~a)" (v-double-complex) v-double-complex
                         :pure t)


;;----------------------------------------------------------------------
;; zero predicate

(v-def-glsl-template-fun zerop (a) "(~a == 0)" (v-single-complex) v-bool)
(v-def-glsl-template-fun zerop (a) "(~a == 0)" (v-double-complex) v-bool)


;;----------------------------------------------------------------------
;; multiplication

(v-defun * ((a v-single-complex) (b v-single-complex))
  (let* ((rx (realpart a))
         (ix (imagpart a))
         (ry (realpart b))
         (iy (imagpart b)))
    (complex (- (* rx ry) (* ix iy))
             (+ (* rx iy) (* ix ry)))))

(v-defun * ((a v-double-complex) (b v-double-complex))
  (let* ((rx (realpart a))
         (ix (imagpart a))
         (ry (realpart b))
         (iy (imagpart b)))
    (complex (- (* rx ry) (* ix iy))
             (+ (* rx iy) (* ix ry)))))

(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-single-complex v-float)
                         v-single-complex)
(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-double-complex v-double)
                         v-double-complex)

(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-float v-single-complex)
                         v-single-complex)
(v-def-glsl-template-fun * (a b) "(~a * ~a)" (v-double v-double-complex)
                         v-double-complex)

;;----------------------------------------------------------------------
;; division

(v-defun / ((a v-single-complex) (b v-single-complex))
  (let* ((rx (realpart a))
         (ix (imagpart a))
         (ry (realpart b))
         (iy (imagpart b)))
    (if (> (abs ry) (abs iy))
        (let* ((r (/ iy ry))
               (dn (+ ry (* r iy))))
          (complex (/ (+ rx (* ix r)) dn)
                   (/ (- ix (* rx r)) dn)))
        (let* ((r (/ ry iy))
               (dn (+ iy (* r ry))))
          (complex (/ (+ (* rx r) ix) dn)
                   (/ (- (* ix r) rx) dn))))))

(v-defun / ((a v-double-complex) (b v-double-complex))
  (let* ((rx (realpart a))
         (ix (imagpart a))
         (ry (realpart b))
         (iy (imagpart b)))
    (if (> (abs ry) (abs iy))
        (let* ((r (/ iy ry))
               (dn (+ ry (* r iy))))
          (complex (/ (+ rx (* ix r)) dn)
                   (/ (- ix (* rx r)) dn)))
        (let* ((r (/ ry iy))
               (dn (+ iy (* r ry))))
          (complex (/ (+ (* rx r) ix) dn)
                   (/ (- (* ix r) rx) dn))))))

(v-defun / ((a v-float) (b v-single-complex))
  (let* ((ry (realpart b))
         (iy (imagpart b)))
    (if (> (abs ry) (abs iy))
        (let* ((r (/ iy ry))
               (dn (* ry (+ 1 (* r r)))))
          (complex (/ a dn)
                   (/ (- (* a r)) dn)))
        (let* ((r (/ ry iy))
               (dn (* iy (+ 1 (* r r)))))
          (complex (/ (* a r) dn)
                   (/ (- a) dn))))))

(v-defun / ((a v-double) (b v-double-complex))
  (let* ((ry (realpart b))
         (iy (imagpart b)))
    (if (> (abs ry) (abs iy))
        (let* ((r (/ iy ry))
               (dn (* ry (+ 1 (* r r)))))
          (complex (/ a dn)
                   (/ (- (* a r)) dn)))
        (let* ((r (/ ry iy))
               (dn (* iy (+ 1 (* r r)))))
          (complex (/ (* a r) dn)
                   (/ (- a) dn))))))

(v-def-glsl-template-fun / (a b) "(~a / ~a)" (v-single-complex v-float)
                         v-single-complex)
(v-def-glsl-template-fun / (a b) "(~a / ~a)" (v-double-complex v-double)
                         v-double-complex)


;;----------------------------------------------------------------------
;; by one

(v-def-glsl-template-fun 1+ (a) "(~a + vec2(1.0f, 0.0f))" (v-single-complex)
                         v-single-complex :pure t)
(v-def-glsl-template-fun 1+ (a) "(~a + dvec2(1.0f, 0.0f))" (v-double-complex)
                         v-double-complex :pure t)

(v-def-glsl-template-fun 1- (a) "(~a - vec2(1.0f, 0.0f))" (v-single-complex)
                         v-single-complex :pure t)
(v-def-glsl-template-fun 1- (a) "(~a - dvec2(1.0f, 0.0f))" (v-double-complex)
                         v-double-complex :pure t)

;;----------------------------------------------------------------------
;; phase

(v-defun phase ((n v-float))
  (atan 0 (realpart n)))

(v-defun phase ((n v-double))
  (atan 0 (realpart n)))

(v-defun phase ((n complex))
  (atan (imagpart n) (realpart n)))


;;----------------------------------------------------------------------
;; abs

(v-def-glsl-template-fun abs (a) "length(~a)" (v-single-complex) v-float
                         :pure t)
(v-def-glsl-template-fun abs (a) "length(~a)" (v-double-complex) v-double
                         :pure t)

;;----------------------------------------------------------------------

(v-defun log ((number v-single-complex) (base v-float))
  (if (zerop base)
      0f0
      (/ (log number) (log base))))

(v-defun log ((number v-double-complex) (base v-double))
  (if (zerop base)
      0d0
      (/ (log number) (log base))))

(v-defun log ((number v-single-complex))
  (complex (log (abs number))
           (phase number)))

(v-defun log ((number v-double-complex))
  (complex (log (abs number))
           (phase number)))

;;----------------------------------------------------------------------

(v-defun exp ((a v-single-complex))
  (* (exp (realpart a))
     (cis (imagpart a))))

;; glsl doesnt have exp for double
;; (v-defun exp ((a v-double-complex))
;;   (* (exp (realpart a))
;;      (cis (imagpart a))))

;;----------------------------------------------------------------------

(v-defun expt ((base v-single-complex) (power v-single-complex))
  (if (and (zerop base) (plusp (realpart power)))
      (* base power)
      (exp (* power (log base)))))

(v-defun expt ((base v-float) (power v-single-complex))
  (if (and (zerop base) (plusp (realpart power)))
      (* base power)
      (exp (* power (log base)))))

(v-defun expt ((base v-single-complex) (power v-float))
  (if (and (zerop base) (plusp (realpart power)))
      (* base power)
      (exp (* power (log base)))))

;; no expt for double-complex as we dont have exp for doubles in glsl

;;----------------------------------------------------------------------

(v-defun sqrt ((z v-single-complex))
  (let* ((rp (realpart z))
         (ip (imagpart z))
         (rho0 (+ (* rp rp) (* ip ip)))
         (rho1 (+ (abs rp) (sqrt rho0)))
         (rho2 (scale-float (sqrt (+ rho1 rho1)) -1)))
    (if (/= rho2 0f0)
        (let ((nu (/ (/ ip rho2) 2f0)))
          (if (< rp 0f0)
              (complex (abs nu) (float-sign ip rho2))
              (complex rho2 nu)))
        (complex rho2 ip))))

(v-defun sqrt ((z v-double-complex))
  (let* ((rp (realpart z))
         (ip (imagpart z))
         (rho0 (+ (* rp rp) (* ip ip)))
         (rho1 (+ (abs rp) (sqrt rho0)))
         (rho2 (scale-float (sqrt (+ rho1 rho1)) -1)))
    (if (/= rho2 0d0)
        (let ((nu (/ (/ ip rho2) 2d0)))
          (if (< rp 0d0)
              (complex (abs nu) (float-sign ip rho2))
              (complex rho2 nu)))
        (complex rho2 ip))))

;;----------------------------------------------------------------------

(v-defun sin ((number v-single-complex))
  (let ((x (realpart number))
        (y (imagpart number)))
    (complex (* (sin x) (cosh y))
             (* (cos x) (sinh y)))))

(v-defun cos ((number v-single-complex))
  (let ((x (realpart number))
        (y (imagpart number)))
    (complex (* (cos x) (cosh y))
             (- (* (sin x) (sinh y))))))

(v-defun tan ((number v-single-complex))
  ;; tan z = -i * tanh(i*z)
  (* (complex 0 1)
     (tanh (* (complex 0 -1) number))))

;;----------------------------------------------------------------------

(v-defun sinh ((number v-single-complex))
  (let ((x (realpart number))
        (y (imagpart number)))
    (complex (* (sinh x) (cos y))
             (* (cosh x) (sin y)))))

(v-defun cosh ((number v-single-complex))
  (let ((x (realpart number))
        (y (imagpart number)))
    (complex (* (cosh x) (cos y))
             (* (sinh x) (sin y)))))

(v-defun tanh ((number v-single-complex))
  (/ (sinh number) (cosh number)))

;;----------------------------------------------------------------------

(v-defun asinh ((number v-single-complex))
  (let* ((in (complex (- (imagpart number)) (realpart number)))
         (result (asin in)))
    (complex (imagpart result)
             (- (realpart result)))))

(v-defun acosh ((number v-single-complex))
  (let ((sqrt-1 (sqrt (- number 1)))
        (sqrt+1 (sqrt (+ number 1))))
    (complex (asinh (realpart (* (conjugate sqrt-1)
                                 sqrt+1)))
             (* 2 (atan (/ (imagpart sqrt-1)
                           (realpart sqrt+1)))))))

(v-defun atanh ((number v-single-complex))
  (/ (- (log (+ 1f0 number)
             (- 1f0 number)))
     2f0))

;;----------------------------------------------------------------------

(v-defun acos ((number v-single-complex))
  (- #.(float (/ pi 2) 0f0)
     (asin number)))

(v-defun asin ((number v-single-complex))
  (let ((sqrt-1-n (sqrt (- 1 number)))
        (sqrt-1+n (sqrt (+ 1 number))))
    (complex (atan (/ (realpart number)
                      (realpart (* sqrt-1-n sqrt-1+n))))
             (asinh (imagpart (* (conjugate sqrt-1-n)
                                 sqrt-1+n))))))

(v-defun atan ((number v-single-complex))
  (let* ((iz (complex (- (imagpart number)) (realpart number)))
         (result (atanh iz)))
    (complex (imagpart result)
             (- (realpart result)))))



;; sbcl bug?
;;
;; (let ((a (complex 1 2)))
;;   (list (sin a)
;;         (sin 1)
;;         (sin 2)))

;;----------------------------------------------------------------------
