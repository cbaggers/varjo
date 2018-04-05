(in-package :vari.glsl)

;;------------------------------------------------------------

(v-def-glsl-template-fun break () "break" () v-void)
(v-def-glsl-template-fun continue () "continue" () v-void)

;;------------------------------------------------------------

(v-def-glsl-template-fun ++ (a) "(++ ~a)" (v-real) nil)
(v-def-glsl-template-fun -- (a) "(-- ~a)" (v-real) nil)

;;------------------------------------------------------------
;; Devil magic that makes operators work on any length

(v-def-glsl-template-fun = (a b c &rest c) "(~a £==£ ~a £==£ ~a ~{ £==£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(v-define-compiler-macro = ((a t) (b t) (c t) &rest (d t))
  (let ((ga (gensym "a")))
    `(let ((,ga ,a))
       (and (= ,ga ,b)
            (= ,ga ,c)
            ,@(loop :for x :in d :collect
                 `(= ,ga ,x))))))

(v-def-glsl-template-fun * (a b c &rest c) "(~a £*£ ~a £*£ ~a ~{ £*£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(v-define-compiler-macro * ((a t) (b t) (c t) &rest (d t))
  `(* ,a (* ,b (* ,c ,@d))))

(v-def-glsl-template-fun / (a b c &rest c) "(~a £/£ ~a £/£ ~a ~{ £/£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(v-define-compiler-macro / ((a t) (b t) (c t) &rest (d t))
  `(/ ,a (/ ,b (/ ,c ,@d))))

(v-def-glsl-template-fun + (a b c &rest c) "(~a £+£ ~a £+£ ~a ~{ £+£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(v-define-compiler-macro + ((a t) (b t) (c t) &rest (d t))
  `(+ ,a (+ ,b (+ ,c ,@d))))

(v-def-glsl-template-fun - (a b c &rest c) "(~a £-£ ~a £-£ ~a ~{ £-£ ~a~})"
                         (t t t &rest t) 0 :pure t)

(v-define-compiler-macro - ((a t) (b t) (c t) &rest (d t))
  `(- ,a (- ,b (- ,c ,@d))))

;;------------------------------------------------------------

(v-def-glsl-template-fun + () "0" () :int :pure t)
(v-def-glsl-template-fun + (x &rest y) "(~a~{ + ~a~})" (v-number &rest v-number) nil :pure t)

(v-def-glsl-template-fun + (a) "~a" (v-real) 0 :pure t)
(v-def-glsl-template-fun + (a) "~a" (v-vector) 0 :pure t)

(v-def-glsl-template-fun - (a) "(-~a)" (v-real) 0 :pure t)
(v-def-glsl-template-fun - (a) "(-~a)" (v-vec2) 0 :pure t)
(v-def-glsl-template-fun - (a) "(-~a)" (v-vec3) 0 :pure t)
(v-def-glsl-template-fun - (a) "(-~a)" (v-vec4) 0 :pure t)

(v-def-glsl-template-fun * () "1" () :int :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-real) 0 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-vector) 0 :pure t)
(v-def-glsl-template-fun * (a) "~a" (v-matrix) 0 :pure t)

(v-def-glsl-template-fun / (a) "(1.0f / ~a)" (v-real) :float :pure t)
(v-def-glsl-template-fun / (a) "(1 / ~a)" (v-vector) 0 :pure t)

(v-def-glsl-template-fun v-not (x &context (:330 :440)) "not(~a)"
         (v-bvector) 0 :pure t)

;;------------------------------------------------------------
