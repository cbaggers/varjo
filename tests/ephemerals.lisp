(in-package :varjo.tests)
(5am:in-suite ephemeral-tests)

;;------------------------------------------------------------
;; Helper data

(v-defstruct some-vert ()
  (position :vec3 :accessor pos)
  (color :vec4 :accessor col))

;;------------------------------------------------------------

(defun ephem-0 ()
  (compile-vert ((vert some-vert)) :450 nil
    (let ((x vert))
      (v! (pos x) 1))))

(5am:def-test ephemeral-0 (:suite ephemeral-tests)
  (finishes-p
   (ephem-0)))

;;------------------------------------------------------------

(defun report (result)
  (print (glsl-code result))
  result)

;;------------------------------------------------------------

(v-deftype breen () nil)

(varjo:v-def-glsl-template-fun zeem (a) "zeem()" (breen) :int)

(defun ephem-1 ()
  (report
   (compile-vert () :450 nil
     (let ((x (breen)))
       (zeem x)
       (v! 1 2 3 4)))))
