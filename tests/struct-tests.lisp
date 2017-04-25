(in-package :varjo.tests)
(5am:in-suite struct-tests)

;;------------------------------------------------------------
;; Helper data

(v-defstruct pos-rot ()
  (pos :vec3)
  (rot :vec4))

(v-defstruct pos-col ()
  (position :vec3 :accessor pos)
  (color :vec4 :accessor col))

;;------------------------------------------------------------
;; Tests

(5am:def-test structs-0 (:suite struct-tests)
  (finishes-p
   (compile-vert ((vert pos-col)) :450 nil
     (values (v! (pos vert) 1.0)
             (col vert)))))

(5am:def-test structs-1 (:suite struct-tests)
  (finishes-p
   (compile-vert-frag () :450 nil
     (((vert pos-rot))
      (values (v! 0 0 0 0)
              (v! 1 1)
              (pos-rot-pos vert)))
     (((tc :vec2) (veec :vec3))
      (v! tc 0 0)))))

(5am:def-test structs-2 (:suite struct-tests)
  (finishes-p
   (compile-vert-frag ((thing pos-rot)) :450 nil
     (()
      (values (v! 0 0 0 0)
              (v! 1 1)
              (pos-rot-pos thing)))
     (((tc :vec2) (veec :vec3))
      (pos-rot-pos thing)
      (v! tc 0 0)))))

(5am:def-test structs-3 (:suite struct-tests)
  (glsl-doesnt-contain-p "in POS_COL VERT;"
   (compile-vert ((vert pos-col)) :450 nil
     (values (v! (pos vert) 1.0)
             (col vert)))))
