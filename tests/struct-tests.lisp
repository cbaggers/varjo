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

(v-defstruct pos-sam ()
  (pos :vec3)
  (sam :sampler-2d))

(v-defun useless-unpack-0 ((pc pos-col))
  (let ((v (pos pc)))
    (+ (x v) (y v) (z v))))

;;------------------------------------------------------------
;; Tests

(5am:def-test structs-0 (:suite struct-tests)
  (finishes-p
   (compile-vert ((vert pos-col)) :410 nil
     (values (v! (pos vert) 1.0)
             (col vert)))))

(5am:def-test structs-1 (:suite struct-tests)
  (finishes-p
   (compile-vert-frag () :410 nil
     (((vert pos-rot))
      (values (v! 0 0 0 0)
              (v! 1 1)
              (pos-rot-pos vert)))
     (((tc :vec2) (veec :vec3))
      (v! tc 0 0)))))

(5am:def-test structs-2 (:suite struct-tests)
  (finishes-p
   (compile-vert-frag ((thing pos-rot)) :410 nil
     (()
      (values (v! 0 0 0 0)
              (v! 1 1)
              (pos-rot-pos thing)))
     (((tc :vec2) (veec :vec3))
      (pos-rot-pos thing)
      (v! tc 0 0)))))

(5am:def-test structs-3 (:suite struct-tests)
  (glsl-doesnt-contain-p "in POS_COL VERT;"
   (compile-vert ((vert pos-col)) :410 nil
     (values (v! (pos vert) 1.0)
             (col vert)))))

(5am:def-test structs-4 (:suite struct-tests)
  (finishes-p
    (compile-vert (&uniform (vert pos-sam)) :410 nil
      (v! 1 2 3 4))))

(5am:def-test structs-5 (:suite struct-tests)
  (signals varjo-conditions:opaque-data-found
    (compile-vert ((vert pos-sam)) :410 nil
      (v! 1 2 3 4))))

(5am:def-test structs-6 (:suite struct-tests)
  (signals varjo-conditions:opaque-data-found
    (compile-vert (&uniform (vert pos-sam :ubo)) :410 nil
      (v! 1 2 3 4))))

(5am:def-test structs-7 (:suite struct-tests)
  (signals varjo-conditions:opaque-data-found
    (compile-vert (&uniform (vert pos-sam :ssbo)) :410 nil
      (v! 1 2 3 4))))

(5am:def-test structs-8 (:suite struct-tests)
  (finishes-p
   (compile-vert (&uniform (data pos-col)) :410 nil
     (useless-unpack-0 data)
     (vec4 0))))

(5am:def-test structs-9 (:suite struct-tests)
  (finishes-p
   (compile-vert (&uniform (data pos-col :ssbo)) :430 nil
     (useless-unpack-0 data)
     (vec4 0))))

(5am:def-test structs-10 (:suite struct-tests)
  (finishes-p
   (compile-vert (&uniform (data pos-col :ubo)) :430 nil
     (useless-unpack-0 data)
     (vec4 0))))

(5am:def-test structs-11 (:suite struct-tests)
  (finishes-p
   (compile-vert ((vert pos-col)) :430 nil
     (useless-unpack-0 vert)
     (vec4 0))))
