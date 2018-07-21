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

(v-defun useless-unpack-1 ((pc pos-col))
  (with-slots (position) pc
    (+ (x position)
       (y position)
       (z position))))

(define-vari-struct plight nil
  (pos :vec3 :accessor plight-pos)
  (color :vec3 :accessor plight-color)
  (strength :float :accessor plight-strength))

(define-vari-struct light-set nil
  (plights (plight (3)) :accessor light-set-plights))

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

(5am:def-test structs-12 (:suite struct-tests)
  (finishes-p
   (compile-vert (&uniform (data pos-col :ssbo)) :430 nil
     (with-slots (color) data
       color))))

(5am:def-test structs-13 (:suite struct-tests)
  (finishes-p
   (compile-vert (&uniform (data pos-col)) :430 nil
     (useless-unpack-1 data)
     (vec4 0))))

(5am:def-test structs-14 (:suite struct-tests)
  (finishes-p
   (compile-vert (&uniform (data pos-col :ubo)) :430 nil
     (useless-unpack-1 data)
     (vec4 0))))

(5am:def-test structs-15 (:suite struct-tests)
  (finishes-p
   (compile-vert () :450 nil
     (flet ((ham ()
              (let ((x (make-pos-rot (vec3 0) (vec4 0))))
                (pos-rot-pos x))))
       (ham)
       (vec4 0)))))

(5am:def-test structs-16 (:suite struct-tests)
  (glsl-contains-n-p 1 "struct PLIGHT"
    (varjo.tests::compile-vert (&uniform (lights light-set :ubo :std-140))
        :330 t
      (vec4 1))))

(5am:def-test structs-17 (:suite struct-tests)
  (glsl-doesnt-contain-p "struct PLIGHT"
    (varjo.tests::compile-vert (&uniform (light plight :ubo :std-140))
        :330 t
      (vec4 1))))

(5am:def-test structs-18 (:suite struct-tests)
  (glsl-contains-1-of-all-p ("struct LIGHT_SET"
                             "struct PLIGHT")
    (varjo.tests::compile-vert (&uniform (lights light-set))
        :330 t
      (vec4 1))))
