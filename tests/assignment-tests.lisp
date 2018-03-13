(in-package :varjo.tests)
(5am:in-suite assignment-tests)

;;-----------------

(v-defstruct bah ()
  (data (:int 100)))

;;-----------------

(5am:def-test assign-0 (:suite assignment-tests)
  (finishes-p
   (compile-vert ((a :int)) :410 nil
     (let ((x 10))
       (setf x 20)
       (v! 0 0 0 0)))))

(5am:def-test assign-1 (:suite assignment-tests)
  (finishes-p
   (compile-vert ((a :int)) :410 nil
     (let ((x 10)
           (y 10))
       (setf x y
             y 10)
       (v! 0 0 0 0)))))

(5am:def-test assign-2 (:suite assignment-tests)
  (finishes-p
   (compile-vert ((a :int)) :410 nil
     (let ((x 10))
       (incf x 1)
       (v! 0 0 0 0)))))

(5am:def-test assign-3 (:suite assignment-tests)
  (finishes-p
   (compile-compute (&uniform (woop bah :ssbo)) :450 nil
     (declare (local-size :x 1 :y 1 :z 1))
     (setf (aref (bah-data woop) (int (x gl-work-group-id)))
           999)
     (values))))

(5am:def-test assign-4 (:suite assignment-tests)
  (signals varjo-conditions:assigning-to-readonly
    (compile-compute (&uniform (woop bah :ssbo)) :450 nil
      (declare (local-size :x 1 :y 1 :z 1))
      (setf (aref (bah-data woop) (int (x gl-work-group-id)))
            999)
      (setf woop woop)
      (values))))

;; (5am:def-test assign-3 (:suite assignment-tests)
;;   (finishes-p
;;    (compile-vert ((a :int)) :410 nil
;;      (let ((x 10))
;;        (decf x 1)
;;        (v! 0 0 0 0)))))
