
;; Now in glsl these dont make much sense so this will be compile time resolved.
;; If not... well then could we generate an ugly proxy? It would be slow suck,
;; but it would be impressive.

;; Hah ^^ that comment was from 2014 me, this is 2016 me armed with 2016 varjo,
;; we can almost certainly do this at compile time. We have the flow analyzer,
;; this will work


(defun-g test ()
  (labels ((thing ((x :int)) (* 2 x)))
    (let ((foo #'thing))
      (funcall foo 10))))

;; the trick is that we must only ever allow only flow-id at the function
;; position of a call site.
;;
;; So ^^ is fine as foo is only ever thing

(defun-g test (&uniform (jam :int))
  (labels ((thing ((x :int)) (* 2 x))
           (thang ((x :int)) (* 3 x)))
    (let ((foo (if (= jam 0)
                   #'thing
                   #'thang)))
      (funcall foo 10))))

;; This ^^ is not ok as foo will have two flow-id's

(defun-g test (&uniform (jam :int))
  (labels ((thing ((x :int)) (* 2 x))
           (thang ((x :int)) (* 3 x))
           (boom ((f (function (:int) :int)) (i :int))
             (funcall f i)))
    (funcall boom thing 10)
    (funcall boom thang 20)))

;; this one should be fine as we call transform funcall to:

(defun-g test (&uniform (jam :int))
  (labels ((thing ((x :int)) (* 2 x))
           (thang ((x :int)) (* 3 x)))
    (labels ((boom ((f (function (:int) :int)) (i :int))
               (funcall f i)))
      (funcall boom thing 10))
    (labels ((boom ((f (function (:int) :int)) (i :int))
               (funcall f i)))
      (funcall boom thang 20))))

;; and now each instance only has 1 possible func

;; this works recursively so you can get code ballooning but that's not
;; something I'm caring about right now.

;; So back to this ^^. When funcall has 1 flow-id it knows which function
;; is calling it, so it rewrites itself as a regular func call

(defun-g test (&uniform (jam :int))
  (labels ((thing ((x :int)) (* 2 x))
           (thang ((x :int)) (* 3 x)))
    (labels ((boom ((i :int))
               (funcall thing i)))
      (boom 10))
    (labels ((boom ((i :int))
               (funcall thang i)))
      (boom 20))))

(defun-g test (&uniform (jam :int))
  (labels ((thing ((x :int)) (* 2 x))
           (thang ((x :int)) (* 3 x)))
    (labels ((boom ((i :int))
               (labels ((thing ((x :int)) (* 2 x)))
                 (thing i))))
      (boom 10))
    (labels ((boom ((i :int))
               (labels ((thang ((x :int)) (* 2 x)))
                 (thang i))))
      (boom 20))))
