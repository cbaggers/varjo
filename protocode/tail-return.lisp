;; hmm how to do this...
;; so we have a function

(defun blah ((x :vec4) (y :mat4)) ;; called full form
  (let ((n (s~ x :xy)))
    (incf n 10)) ;; called form a
  (let ((res (* x y)))
    x)) ;;called form b

;; ok so let's assume that we dont allow setiing gl-vars any more. This
;; effectively means there are only local side effects...this also means
;; setf in the first clause a is pointless...in fact clause a is pointless

;; right so functions have 1 form only..done :)

;; is there a more general form?..must be.. imagine the following


(defun blah ((x :vec4) (y :mat4))
  (progn
    (let ((n (s~ x :xy)))
      (incf n 10)) ;; called form a
    (let ((res (* x y)))
      x)))

;; again clause-a contributes nothing...so progn is almost worthless?
;; hmmm except when inside a let

(defun blah ((x :vec4) (y :mat4))
  (let ((x 0))
    (progn
      (setf x (s~ x :xy)) ;; called form a
      (let ((res (* x y)))
        res))))

;; here progn is valid .. so the rule is:
;;   'progn is only valid if in a scope with local vars'
;; and really only valid if it modifies one...
;; which actially could have been written as

(defun blah ((x :vec4) (y :mat4))
  (let ((x (s~ x :xy)))
    (let ((res (* x y)))
      res)))

;; and further

(defun blah ((x :vec4) (y :mat4))
  (let* ((x (s~ x :xy)))
    (* x y)))

;; which is better...so really we could remove progn entirely
;; Only problem is that is not very cl though.

;; ok so we won't remove it...but we won't do anything to make it
;; relevent either.


;; let's treat that as settled and move on.
;; Next is values
;; but first we rename out to %out


(defun blah ((x :vec4) (y :mat4))
  (values (let* ((x (s~ x :xy)))
            (* x y))
          (x :smooth)))

;; values is extended from CL to support
;; values behave differently when used in stage or in functions

;; in a stage it becomes setf's and %out's, the first arg is always
;; the default out param for that stage so for vertex is it gl-position
;; for frag, color etc

;; so
(values (let* ((x (s~ x :xy)))
            (* x y))
        (x :smooth))

;;becomes
(progn
  (setf gl-position (let* ((x (s~ x :xy)))
                      (* x y)))
  (%out x :smooth))

;; you can also specify gl-special vars
(values x (y gl-frag-col))
;; becomes
(progn
  (setf gl-position x)
  (setf gl-frag-col y))

;; this is the only place it is valid to set a gl-var

(gmap #'+ s1 s2)
