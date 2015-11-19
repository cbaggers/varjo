;; scratchy

(defmacro while (condition &body body)
  `(progn
     (loop :while ,condition :do (progn ,@body))
     nil))

;; just external side effects

(let ((x 20))
  (while (< x 10)
    (print x)))

;; by external I mean that the program isnt affected by this
;; io that only goes out is not effecting the future of the
;; programs execution

;;-    -    -    -    -    -    -    -    -    -    -    -

;; just internal side effects
(let ((x 20))
  (while (< x 10)
    (let ((z 2))
      (setq z 1))))

;; never reaches outside the loop so no worries

;;-    -    -    -    -    -    -    -    -    -    -    -

;; return

(let ((x 20))
  (while (< x 10)
    (return 10)))

;; this does affect the program. We should disallow return
;; as we use tail return.

;;-    -    -    -    -    -    -    -    -    -    -    -

;; break/continue

(let ((x 20))
  (while (< x 10)
    (break)
    (setq x 20)))

;; these can affect what code gets run, and so in effect what
;; non internal side effects can happen. However the case above
;; is super unlikely. All breaks/continues in practice will be
;; inside a conditional

;;-    -    -    -    -    -    -    -    -    -    -    -

;; setq

;; assignment is the only way for data to flow out of the while
;; block.

(let ((x 0))
  (while (< x 10)
    (setq x (+ x 1))))

;; however there are different ways of doing this, let's look at those

;;-    -    -    -    -    -    -    -    -    -    -    -

;; set a variable

;; The example from above is repeated here

(let ((x 0))
  (while (< x 10)
    (setq x (+ x 1))))

;; Let's consider the difference between ^^^^-this
;; and this-vvvv

(let ((x 0))
  (progn
    (setq x (+ x 1))))

;; So the difference is the top one will happen -n- times
;; But if only the final result matters to the rest of the program,
;; and each loop a new value is generated, then (as far as flow
;; analysis is concerned) it's the same as one loop.

;;-    -    -    -    -    -    -    -    -    -    -    -

;; So the value of a var being assigned from a loop depends on
;; some things. So I'm gonna deduce this stuff here.
;; Take this example:

(let ((x 0)
      (z 1))
  (while (< x 10)
    (setq x ???)))

;; by the end x can either be a value from outside the loop
;; e.g (setq x z) or a value from inside the loop (setq x (+ x 1))
;; values from outside the loop wont change unless the loop sets
;; them (making them a value from inside the loop)

;; so regardless of number of loops we can say if it comes from
;; side then the value is (flow-id!) else it is the flow-ids of
;; the external value.

;; The only things that breaks this are collections

;;-    -    -    -    -    -    -    -    -    -    -    -

;; Collections

;; So collections make this tricky as we don't know what which
;; values are in the collection until runtime.

;; that means any element added should add it's flow-id to the
;; flow-id of the collection.

;; also that any element retrieved from the collection should
;; have the flow-id of the entire collection.

;; This way we ensure we get all possible flow through the code

;;-    -    -    -    -    -    -    -    -    -    -    -

;; Almost!

;; This case is fun..

(let ((x 0)
      (z 1))
  (while (< x 10)
    (setq x z)
    (setq z (random 10))))

;; lets assume flow-ids:
;; x -> α    z -> β

;; one pass will get us:
;; x -> β    z -> χ

;; but that isn't the full story as next loop it would be
;; x -> χ    z -> δ

;; How should we handle this? Well here is a thought experiment
;; let's assume that each round we reset #'flow-id! so it gave out
;; ids in the same order.
;; Now the 3 iterations would look like this

;; x -> α    z -> β
;; ----------------
;; x -> β    z -> χ
;; x -> β    z -> χ

;; We have reached a fix point! That means the final flow-ids
;; are:
;; x -> (α β)    z -> (β χ)

;; eh the 3 iterations above are the correct idea but it is incorrect
;; to assume resetting flow Id would get that. Otherwise it would be

;; x -> α    z -> β
;; ----------------
;; x -> β    z -> α
;; x -> α    z -> α

;; which is wrong.. BUT it is some kind of control over the flow id.
;; What is it? maybe we start the flow-ids at the highest id currently
;; in the outside scope. This should give the first pattern (the one
;; we want) .. no wait it doesnt

;; x -> α    z -> β
;; ----------------
;; x -> β    z -> χ
;; x -> χ    z -> δ

;; Ok try again. This time we say we have reached a fixpoint if either
;; - nothing changes
;; - no new flow-ids are introduced from the outer scope

;; x -> α    z -> β
;; ----------------
;; x -> β    z -> χ

;; after first pass one of the changes, x=β is from the original scope
;; so we keep going

;; x -> χ    z -> δ

;; by now, none of the new flow-ids came from the outer scope.
;; We are done.


;;-    -    -    -    -    -    -    -    -    -    -    -

;; I think that by combining our 2 insights..

;; - repeat passes on body until fixpoint
;; - collections are unions of flow-ids

;; ..we get a way to properly analyze loops :)

;;-    -    -    -    -    -    -    -    -    -    -    -
