(in-package :varjo)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Prog1

(v-defmacro prog1 (&body body)
  (let ((tmp (gensym "PROG1-TMP")))
    `(let ((,tmp ,(first body)))
       ,@(rest body)
       ,tmp)))

;;------------------------------------------------------------
;; Progn

(v-defspecial progn (&rest body)
  ;; this is super important as it is the only function that implements
  ;; imperitive coding. It does this by passing the env from one form
  ;; to the next.
  ;; it also returns this mutated env
  :args-valid t
  :return
  (if body
      (merge-progn (compile-progn body env) env)
      (error 'empty-progn)))

;;------------------------------------------------------------
