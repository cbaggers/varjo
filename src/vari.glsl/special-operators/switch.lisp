(in-package :vari.glsl)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Switch

(v-defmacro switch (test-form &body clauses)
  (assert (loop :for (test) :in clauses :always
             (valid-for-switch-statementp test))
          () 'switch-type-error :form (cons test-form clauses))
  `(case ,test-form ,@clauses))

(defun valid-for-switch-statementp (key-primary-type)
  (or (integerp key-primary-type)
      (floatp key-primary-type)))

;;------------------------------------------------------------
