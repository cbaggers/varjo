(in-package :vari.cl)

(v-defmacro lambda (args &body body)
  `(labels ((lmbda ,args ,@body))
     #'lmbda))

(v-defmacro cond (&rest clauses)
  (assert clauses ()
          "Varjo: COND must contain at least 1 clause")
  `(if ,(caar clauses)
       (progn
         ,@(or (cdar clauses) '((values))))
       ,@(when (rest clauses)
           `((cond ,@(rest clauses))))))
