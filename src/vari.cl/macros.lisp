(in-package :vari.cl)

(v-defmacro lambda (args &body body)
  `(labels ((lmbda ,args ,@body))
     #'lmbda))

(v-defmacro cond (&rest clauses)
  `(if ,(caar clauses)
       (progn
         ,@(cdar clauses))
       ,@(when (rest clauses)
           `((cond ,@(rest clauses))))))
