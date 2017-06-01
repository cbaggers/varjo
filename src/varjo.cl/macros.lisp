(in-package :varjo.cl)

(v-defmacro lambda (args &body body)
  `(labels ((lmbda ,args ,@body))
     #'lmbda))

(v-defmacro cond (&rest clauses)
  `(if ,(caar clauses)
       ,(cadar clauses)
       ,@(when (rest clauses)
               `((cond ,@(rest clauses))))))

(v-defmacro eql (&rest numbers)
  (if (> (length numbers) 2)
      `(%eql ,(first numbers) (eql ,@(rest numbers)))
      `(%eql ,@numbers)))

(v-defmacro equal (&rest numbers)
  (if (> (length numbers) 2)
      `(%equal ,(first numbers) (equal ,@(rest numbers)))
      `(%equal ,@numbers)))

(v-defmacro /= (&rest numbers)
  (if (> (length numbers) 2)
      `(%/= ,(first numbers) (/= ,@(rest numbers)))
      `(%/= ,@numbers)))
