;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)

(v-defmacro :cond (&rest clauses)
  `(if ,(caar clauses)
       ,(cadar clauses)
       ,@(when (rest clauses)
               `((cond ,@(rest clauses))))))

(v-defmacro :incf (place &optional (delta 1))
  `(progn (setf ,place (+ ,place ,delta))
          ,place))

(v-define-compiler-macro :+ (&rest numbers)
  (cond
    ((= (length numbers) 1) (first numbers))
    ((> (length numbers) 2)
     `(%+ ,(first numbers) (+ ,@(rest numbers))))
    (t `(%+ ,@numbers))))

(v-define-compiler-macro :- (&rest numbers)
  (if (> (length numbers) 2)
      `(%- ,(first numbers) (- ,@(rest numbers)))
      `(%- ,@numbers)))

(v-define-compiler-macro :* (&rest numbers)
  (if (> (length numbers) 2)
      `(%* ,(first numbers) (* ,@(rest numbers)))
      `(%* ,@numbers)))

(v-define-compiler-macro :/ (&rest numbers)
  (if (> (length numbers) 2)
      `(%/ ,(first numbers) (/ ,@(rest numbers)))
      `(%/ ,@numbers)))

(v-define-compiler-macro := (&rest numbers)
  (if (> (length numbers) 2)
      `(%= ,(first numbers) (= ,@(rest numbers)))
      `(%= ,@numbers)))
(v-define-compiler-macro :eql (&rest numbers)
  (if (> (length numbers) 2)
      `(%eql ,(first numbers) (eql ,@(rest numbers)))
      `(%eql ,@numbers)))
(v-define-compiler-macro :equal (&rest numbers)
  (if (> (length numbers) 2)
      `(%equal ,(first numbers) (equal ,@(rest numbers)))
      `(%equal ,@numbers)))

(v-define-compiler-macro :> (&rest numbers)
  (if (> (length numbers) 2)
      `(%> ,(first numbers) (> ,@(rest numbers)))
      `(%> ,@numbers)))
(v-define-compiler-macro :< (&rest numbers)
  (if (> (length numbers) 2)
      `(%< ,(first numbers) (< ,@(rest numbers)))
      `(%< ,@numbers)))
(v-define-compiler-macro :>= (&rest numbers)
  (if (> (length numbers) 2)
      `(%>= ,(first numbers) (>= ,@(rest numbers)))
      `(%>= ,@numbers)))
(v-define-compiler-macro :<= (&rest numbers)
  (if (> (length numbers) 2)
      `(%<= ,(first numbers) (<= ,@(rest numbers)))
      `(%<= ,@numbers)))
