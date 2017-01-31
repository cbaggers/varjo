(in-package :varjo)

(symbol-macrolet )

(symbol-macrolet ((x 'foo))
  (list x (let ((x 'bar)) x)))

;; defined as symbol-macrolet ((symbol expansion)*) declaration* form*
;; so thats

(symbol-macrolet ((symb-a expansion-a)
                  (symb-b expansion-b))
  (declare ..)
  (declare ..)
  body-a
  body-b)

;; THIS CAUSES RECURSIVE EXPANSION
;; (macroexpand-dammit:macroexpand-dammit
;;  '(symbol-macrolet ((a (list a)))
;;    (print a)))

(macrolet ((foo (x) `(v! x x)))
  (foo))

;; (macrolet ((name lambda-list
;;              [[local-declaration* | local-documentation]]
;;              local-form*)*)
;;   declaration*
;;   form*)


;; this works! this is well know, but I certainly didnt know it :)
(destructuring-bind (foo bar &body body) '(1 2 3 4 5 6)
  (list foo bar body))

(defun extract-arg-pair (lambda-list key)
  (let* ((key-pos (position key lambda-list :test #'symbol-name-equal))
         (value (when key-pos
                  (first (subseq lambda-list (1+ key-pos)))))
         (cleaned (if key-pos
                      (append (subseq lambda-list 0 key-pos)
                              (subseq lambda-list (+ 2 key-pos)))
                      lambda-list)))
    (values value cleaned)))

;; {TODO} this doesnt handle doc-strings in the v-macros
(defmacro new-v-defmacro (name lambda-list &body body)
  (alexandria:with-gensyms (form-var environment)
    (vbind (context lambda-list)
        (extract-arg-pair lambda-list :&context)
      (vbind (maybe-env lambda-list)
          (extract-arg-pair lambda-list :&environment)
        (let* ((env-var (or maybe-env environment)))
          `(progn
             (add-macro
              ',name
              (lambda (,form-var ,env-var)
                (declare (ignorable ,env-var))
                (destructuring-bind ,lambda-list ,form-var
                  ,@body))
              ,context
              *global-env*)
             ',name))))))

#+nil
(new-v-defmacro jam (&environment e x y &body body)
  (list x y body))

(defun testar (&rest foo)
  (print foo))

(define-compiler-macro testar (&whole w &rest foo)
  (print w)
  `(print ',foo))

;; Its manner of definition is the same as for defmacro; the only
;; differences are:
;;
;; * The name can be a function name naming any function or macro.
;;
;; * The expander function is installed as a compiler macro function for the
;;   name, rather than as a macro function.
;;
;; * The &whole argument is bound to the form argument that is passed to the
;;   compiler macro function. The remaining lambda-list parameters are specified
;;   as if this form contained the function name in the car and the actual
;;   arguments in the cdr, but if the car of the actual form is the symbol
;;   funcall, then the destructuring of the arguments is actually performed
;;   using its cddr instead.
;;
;; * Documentation is attached as a documentation string to name (as kind
;;   compiler-macro) and to the compiler macro function.
;;
;; * Unlike an ordinary macro, a compiler macro can decline to provide an
;;   expansion merely by returning a form that is the same as the original
;;   (which can be obtained by using &whole).
;;             ↑↑↑↑↑↑↑↑
;;            Important!
