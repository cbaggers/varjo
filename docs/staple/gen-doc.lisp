(in-package :varjo.tests)

(defun gen-docs ()
  (staple:generate
   :varjo
   :extension (asdf:system-relative-pathname
               :varjo "docs/staple/varjo.ext.lisp"))

  (let* ((things '(staple:symb-accessor
                   staple:symb-function
                   staple:symb-macro
                   staple:symb-class
                   staple:symb-method
                   staple:symb-generic
                   staple:symb-special
                   staple:symb-variable
                   staple:symb-constant
                   staple:symb-condition
                   staple:symb-structure))
         (current (loop :for n :in things :collect (staple:converter n)))
         (ignores (lambda (x y) (declare (ignore x y)))))
    (loop
       :for n :in things
       :for c :in current
       :do (when c (setf (staple:converter n) ignores)))
    (unwind-protect
         (staple:generate
          :varjo
          :extension (asdf:system-relative-pathname
                      :varjo "docs/staple/vari.glsl.ext.lisp"))
      (loop
         :for n :in things
         :for c :in current
         :do (when c (setf (staple:converter n) c))))))
