(in-package :varjo.internals)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------

(defun make-extension (extension-form)
  (let* ((ext (listify extension-form))
         (name (first ext))
         (behavior (or (and (= (length ext) 2)
                            (second ext))
                       :enable)))
    (assert (stringp (first ext)))
    (assert (and (keywordp behavior)
                 (member behavior *glsl-extension-behaviors*))
            () "Varjo: Not a valid extension behavior: ~a" behavior)
    (make-instance 'extension
                   :name name
                   :behavior behavior
                   :glsl-string (format nil "~a : ~(~a~)"
                                        name behavior))))

