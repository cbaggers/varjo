(in-package :varjo)

;;------------------------------------------------------------
;; GLSL Variables
;;----------------


(defun free-name (name env &aux)
  (loop :for i = (append (get-var name env)
                         (get-function name env)) 
     :for num from 0
     :while i :do (setf name (symb name '- num))
     :finally (return name)))

(defun glsl-var-namep (name-symbol)
  (equal "GL-" (subseq (symbol-name name-symbol) 0 3)))
