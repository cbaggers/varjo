(in-package :varjo)

(defun out-qualifier-p (x env)
  (declare (ignore env))
  ;; {TODO} make this work properly
  (find x '(:smooth :flat :noperspective)))

(defun check-return-set (env set)
  (every λ(every λ(out-qualifier-p _ env) _) set)
  set)

(defun %merge-return-sets (set-a set-b)
  (warn "no ship until fix 0")
  (assert (every #'equal set-a set-b))
  set-a)

(defun merge-return-sets (sets)
  (let ((sets (remove nil sets)))
    (if (> (length sets) 1)
        (reduce #'%merge-return-sets (rest sets) :initial-value (first sets))
        (first sets))))

(defun make-return-set (env &rest qualifiers)
  (apply #'vector (check-return-set env qualifiers)))
