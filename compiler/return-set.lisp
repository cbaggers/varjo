(in-package :varjo)
(in-readtable :fn.reader)

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

(defun make-return-set-from-code-obj (code-obj)
  (cons (list (v-type-of code-obj))
        (let ((mvals (multi-vals code-obj)))
          (loop :for mval :in mvals :for i :from 1 :collect
             (cons (v-type-of mval)
                   (multi-val-qualifiers mval))))))

(defun make-return-set (env &rest qualifiers)
  (warn "make-return-set should take a type explicitly and assert it")
  (apply #'vector (check-return-set env qualifiers)))

(defun make-single-val-return-set (env type)
  (make-return-set env (list type)))

(defun nth-return-name (n)
  (format nil "_OUT_~a" n))

(defun mvals->out-form (code-object env)
  (declare (ignore env))
  (warn "mvals->out-form: Yo chris. We used to emit:~% `(%out (,(gensym \"OUT\") ,@qualifiers) ,value)~%here. However our changes mean we loose the other qualifiers. Please fix this")
  (let ((mvals (multi-vals code-object)))
    `(progn
       ,@(loop :for mval :in mvals :for i :from 1 :collect
            (with-slots (value qualifiers) mval
              `(glsl-expr ,(format nil "~a = ~~a" (nth-return-name i))
                          :void ,value))))))
