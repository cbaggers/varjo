(in-package :varjo)
(in-readtable :fn.reader)

;;------------------------------------------------------------

(defun make-return-val (type &optional qualifiers)
  (assert (typep type 'v-type))
  (assert (every #'keywordp qualifiers))
  (make-instance 'return-val :type type
                 :qualifiers (sort (copy-list qualifiers) #'<)))

(defun make-return-val-from-mval (mval)
  (make-return-val (v-type-of mval) (multi-val-qualifiers mval)))

(defun return-val-eql (ret-a ret-b)
  (and (v-type-eq (v-type-of ret-a) (v-type-of ret-b))
       (= (length (qualifiers ret-a)) (length (qualifiers ret-b)))
       (every #'eq (qualifiers ret-a) (qualifiers ret-b))))

;;------------------------------------------------------------

(defun make-return-set (&rest return-vals)
  (assert (every λ(typep _ 'return-val) return-vals))
  (apply #'vector return-vals))

(defun merge-return-sets (sets)
  (labels ((%merge-return-sets (set-a set-b)
             (assert (and (= (length set-a) (length set-b))
                          (every #'return-val-eql set-a set-b))
                     () 'return-type-mismatch
                     :returns (list set-a set-b))
             set-a))
    (let ((sets (remove nil sets)))
      (if (> (length sets) 1)
          (reduce #'%merge-return-sets (rest sets) :initial-value (first sets))
          (first sets)))))

(defun make-return-set-from-code-obj (code-obj env)
  (let ((mval-rets (mapcar #'make-return-val-from-mval (multi-vals code-obj)))
        (prim-ret (make-return-val (v-type-of code-obj))))
    (apply #'make-return-set
           (if (member :vertex (v-context env))
               mval-rets
               (cons prim-ret mval-rets)))))

;;------------------------------------------------------------

(defun nth-return-name (n)
  (format nil "_OUT_~a" n))

(defun mvals->out-form (code-object env)
  (declare (ignore env))
  (let ((mvals (multi-vals code-object)))
    `(progn
       ,@(loop :for mval :in mvals :for i :from 1 :collect
            (with-slots (value qualifiers) mval
              `(glsl-expr ,(format nil "~a = ~~a" (nth-return-name i))
                          :void ,value))))))


;; (defun out-qualifier-p (x env)
;;   (declare (ignore env))
;;   ;; {TODO} make this work properly
;;   (find x '(:smooth :flat :noperspective)))
