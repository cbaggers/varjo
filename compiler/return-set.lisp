(in-package :varjo)
(in-readtable :fn.reader)

;;------------------------------------------------------------

(defun make-return-val (type &optional qualifiers)
  (assert (typep type 'v-type))
  (assert (every #'keywordp qualifiers))
  (make-instance 'return-val
                 :type type
                 :qualifiers (sort (copy-list qualifiers) #'<)))


(defun make-external-return-val (glsl-name type &optional qualifiers)
  (assert (typep type 'v-type))
  (assert (every #'keywordp qualifiers))
  (make-instance 'external-return-val
                 :out-name glsl-name
                 :type type
                 :qualifiers (sort (copy-list qualifiers) #'<)))

(defun make-return-val-from-mval (mval)
  (let ((type (v-type-of mval))
        (qualifiers (multi-val-qualifiers mval)))
    (assert (typep type 'v-type))
    (assert (every #'keywordp qualifiers))
    (make-instance 'named-return-val
                   :type type
                   :glsl-name (v-glsl-name (multi-val-value mval))
                   :qualifiers (sort (copy-list qualifiers) #'<))))

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
                     :sets (list set-a set-b))
             set-a))
    (let* ((sets (remove nil sets)))
      (reduce #'%merge-return-sets (rest sets)
                        :initial-value (first sets)))))

(defun make-return-set-from-code-obj (code-obj)
  (let ((mval-rets (mapcar #'make-return-val-from-mval (multi-vals code-obj)))
        (prim-ret (make-return-val (v-type-of code-obj))))
    (apply #'make-return-set (cons prim-ret mval-rets))))

;;------------------------------------------------------------

(defgeneric nth-return-name (n stage)
  (:method (n (stage stage))
	(format nil "_~a_OUT_~a" (type-of stage) n)))

(defun mvals->out-form (code-object env)
  (let ((mvals (multi-vals code-object))
        (stage (stage env)))
    `(progn
       ,@(loop :for mval :in mvals :for i :from 1 :collect
            (with-slots (value qualifiers) mval
              `(glsl-expr ,(format nil "~a = ~~a" (nth-return-name i stage))
                          :void ,value))))))


;; (defun out-qualifier-p (x env)
;;   (declare (ignore env))
;;   ;; {TODO} make this work properly
;;   (find x '(:smooth :flat :noperspective)))
