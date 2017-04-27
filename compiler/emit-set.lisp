(in-package :varjo)
(in-readtable :fn.reader)

;;------------------------------------------------------------

(defun make-emit-val (type &optional qualifiers)
  (assert (typep type 'v-type))
  (assert (every #'keywordp qualifiers))
  (make-instance 'emit-val
                 :type type
                 :qualifiers (sort (copy-list qualifiers) #'<)))

(defun make-emit-val-from-mval (mval)
  (let ((type (v-type-of mval))
        (qualifiers (multi-val-qualifiers mval)))
    (assert (typep type 'v-type))
    (assert (every #'keywordp qualifiers))
    (make-instance 'named-emit-val
                   :type type
                   :glsl-name (glsl-name (multi-val-value mval))
                   :qualifiers (sort (copy-list qualifiers) #'<))))

(defun emit-val-eql (ret-a ret-b)
  (and (v-type-eq (v-type-of ret-a) (v-type-of ret-b))
       (= (length (qualifiers ret-a)) (length (qualifiers ret-b)))
       (every #'eq (qualifiers ret-a) (qualifiers ret-b))))

;;------------------------------------------------------------

(defun make-emit-set (&rest emit-vals)
  (assert (every λ(typep _ 'emit-val) emit-vals))
  (apply #'vector emit-vals))

(defun merge-emit-sets (sets)
  (labels ((%merge-emit-sets (set-a set-b)
             (assert (and (= (length set-a) (length set-b))
                          (every #'emit-val-eql set-a set-b))
                     () 'emit-type-mismatch
                     :sets (list (map 'list #'v-type-of set-a)
                                 (map 'list #'v-type-of set-b)))
             set-a))
    (let* ((sets (remove nil sets)))
      (reduce #'%merge-emit-sets (rest sets)
              :initial-value (first sets)))))

(defun make-emit-set-from-code-obj (code-obj)
  (assert (not (v-typep (code-type code-obj) (type-spec->type :void))) ()
          "Varjo: Cannot emit void data")
  (let ((mval-rets (mapcar #'make-emit-val-from-mval (multi-vals code-obj)))
        (prim-emit (make-emit-val (v-type-of code-obj))))
    (apply #'make-emit-set (cons prim-emit mval-rets))))

;;------------------------------------------------------------

(defun %array-the-emit-vals-for-size (size emit-vals)
  (map 'vector
       λ(let ((type (v-type-of _)))
          (make-emit-val (v-array-type-of type size (flow-ids type))
                         (qualifiers _)))
       emit-vals))

;;------------------------------------------------------------
