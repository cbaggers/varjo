(in-package :varjo)
(in-readtable :fn.reader)

;;------------------------------------------------------------

(defun emit-val-eql (ret-a ret-b)
  (let ((ret-a (if (typep ret-a 'v-type) ret-a (v-type-of ret-a)))
        (ret-b (if (typep ret-b 'v-type) ret-b (v-type-of ret-b))))
    (and (v-type-eq ret-a ret-b)
         (= (length (qualifiers ret-a)) (length (qualifiers ret-b)))
         (every #'eq (qualifiers ret-a) (qualifiers ret-b)))))

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

;;------------------------------------------------------------
