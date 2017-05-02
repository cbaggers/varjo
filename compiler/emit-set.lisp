(in-package :varjo)
(in-readtable :fn.reader)

;;------------------------------------------------------------

(defun emit-val-eql (ret-a ret-b)
  (and (v-type-eq ret-a ret-b)
       (= (length (qualifiers ret-a)) (length (qualifiers ret-b)))
       (every #'eq (qualifiers ret-a) (qualifiers ret-b))))

(defun merge-emit-sets (sets)
  (labels ((%merge-emit-sets (set-a set-b)
             (assert (and (= (length set-a) (length set-b))
                          (every #'emit-val-eql set-a set-b))
                     () 'emit-type-mismatch
                     :sets (list (map 'list #'type->type-spec set-a)
                                 (map 'list #'type->type-spec set-b)))
             set-a))
    (let* ((sets (remove nil sets)))
      (reduce #'%merge-emit-sets (rest sets)
              :initial-value (first sets)))))

;;------------------------------------------------------------
