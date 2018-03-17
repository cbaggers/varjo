(in-package :varjo.internals)
(in-readtable :fn.reader)

(defun merge-return-sets (sets)
  (declare (optimize (speed 3)))
  (labels ((qualified-eql (ret-a ret-b)
             (declare (type v-type ret-a ret-b))
             (and (v-type-eq ret-a ret-b)
                  (let ((qa (qualifiers ret-a))
                        (qb (qualifiers ret-b)))
                    (declare (type list qa qb))
                    (and (= (length qa) (length qb))
                         (every #'qualifier= qa qb)))))

           (type-sets-equal (set-a set-b)
             (declare (type vector set-a set-b))
             (and (= (length set-a) (length set-b))
                  (every #'qualified-eql set-a set-b)))

           (%merge-return-sets (set-a set-b)
             (assert (type-sets-equal set-a set-b)
                     () 'return-type-mismatch
                     :sets sets)
             set-a))
    (let* ((sets (remove nil sets)))
      (reduce #'%merge-return-sets (rest sets)
              :initial-value (first sets)))))

;;------------------------------------------------------------

(defgeneric nth-return-name (n stage &optional include-instance-name)
  (:method (n (stage stage) &optional include-instance-name)
    (format nil "~@[~a.~]_~a_OUT_~a"
            (when include-instance-name
              *out-block-name*)
            (substitute #\_ #\- (symbol-name (type-of stage)))
            n))
  (:method (n (stage tessellation-control-stage) &optional include-instance-name)
    (format nil "~@[~a.~]_~a_OUT_~a"
            (when include-instance-name
              (format nil "~a[gl_InvocationID]" *out-block-name*))
            (substitute #\_ #\- (symbol-name (type-of stage)))
            n))
  (:method (n (stage tessellation-evaluation-stage) &optional include-instance-name)
    (declare (ignore include-instance-name))
    (if (= n 0)
        "gl_Position"
        (call-next-method)))
  (:method (n (stage vertex-stage) &optional include-instance-name)
    (declare (ignore include-instance-name))
    (if (= n 0)
        "gl_Position"
        (call-next-method)))
  (:method (n (stage fragment-stage) &optional include-instance-name)
    (declare (ignore include-instance-name))
    (format nil "_~a_OUT_~a"
            (substitute #\_ #\- (symbol-name (type-of stage)))
            n)))

;;------------------------------------------------------------
