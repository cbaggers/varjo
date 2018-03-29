(in-package :varjo.internals)
(in-readtable :fn.reader)

;;------------------------------------------------------------

(defun cast-code (src-obj cast-to-type)
  (cast-code-inner (primary-type src-obj) src-obj cast-to-type))

(defmethod cast-code-inner (varjo-type src-obj cast-to-type)
  (declare (ignore varjo-type))
  (let* ((src-type (primary-type src-obj))
         (dest-type (set-flow-id cast-to-type (flow-ids src-type))))
    (if (v-type-eq src-type cast-to-type)
        (copy-compiled src-obj :type-set (make-type-set dest-type))
        (copy-compiled src-obj :current-line (cast-string cast-to-type src-obj)
                       :type-set (make-type-set dest-type)))))

(defmethod cast-code-inner
    (varjo-type src-obj (cast-to-type v-function-type))
  (declare (ignore varjo-type))
  (let ((new-type (make-instance
                   'v-function-type
                   :arg-spec (v-argument-spec cast-to-type)
                   :return-spec (v-return-spec cast-to-type)
                   :ctv (ctv (primary-type src-obj))
                   :flow-ids (flow-ids src-obj))))
    (if (v-type-eq (primary-type src-obj) new-type)
        (copy-compiled src-obj :type-set (make-type-set new-type))
        (copy-compiled
         src-obj
         :current-line (cast-string new-type src-obj)
         :type-set (make-type-set new-type)))))

;;------------------------------------------------------------

(defun cast-for-array-literal (target-element-type elements)
  (labels ((cast (x)
             (if (v-type-eq (primary-type x) target-element-type)
                 x
                 (cast-code x target-element-type))))
    (mapcar #'cast elements)))

;;------------------------------------------------------------
