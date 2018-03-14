(in-package :varjo.internals)

(defvar *in-qualifier*
  (make-instance 'qualifier :name :in :glsl-string "in"))

(defvar *out-qualifier*
  (make-instance 'qualifier :name :out :glsl-string "out"))

(defmethod print-object ((obj qualifier) stream)
  (format stream "#<QUALIFIER ~a>" (name obj)))

(defun parse-qualifier (qualifier-form)
  (destructuring-bind (name &rest args) (listify qualifier-form)
    (let ((spec (find name *varjo-qualifiers* :key #'first)))
      ;; {TODO} Proper error
      (assert spec () "Varjo: '~a' is not a valid qualifier." name)
      (case name
        (:feedback (parse-feedback-qualifier name args))
        (otherwise (make-instance 'qualifier
                                  :name name
                                  :glsl-string (second spec)))))))

(defun parse-feedback-qualifier (name args)
  (let ((fb-arg-len (length args))
        (arg (or (first args) 0)))
    (assert (and (or (= fb-arg-len 0) (= fb-arg-len 1))
                 (and (integerp arg) (>= arg 0)))
            () 'invalid-feedback-qualifier-form
            :form (if args (cons name args) name))
    (make-instance 'feedback-qualifier
                   :name name
                   :group arg
                   :glsl-string nil)))

(defmethod qualifier= ((qual-a qualifier) (qual-b qualifier))
  (eq (name qual-a) (name qual-b)))

(defmethod qualifier= ((qual-a feedback-qualifier)
                       (qual-b feedback-qualifier))
  (and (eq (name qual-a) (name qual-b))
       (eql (feedback-group qual-a) (feedback-group qual-b))))

(defmethod qualifier= (a b)
  (let ((a (if (typep a 'qualifier)
               (name a)
               a))
        (b (if (typep b 'qualifier)
               (name b)
               b)))
    (string= a b)))


(defun block-memory-layout-qualfier-p (qualifier)
  (check-type qualifier qualifier)
  (not (null (member (name qualifier)
                     '(:std-140 :std-430 :packed :shared)))))

(defmethod qualifiers ((obj shader-variable))
  (qualifiers (v-type-of obj)))
