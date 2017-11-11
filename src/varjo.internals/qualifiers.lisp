(in-package :varjo.internals)

(defvar *in-qualifier*
  (make-instance 'qualifier :name :in :glsl-string "in"))

(defvar *out-qualifier*
  (make-instance 'qualifier :name :out :glsl-string "out"))

(defun parse-qualifier (qualifier-form)
  (destructuring-bind (name &rest args) (listify qualifier-form)
    (case name
      (:feedback (parse-feedback-qualifier name args))
      (otherwise (make-instance 'qualifier
                                :name name
                                :glsl-string (string-downcase (string name)))))))

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

(defmethod qualifier-equal (qual-a qual-b)
  (eq (name qual-a) (name qual-b)))

(defmethod qualifier-equal ((qual-a feedback-qualifier)
                            (qual-b feedback-qualifier))
  (and (eq (name qual-a) (name qual-b))
       (eql (feedback-group qual-a) (feedback-group qual-b))))
