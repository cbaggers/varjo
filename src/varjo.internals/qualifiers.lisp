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
      (when (third spec)
        (assert (first args) ()
                "Varjo: Qualifier '~a' requires an argument, but none was specified" (first spec)))
      (case name
        (:feedback (parse-feedback-qualifier name args))
        (otherwise (make-instance 'qualifier
                                  :name name
                                  :glsl-string (if (third spec)
                                                   (format nil "~a = ~a"
                                                           (second spec)
                                                           (first args))
                                                   (second spec))))))))

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

(defun constant-memory-layout-qualifier-p (qualifier)
  (check-type qualifier qualifier)
  (not (null (member (name qualifier)
                     '(:constant-id)))))

(defun image-memory-layout-qualifier-p (qualifier)
  (check-type qualifier qualifier)
  (not (null (find (name qualifier) *glsl-image-format-qualifiers* :key #'first))))

(defun uniform-memory-layout-qualifier-p (qualifier &optional imagep)
  (check-type qualifier qualifier)
  (or (not (null (member (name qualifier)
                         '(:binding
                           :set))))
      (and imagep
           (image-memory-layout-qualifier-p qualifier))))

(defun block-memory-layout-qualifier-p (qualifier)
  (check-type qualifier qualifier)
  (not (null (member (name qualifier)
                     '(:std-140
                       :std-430
                       :packed
                       :shared
                       :binding
                       :set
                       :push-constant)))))

(defun memory-layout-qualifier-p (qualifier)
  (or (block-memory-layout-qualifier-p qualifier)
      (uniform-memory-layout-qualifier-p qualifier)))

(defmethod qualifiers ((obj shader-variable))
  (qualifiers (v-type-of obj)))
