(in-package :varjo)
(in-readtable fn:fn-reader)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *definitions-missing-from-glsl-spec*
    '((:name "gl_in" :type "{gl_PerVertex" :place-p t
       :versions (150 330 400 410 420 430 440 450)
       :stage :geometry)
      )))

(defmacro populate-vars ()
  (let ((vars (mapcar λ(destructuring-bind
                             (&key name type place-p versions (stage t)
                                   &allow-other-keys) _
                         (declare (ignore versions))
                         (let* ((lisp-name (intern (parse-gl-var-name name)
                                                   :varjo-lang))
                                (lisp-type (parse-gl-type-name type)))
                           `(,stage ,lisp-name ,name ,lisp-type ,place-p)))
                      (append *definitions-missing-from-glsl-spec*
                              glsl-spec:*variables*))))

    `(progn
       (setf *glsl-variables*
         ',(mapcar (lambda (stage-name stage-type-name)
                     (cons stage-type-name
                           (mapcar #'rest (remove-if-not
                                           λ(eq stage-name _)
                                           vars :key #'first))))
                   (cons t *stage-names*)
                   (cons t *stage-type-names*)))
       (export ',(mapcar #'second vars) :varjo-lang))))
(populate-vars)
