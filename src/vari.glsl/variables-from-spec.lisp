(in-package :vari.glsl)
(in-readtable fn:fn-reader)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *definitions-missing-from-glsl-spec*
    '((:lisp-name "GL-IN" :name "gl_in" :type "{gl_PerVertex" :place-p t
       :versions (150 330 400 410 420 430 440 450 460)
       :stage :geometry)
      )))

(defmacro populate-vars ()
  (let ((vars (mapcar λ(destructuring-bind
                             (&key lisp-name name type place-p versions
                                   (stage t) &allow-other-keys) _
                         (declare (ignore versions))
                         (assert lisp-name)
                         (let* ((lisp-name (intern lisp-name :vari.glsl))
                                (lisp-type (parse-gl-type-name type)))
                           `(,stage ,lisp-name ,name ,lisp-type ,place-p)))
                      (append *definitions-missing-from-glsl-spec*
                              glsl-spec:*variables*))))

    `(progn
       (setf varjo.internals::*glsl-variables*
         ',(mapcar (lambda (stage-name stage-type-name)
                     (cons stage-type-name
                           (mapcar #'rest (remove-if-not
                                           λ(eq stage-name _)
                                           vars :key #'first))))
                   (cons t *stage-names*)
                   (cons t *stage-type-names*)))
       (export ',(mapcar #'second vars) :vari.glsl))))

(populate-vars)
