(in-package :varjo)
(in-readtable fn:fn-reader)

(defparameter *definitions-missing-from-glsl-spec*
  '((:name "gl_in" :type "{gl_PerVertex" :place-p t
     :versions (150 330 400 410 420 430 440 450)
     :stage :geometry)
    ))

(defmacro populate-vars ()
  (let ((vars (mapcar λ(destructuring-bind
                             (&key name type place-p versions (stage t)) _
                         (declare (ignore versions))
                         (let* ((lisp-name (intern (parse-gl-var-name name)
                                                   :varjo-lang))
                                (lisp-type (parse-gl-type-name type)))
                           `(,stage ,lisp-name ,name ,lisp-type ,place-p)))
                      (append *definitions-missing-from-glsl-spec*
                              glsl-spec:*variables*))))

    `(progn
       (defparameter *glsl-variables*
         ',(mapcar (lambda (x)
                     (cons x (mapcar #'rest (remove-if-not λ(eq x _) vars
                                                           :key #'first))))
                   '(t :vertex :tesselation-control :tesselation-evaluation
                     :geometry :fragment)))
       (export ',(mapcar #'second vars) :varjo-lang))))
(populate-vars)
