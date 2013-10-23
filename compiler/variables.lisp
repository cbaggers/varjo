(in-package :varjo)

;;------------------------------------------------------------
;; GLSL Variables
;;----------------

(defun var-name (var)
  (first var))

(defun var-type (var)
  (second var))

(defun var-gl-name (var)
  (third var))

(defun var-read-only (var)
  (fourth var))

(defun var-restriction (var)
  (fifth var))

(defun compile-let-forms (let-forms &optional (typify t) 
                                      (gensym-vars t))
  ;; takes forms and returns a list of two things
  ;; the compiled forms and the variable forms which can be 
  ;; appended to *glsl-variables*
  (labels ((var-name (form) 
             (if (listp (first form)) (first (first form))
                 (first form)))
           (var-type (form) 
             (when (listp (first form))
               (flesh-out-type (second (first form)))))
           (val (form) 
             (second form))
           (compile-form (name type value)
             (if value
                 (if typify
                     (varjo->glsl `(%typify 
                                    (setf (%make-var ,name ,type)
                                          ,value)))
                     (varjo->glsl `(setf (%make-var ,name ,type) 
                                         ,value)))
                 (if typify
                     (varjo->glsl `(%typify 
                                    (%make-var ,name ,type)))
                     (varjo->glsl `(%make-var ,name ,type) )))))
    (let* ((val-objs (loop :for form in let-forms
                        :collect (varjo->glsl (val form))))
           (var-names (mapcar #'var-name let-forms))
           (var-gl-names (if gensym-vars
                             (mapcar #'glsl-gensym var-names)
                             (mapcar #'safe-gl-name var-names)))
           (var-types (loop :for form :in let-forms
                         :for obj :in val-objs
                         :collect (or (var-type form)
                                      (when obj 
                                        (code-type obj))))))
      ;; THe above can be nil when the val half __^^^^^^
      ;; of the let form is left blank
      (list (mapcar #'compile-form var-gl-names var-types val-objs)
            (mapcar #'list var-names var-types var-gl-names)))))

(defun var-existsp (name)
  (not (null (assoc name *glsl-variables*
                    :test #'symbol-name-equal))))

;;probably redundant
(defmacro add-vars ((var-declarations &optional (shadowing t))
                    &body body)
  
  `(if (or ,shadowing 
           (notany #'(lambda (var) (var-existsp (var-name var)))
                   ,var-declarations))
       (let ((*glsl-variables* (append ,var-declarations 
                                       *glsl-variables*)))
         ,@body)
       (error "Variable already defined and cannot be shadowed.~%~a~%~a" ,var-declarations *glsl-variables*)))

(defun instance-var (symbol)
  (let ((var-spec (assoc symbol *glsl-variables*
                         :test #'symbol-name-equal)))
    (make-instance 'code
                   :type (let ((new-type (flesh-out-type (var-type var-spec))))
                           (if (var-read-only var-spec)
                               new-type
                               (set-place-t new-type)))
                   :current-line (format nil "~a" 
                                         (or (var-gl-name var-spec)
                                             (var-name var-spec))))))
