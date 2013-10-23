(in-package :varjo)

;; [TODO] How should we specify numbers unsigned?
(defun varjo->glsl (varjo-code)
  (labels ((num-suffix (type)
             (or (assocr (type-principle type) '((:float . "f") (:uint . "u")))
                 "")))
    (cond ((null varjo-code) nil)
          ((typep varjo-code 'code) varjo-code) 
          ((eq t varjo-code) (make-instance 'code :current-line "true" 
                                            :type '(:bool nil)))
          ((numberp varjo-code) 
           (let ((num-type (get-number-type varjo-code)))
             (make-instance 'code :current-line 
                            (format nil "~a~a" varjo-code (num-suffix num-type))
                            :type num-type)))
          ((atom varjo-code) 
           (if (assoc varjo-code *glsl-variables* :test #'symbol-name-equal)
               (instance-var varjo-code)
               (error "Varjo: '~s' is unidentified." varjo-code)))
          ((special-functionp (first varjo-code)) 
           (apply-special (first varjo-code) (rest varjo-code)))
          ((vfunctionp (first varjo-code))
           (compile-function (first varjo-code) (rest varjo-code)))
          (t (let ((error-message 
                    (format nil "Function '~s' is not available for ~A shaders in varjo."
                            (first varjo-code) *shader-context*)))
               (error 'missing-function-error :text error-message))))))


(defun compile-function (func-name args)
  (let ((func-specs (func-specs func-name))
        (arg-objs (mapcar #'varjo->glsl args)))
    (loop :for f-spec :in func-specs 
       :if (glsl-valid-function-args f-spec arg-objs )
       :return (merge-obs arg-objs
                          :type (glsl-resolve-func-type f-spec arg-objs)
                          :current-line (apply #'format 
                                               (append 
                                                (list nil (func-body f-spec))
                                                (mapcar #'current-line
                                                        arg-objs))))
       :finally (error "There is no applicable method for the glsl function '~s'~%when called with argument types:~%~s " func-name (mapcar #'code-type arg-objs)))))

(defun macroexpand-and-substitute (varjo-code)
  (cond ((null varjo-code) nil)
        ((listp varjo-code) 
         (let ((sub (substitution (first varjo-code)))) 
           (if sub
               (mapcar #'macroexpand-and-substitute
                       (apply sub (rest varjo-code)))
               (mapcar #'macroexpand-and-substitute
                       varjo-code))))
        (t varjo-code)))

(defun get-number-type (x)
  (cond ((floatp x) '(:float nil))
        ((integerp x) '(:int nil))
        (t (error "Varjo: Do not know the type of the number '~s'" x))))

(defun compile-var (name type &rest qualifiers)
  (%compile-var name type qualifiers))

(defun %compile-var (name type &optional qualifiers)
  (%qualify (varjo->glsl `(%in-typify (%make-var
                                       ,name 
                                       ,(flesh-out-type type))))
            qualifiers))

(defun %qualify (obj qualifiers)
  (merge-obs obj :current-line (format nil "~(~{~a ~}~)~a" 
                                       qualifiers 
                                       (current-line obj))))

(defun qualify (obj &rest qualifiers)
  (%qualify obj qualifiers))

(defun get-vars-for-context (context)
  (loop for item in context
     :append (assocr item *built-in-vars*
                     :test #'symbol-name-equal)))
