(in-package :varjo)

(defgeneric indent (input))

(defmethod indent ((input string))
  (mapcar #'(lambda (x) (format nil "    ~a" x))
          (split-sequence:split-sequence #\newline input)))

(defmethod indent ((input list))
  (mapcan #'indent input))

(defun indent-ob (code-obj)
  (merge-obs code-obj
             :to-block (indent (to-block code-obj))))

(defun write-output-string (version struct-definitions
                            code in-vars out-vars uniforms)
  (if (or (to-block code) (current-line code))
      (error "The following code not written to output.~%~a~%~a"
             (to-block code) (current-line code))
      (format 
       nil 
       "#version ~a~%~{~%~{~a~%~}~}" 
       version
       (remove-if #'null
                  (list
                   (mapcar #'struct-init-form struct-definitions)
                   (mapcar #'(lambda (x) (current-line (first x))) 
                           (remove-if #'null in-vars))
                   (mapcar #'(lambda (x) (current-line x)) 
                           (remove-if #'null out-vars))
                   (mapcar #'current-line uniforms)
                   (to-top code))))))

;;----v-v-v-new-v-v-v----;;
(defmethod v-type->string ((type v-type))
  (format nil "~a" (v-glsl-string type)))

(defmethod v-type->string ((type v-array))
  (format nil "~a[~a]" (v-glsl-string type) (v-length type)))
