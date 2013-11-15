(in-package :varjo)

;; [TODO] name generation MUST take reserver glsl names into account.

;; (defgeneric indent (input))

;; (defmethod indent ((input string))
;;   (mapcar #'(lambda (x) (format nil "    ~a" x))
;;           (split-sequence:split-sequence #\newline input)))

;; (defmethod indent ((input list))
;;   (mapcan #'indent input))

;; (defun indent-ob (code-obj)
;;   (merge-obs code-obj
;;              :to-block (indent (to-block code-obj))))

;; (defun write-output-string (version struct-definitions
;;                             code in-vars out-vars uniforms)
;;   (if (or (to-block code) (current-line code))
;;       (error "The following code not written to output.~%~a~%~a"
;;              (to-block code) (current-line code))
;;       (format 
;;        nil 
;;        "#version ~a~%~{~%~{~a~%~}~}" 
;;        version
;;        (remove-if #'null
;;                   (list
;;                    (mapcar #'struct-init-form struct-definitions)
;;                    (mapcar #'(lambda (x) (current-line (first x))) 
;;                            (remove-if #'null in-vars))
;;                    (mapcar #'(lambda (x) (current-line x)) 
;;                            (remove-if #'null out-vars))
;;                    (mapcar #'current-line uniforms)
;;                    (to-top code))))))

;;----v-v-v-new-v-v-v----;;

(defun num-suffix (type)
  (or (assocr (v-type-name type) '((v-float . "f") (v-uint . "u"))) ""))

(defun gen-number-string (number type)
    (format nil "~a~a" number (num-suffix type)))

(defun gen-variable-string (var-name)
  (format nil "~a" var-name))

(defun gen-function-string (func arg-objs)
  (apply #'format nil (v-glsl-string func) (mapcar #'current-line arg-objs)))

(defun gen-function-body-string (name args type body-obj)
  (format nil "~a ~a(~(~{~{~a ~a~}~^,~^ ~}~)) {~%~{~a~%~}~@[    ~a~%~]}~%"
          (v-glsl-string type)
          name 
          (mapcar #'reverse args)
          (to-block body-obj) 
          (current-line (end-line body-obj))))

(defun qualify (obj &rest qualifiers)
  (%qualify obj qualifiers))

(defun %qualify (obj qualifiers)
  (merge-obs obj :current-line (format nil "~(~{~a ~}~)~a" 
                                       qualifiers 
                                       (current-line obj))))

;;[TODO] Work out where to handle qualifiers
(defun prefix-type-declaration (code-obj &optional qualifiers)
  (let* ((type (code-type code-obj))
         (line (cond ((typep type 'v-array) (format nil (v-glsl-string type)
                                                    (current-line code-obj)))
                     ((typep type 'v-type) (format nil "~a ~a" 
                                                   (v-glsl-string type)
                                                   (current-line code-obj)))
                     (t (error "dont know how to add the type here")))))
    (if qualifiers
        (format nil "~{~a ~} ~a" qualifiers line)
        line)))


;;[TODO] make this properly
(defun lisp-name->glsl-name (name)
  (string name))
