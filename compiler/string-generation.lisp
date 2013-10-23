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
