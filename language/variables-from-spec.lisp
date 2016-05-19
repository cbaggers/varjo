(in-package :varjo)
(in-readtable fn:fn-reader)

(defmacro populate-from-spec ()
  (labels ((name->symb (name &optional (prefix "V"))
	     (intern
	      (format nil "~@[~a-~]~{~a~^-~}"
		      prefix
		      (mapcar #'string-upcase
			      (remove "_"
				      (split-seq λ(or (upper-case-p _) (char= #\_ _))
						 ;;(substitute #\- #\_ name)
						 name
						 :keep-split t)
				      :test #'equal))))))
    (loop :for func-spec :in glsl-spec:*variables* :collect
       (destructuring-bind (&key name type place-p versions stage)
	   func-spec
	 (let* ((lisp-name (name->symb name nil))
		(lisp-type :- ;;(name->symb type)
		  ))
	   (list lisp-name name lisp-type place-p versions stage))))))

(populate-from-spec)
