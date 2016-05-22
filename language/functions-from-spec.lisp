(in-package :varjo)
(in-readtable fn:fn-reader)

(defmacro populate-functions ()
  `(progn
     ,@(loop :for func-spec :in glsl-spec:*functions* :append
	  (destructuring-bind (&key name return args versions) func-spec
	    (let* ((lisp-name (intern (parse-gl-func-name name) :varjo-lang))
		   (arg-types (mapcar #'second args))
		   (lisp-arg-types (mapcar #'parse-gl-type-name arg-types))
		   (lisp-return (parse-gl-type-name return))
		   (transform (format nil "~a(~{~a~^,~})" name
				      (loop :for i :below (length args) :collect
					 "~a"))))
	      (unless (some #'third args)
		`((add-function
		   ',lisp-name
		   (v-make-f-spec
		    ',lisp-name ,transform ',versions ',lisp-arg-types
		    ',lisp-return :v-place-index nil :glsl-name nil
		    :flow-ids (%gl-flow-id!)
		    :in-arg-flow-ids (n-of (%gl-flow-id!) ,(length args)))
		   *global-env*)
		  (export ',lisp-name :varjo-lang))))))))

(populate-functions)
