(in-package :varjo)
(in-readtable fn:fn-reader)

(macrolet
    ((populate-from-spec ()
       (labels ((name->symb (name &optional (prefix "V"))
		  (intern
		   (format nil "~@[~a-~]~{~a~^-~}"
			   prefix
			   (mapcar #'string-upcase
				   (split-seq #'upper-case-p name :keep-split t))))))
	 (loop :for func-spec :in glsl-spec:*functions* :do
	    (destructuring-bind (&key name return args versions) func-spec
	      (let* ((lisp-name (name->symb name nil))
		     (arg-types (mapcar #'second args))
		     (lisp-arg-types (mapcar #'name->symb arg-types))
		     (lisp-return (name->symb return))
		     (transform (format nil "~a(~{~a~^,~})" name
					(loop :for i :below (length args) :collect
					   "~a"))))
		(add-function
		 lisp-name
		 (v-make-f-spec
		  lisp-name transform versions lisp-arg-types lisp-return
		  :v-place-index nil :glsl-name nil :flow-ids (%gl-flow-id!)
		  :in-arg-flow-ids (n-of (%gl-flow-id!) (length args)))
		 *global-env*)))))))
  (populate-from-spec))


(defun parse-gl-func-name (name)
  (with-input-from-string (seq name)
    (let (last-case
	  case-changed
	  (frist t))
      (labels ((readc (seq)
		 (let ((r (read-char seq nil :eos)))
		   (unless (eq r :eos)
		     (setf case-changed (and (not last-case)
					     (upper-case-p r))
			   last-case (upper-case-p r)))
		   r)))
	(format nil "~{~a~}"
		(loop :for char = (readc seq)
		   :while (not (eq char :eos)) :collect
		   (prog1
		       (if (and case-changed (not frist))
			   (format nil "-~a" (string-upcase char))
			   (string-upcase char))
		     (setf frist nil))))))))

(defun parse-gl-type-name (name)
  (with-input-from-string (seq name)

    (let ((array-p (char= (peek-char nil seq) #\[))
	  last-case
	  case-changed
	  (frist t))
      (when array-p (read-char seq nil nil))
      (labels ((readc (seq)
		 (let ((r (read-char seq nil :eos)))
		   (unless (eq r :eos)
		     (let ((check (if last-case
				      (or (digit-char-p r)
					  (upper-case-p r))
				      (upper-case-p r))))
		       (setf case-changed (and (not last-case)
					       check)
			     last-case check)))
		   r)))
	(format nil "V-~{~a~}"
		(loop :for char = (readc seq)
		   :while (not (eq char :eos)) :collect
		   (prog1
		       (if (char= char #\_)
			   "-"
			   (if (and case-changed (not frist))
			       (format nil "-~a" (string-upcase char))
			       (string-upcase char)))
		     (setf frist nil))))))))

(break "Chris, stop and goto functions-from-spec.
You have stuff to finish there")
;; CHECK THE TYPES
;;
;; (mapcar #'parse-gl-type-name
;; 	(remove-duplicates
;; 	 (mapcan (lambda (x) (mapcar #'second (elt x 5)))
;; 		 glsl-spec:*functions*)
;; 	 :test #'equal))

(defun parse-gl-var-name (name)
  (with-input-from-string (seq name :start 3)
    (let (last-case
	  case-changed
	  (frist t))
      (labels ((readc (seq)
		 (let ((r (read-char seq nil :eos)))
		   (unless (eq r :eos)
		     (setf case-changed (and (not last-case)
					     (upper-case-p r))
			   last-case (upper-case-p r)))
		   r)))
	(format nil "GL-~{~a~}"
		(loop :for char = (readc seq)
		   :while (not (eq char :eos)) :collect
		   (prog1
		       (if (and case-changed (not frist))
			   (format nil "-~a" (string-upcase char))
			   (string-upcase char))
		     (setf frist nil))))))))
