(in-package :varjo)

(defclass varjo-compile-result ()
  ((glsl-code :initarg :glsl-code :accessor glsl-code)
   (stage-type :initarg :stage-type :accessor stage-type)
   (out-vars :initarg :out-vars :accessor out-vars)
   (in-args :initarg :in-args :accessor in-args)
   (uniforms :initarg :uniforms :accessor uniforms)
   (implicit-uniforms :initarg :implicit-uniforms :accessor implicit-uniforms)
   (context :initarg :context :accessor context)
   (function-calls :initarg :function-calls :accessor function-calls)
   (used-macros :initarg :used-macros :reader used-macros)
   (used-compiler-macros :initarg :used-compiler-macros
			 :reader used-compiler-macros)
   (ast :initarg :ast :reader ast)
   (flow-id-origins :initarg :flow-id-origins)
   (val-origins :initarg :val-origins)
   (used-symbol-macros :initarg :used-symbol-macros
		       :reader used-symbol-macros)
   (third-party-metadata :initarg :third-party-metadata
			 :initform (make-hash-table)
			 :reader third-party-metadata)))
;; {NOTE} third-party-metadata only applies to rolling-translate

(defun %compile-result-flow-id-origins (flow-id r error-on-missingp)
  (assert (typep r 'varjo-compile-result))
  (labels ((raw->ast (id)
	     (let ((raw-id (slot-value id 'val)))
	       (or (assocr raw-id (slot-value r 'flow-id-origins) :test #'=)
		   (when error-on-missingp
		     (error "Could not find origin for ~s" raw-id))))))
    (flatten (mapcar #'raw->ast (ids flow-id)))))

(defun %alist-flow-id-origins (flow-id source error-on-missingp)
  (labels ((get-seen (raw-id)
	     (or (assocr raw-id source :test #'=)
		 (when error-on-missingp
		   (error "Could not find origin for ~s" raw-id))))
	   (per-val-id (val-id)
	     (let ((raw-id (slot-value val-id 'val)))
	       (or (get-seen raw-id)
		   (when error-on-missingp
		     (error "Could not find origin for ~s" raw-id))))))
    (flatten (mapcar λ(per-val-id _) (ids flow-id)))))


(defmethod flow-id-origins ((flow-id flow-identifier)
			   &optional r error-on-missingp)
  (typecase r
    (varjo-compile-result (%compile-result-flow-id-origins
			   flow-id r error-on-missingp))
    (list (%alist-flow-id-origins
	   flow-id r error-on-missingp))))

(defmethod val-origins ((flow-id flow-identifier) &optional r error-on-missingp)
  (assert (typep r 'varjo-compile-result))
  (labels ((raw->ast (id)
	     (let ((raw-id (slot-value id 'val)))
	       (or (assocr raw-id (slot-value r 'val-origins) :test #'=)
		   (when error-on-missingp
		     (error "Could not find origin for ~s" raw-id))))))
    (flatten (mapcar #'raw->ast (ids flow-id)))))
