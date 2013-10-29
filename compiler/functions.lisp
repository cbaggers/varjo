(in-package :varjo)

;;------------------------------------------------------------
;; GLSL Functions
;;----------------

(defun v-make-f-spec (transform args arg-types return-spec &key place)
  (let* ((context-pos (position '&context args :test #'symbol-name-equal))
         (context (when context-pos (subseq args (1+ context-pos))))
         (args (if context-pos (subseq args 0 context-pos) args)))
    (declare (ignore args))
    `(,transform ,arg-types ,return-spec ,context ,place)))

;; 3 kinds of function
;;  wrapper - first line is a string template for a glsl function
;;            the rest defines the params. This is stored as list spec
;;            in the global environment
;;  special - The arg check and type resolution is implemented in lisp
;; :inject - The function is implemented in varjo lisp to be injected 
;;           into the body if it is used by a shader
(defmacro v-defun (name args &body body)
  (let* ((context-pos (position '&context args :test #'symbol-name-equal))
         (context (when context-pos (subseq args (1+ context-pos))))
         (args (if context-pos (subseq args 0 context-pos) args)))
    (declare (ignore args))
    (cond ((stringp (first body))
           (destructuring-bind (transform arg-types return-spec &key place) body
             `(setf (gethash ',name (v-functions *global-env*)) 
                    '(,transform ,arg-types ,return-spec ,context ,place))))
          ((eq (first body) :inject) `(error "injected function not implemented"))
          (t (destructuring-bind (&key args return) body
                 `(setf (gethash ',name (v-functions *global-env*)) 
                        '(:special ,args ,return ,context nil)))))))

;;------------------------------------------------------------

(defun find-function-for-args (func-name args env)
  (let ((arg-len (length args)))
    (loop :for func :in (get-function func-name env)
       :if (and (valid-for-contextp func env)
                (or (and (v-special-functionp func)
                         (funcall (v-argument-spec func) args))
                    (and (eql arg-len (length (v-argument-spec func)))
                         (loop :for arg :in args 
                            :for arg-type :in (v-argument-spec func)
                            :always (v-casts-to-p (code-type arg) arg-type)))))
       :collect func)))

(defun glsl-resolve-func-type (func args)
  "nil - superior type
   number - type of nth arg
   function - call the function
   (:element n) - element type of nth arg
   list - type spec"
  (let ((spec (v-return-spec func))
        (arg-types (mapcar #'code-type args)))
    (cond ((null spec) (find-mutual-cast-type arg-types))
          ((numberp spec) (nth spec arg-types))
          ((functionp spec) (apply spec args))
          ((and (listp spec) (eq (first spec) :element))
           (v-element-type (nth (second spec) arg-types)))
          ((or (symbolp spec) (listp spec)) (type-spec->type spec))
          (t (error 'invalid-function-return-spec :func func :spec spec)))))

;;------------------------------------------------------------

(defun context-ok-given-restriction (context restriction)
  (every #'identity
         (loop :for item :in restriction :collect
            (if (listp item)
                (some #'identity (loop :for sub-item :in item :collect
                                    (find sub-item context)))
                (find item context)))))

(defmethod valid-for-contextp ((func function) (env environment))
  (let ((restriction (v-restriction func))
        (context (v-context env)))
    (if restriction
        (when (context-ok-given-restriction context restriction) func)
        func)))

;;------------------------------------------------------------
