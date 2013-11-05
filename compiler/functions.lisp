(in-package :varjo)

;;------------------------------------------------------------
;; GLSL Functions
;;----------------

(defun v-make-f-spec (transform args arg-types return-spec 
                      &key place glsl-spec-matching)
  (let* ((context-pos (position '&context args :test #'symbol-name-equal))
         (context (when context-pos (subseq args (1+ context-pos))))
         (args (if context-pos (subseq args 0 context-pos) args)))
    (declare (ignore args))
    `(,transform ,arg-types ,return-spec ,context ,place ,glsl-spec-matching)))
;;
;; {IMPORTANT NOTE} IF YOU CHANGE ONE-^^^^, CHANGE THE OTHER-vvvvv
;;
(defmacro v-defun (name args &body body)
  (let* ((context-pos (position '&context args :test #'symbol-name-equal))
         (context (when context-pos (subseq args (1+ context-pos))))
         (args (if context-pos (subseq args 0 context-pos) args)))
    (cond ((stringp (first body))
           (destructuring-bind (transform arg-types return-spec 
                                          &key place glsl-spec-matching) body
             `(progn (add-function ',name '(,transform ,arg-types ,return-spec
                                            ,context ,place ,glsl-spec-matching)
                                   *global-env*)
                     ',name)))
          ((eq (first body) :inject)
           `(progn (setf (gethash ',name (v-external-functions *global-env*))
                         '(,args ,@(rest body)))
                   ',name))
          (t (destructuring-bind (&key args return) body
               `(progn (setf (gethash ',name (v-functions *global-env*)) 
                             '(:special ,args ,return ,context nil))
                       ',name))))))

;;------------------------------------------------------------

;;[TODO] Need to handle fake-structs
;;[TODO] if type specifiers are same in in-args then the args passed in must 
;;       be compatible (maybe only if :spec-match is true
;;[TODO] for special funcs this can involve walking the rest of a tree, we dont 
;;       want to do this twice
;;[TODO] We need to prioritise matches that dont require casting
;;[TODO] We need to collect the new type (in case of casting) and the number of
;;       casts[?] or some priority of results
(defun find-function-for-args (func-name args env)
  (let ((arg-len (length args)))
    (loop :for func :in (get-function func-name env)
       :if (and (valid-for-contextp func env)
                (or (and (v-special-functionp func)
                         (funcall (v-argument-spec func) args))
                    (and (eql arg-len (length (v-argument-spec func)))
                         (and (loop :for arg :in args 
                                 :for arg-type :in (v-argument-spec func)
                                 :always (v-casts-to-p (code-type arg) arg-type))
                              (multiple-type-equivilent )))))
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
