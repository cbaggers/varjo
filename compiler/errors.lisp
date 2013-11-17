(in-package :varjo)

;-----------EXAMPLE-----------;
(define-condition machine-error (error)
  ((machine-name :initarg :machine-name :reader machine-error-machine-name))
  (:report (lambda (condition stream)
             (format stream "There is a problem with ~A."
                     (machine-error-machine-name condition)))))
;------------HELPER-----------;

;;[TODO] need better arg test
(defmacro deferror (name (&rest args) error-string &body body)
  (unless (every #'symbolp args) (error "can only take simple args"))
  (loop :for arg :in args :do
     (setf body (subst `(,arg condition) arg body :test #'eq)))
  `(define-condition ,name (error)
     (,@(loop :for arg :in args :collect
           `(,arg :initarg ,(kwd arg) :reader ,arg)))
     (:report (lambda (condition stream)
                (declare (ignorable condition))
                (format stream ,error-string ,@body)))))

;-----------------------------;

(define-condition missing-function-error (error)
  ((text :initarg :text :reader text)))

(deferror problem-with-the-compiler (target)
  "This shouldnt have been possible so this needs a bug report. Sorry about that~%~s" target)

(deferror cannot-compile (code)
  "Cannot compile the following code:~%~a" code)

(deferror no-function-returns (name)
  "Function '~a' did not specify any return types" name)

(deferror not-core-type-error (type-name)
  "Type ~a is not a core type and thus cannot end up in glsl src code
   It must end up being used or converted to something that resolves 
   to a glsl type." type-name)

(deferror invalid-function-return-spec (func spec)
    "Return type spec of function ~a is invalid:~%~a" func spec)

(deferror unknown-type-spec (type-spec)
    "Unknown specification for type: ~a" type-spec)

(deferror duplicate-name (name)
    "This name appears more than once in this form list ~a" name)

(deferror clone-global-env-error ()
    "Cannot clone the global environment")

(deferror could-not-find-function (name)
    "No function called '~a' was found in this environment" name)

(deferror no-valid-function (name types)
    "There is no applicable method for the glsl function '~s'~%when called with argument types:~%~s " name types)

(deferror return-type-mismatch (name types returns)
    "Some of the return statements in function '~a' return different types~%~a~%~a" 
  name types returns)

(deferror non-place-assign (place val)
    "You cannot setf this: ~a ~%This was attempted as follows ~a"
  (current-line place)
  (gen-assignment-string place val))

(deferror cannot-not-shadow-core ()
    "You cannot shadow or replace core macros or special functions.")

(deferror out-var-name-taken (out-var-name)
    "The variable name '~a' is already taken and so cannot be used~%for an out variable" 
  out-var-name)

(deferror loop-will-never-halt (test-code test-obj)
    "The loop is using the following code as it's test.~%~a~%~%This will only ever result in a ~a which means the loop will never halt" test-code (v-type-name (code-type test-obj)))

;-----------------------------;

