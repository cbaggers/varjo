(in-package :varjo)

(defvar *v-debug* nil)

;-----------INSPIRATION-----------;

;; (define-condition machine-error (error)
;;   ((machine-name :initarg :machine-name :reader machine-error-machine-name))
;;   (:report (lambda (condition stream)
;;              (format stream "There is a problem with ~A."
;;                      (machine-error-machine-name condition)))))

;------------HELPER-----------;

;;[TODO] need better arg test
;; (defmacro deferror (name (&rest args) error-string &body body)
;;   (unless (every #'symbolp args) (error "can only take simple args"))
;;   (loop :for arg :in args :do
;;      (setf body (subst `(,arg condition) arg body :test #'eq)))
;;   `(define-condition ,name (varjo-error)
;;      (,@(loop :for arg :in args :collect
;;            `(,arg :initarg ,(kwd arg) :reader ,arg)))
;;      (:report (lambda (condition stream)
;;                 (declare (ignorable condition))
;;                 (format stream ,(format nil "Varjo: ~a" error-string) ,@body)))))

(defmacro deferror (name (&key (error-type 'varjo-error) prefix)
                            (&rest args) error-string &body body)
  (unless (every #'symbolp args) (error "can only take simple args"))
  (loop :for arg :in args :do
     (setf body (subst `(,arg condition) arg body :test #'eq)))
  `(define-condition ,name (,error-type)
     (,@(loop :for arg :in args :collect
           `(,arg :initarg ,(kwd arg) :reader ,arg)))
     (:report (lambda (condition stream)
                (declare (ignorable condition))
                (format stream ,(format nil "~@[~a:~] ~a" prefix error-string)
                        ,@body)))))

(define-condition varjo-error (error) ())
(define-condition varjo-critical-error (error) ())

;-----------------------------;

(define-condition missing-function-error (error)
  ((text :initarg :text :reader text)))

(deferror problem-with-the-compiler () (target)
  "This shouldnt have been possible so this needs a bug report. Sorry about that~%~s" target)

(deferror cannot-compile () (code)
  "Cannot compile the following code:~%~a" code)

(deferror no-function-returns () (name)
  "Function '~a' did not specify any return types" name)

(deferror not-core-type-error () (type-name)
  "Type ~a is not a core type and thus cannot end up in glsl src code
   It must end up being used or converted to something that resolves
   to a glsl type." type-name)

(deferror invalid-function-return-spec () (func spec)
    "Return type spec of function ~a is invalid:~%~a" func spec)

(deferror unknown-type-spec () (type-spec)
    "Could not find the correct type for type-spec ~a~@[ ~a~]~%~a"
  type-spec
  (when (typep type-spec 'v-type)
    (format nil "~%It seems we recieved a type object instead of type spec"))
  (let ((found (find-alternative-types-for-spec type-spec)))
    (if found
        (format nil "~%Perhaps you meant one of these types?:~%~(~{~s~%~}~)"
                found)
        "")))

(deferror duplicate-name () (name)
    "This name appears more than once in this form list ~a" name)

(deferror clone-global-env-error () ()
    "Cannot clone the global environment")

(deferror clean-global-env-error () ()
    "Cannot clean the global environment")

(deferror could-not-find-function () (name)
    "No function called '~a' was found in this environment" name)

(deferror no-valid-function () (name types)
    "There is no applicable method for the glsl function '~s'~%when called with argument types:~%~s " name types)

(deferror return-type-mismatch () (returns)
    "Some of the return statements return different types:~{~%~a~}"
  returns)

(deferror non-place-assign () (place val)
    "You cannot setf this: ~a ~%This was attempted as follows ~a"
  (current-line place)
  (gen-assignment-string place val))

(deferror setf-type-match () (code-obj-a code-obj-b)
    "Currently varjo cannot handle changing the type through a setf due to the static nature of glsl.~%place: ~a  value: ~a"
  (code-type code-obj-a) (code-type code-obj-b))

(deferror cannot-not-shadow-core () ()
    "You cannot shadow or replace core macros or special functions.")

(deferror out-var-name-taken () (out-var-name)
    "The variable name '~a' is already taken and so cannot be used~%for an out variable"
  out-var-name)

(deferror unknown-variable-type () (name)
    "Could not establish the type of the variable: ~s" name)

(deferror var-type-mismatch () (var-type code-obj)
    "Type specified does not match the type of the form~%~s~%~s"
  (code-type code-obj) var-type)

(deferror switch-type-error () (test-obj keys)
    "In a switch statement the result of the test and the keys must all be either ints or uints:~%Test type: ~a~%Keys used: ~{~a~^,~}" (code-type test-obj) keys)

(deferror loop-will-never-halt () (test-code test-obj)
    "The loop is using the following code as it's test.~%~a~%~%This will only ever result in a ~a which means the loop will never halt" test-code (type->type-spec (code-type test-obj)))

(deferror for-loop-simple-expression () ()
    "Only simple expressions are allowed in the condition and update slots of a for loop")

(deferror for-loop-only-one-var () ()
    "for loops can only iterate over one variable")

(deferror invalid-for-loop-type () (decl-obj)
    "Invalid type ~a used as counter for for-loop"
  (code-type decl-obj))

(deferror no-version-in-context () (env)
    "No supported version found in context:~%~a"
  (v-context env))

(deferror name-unsuitable () (name)
    "Names of variables and functions must start with an alpha char.~%They also may not start with 'gl-' or '-sc-' ~%Supplied Name: ~a~%" name)

(deferror unable-to-resolve-func-type () (func-name args)
    "Unable to resolve the result type of function '~a' when called~%with the argument types:~%~a~%" func-name (mapcar #'code-type args))

(deferror out-var-type-mismatch () (var-name var-types)
    "The out variable ~a is has been set with different types.~%Types used: ~a" var-name var-types)

(deferror fake-type-global () (env)
    "fake types can not be added to the global environment")

(deferror invalid-context-symbol () (context-symb)
    "Sorry but the symbol '~a' is not valid as a context specifier" context-symb)

(deferror args-incompatible () (previous-args current-args)
    "Sorry but the output arguments from one stage are not compatible with the input arguments of the next.~%Out vars from previous stage: ~a~%In args from this stage: ~a"
   previous-args current-args)

(deferror invalid-shader-stage () (stage)
    "Sorry but '~a' is not a valid shader stage" stage)

(deferror swizzle-keyword () (item)
    "Swizzle expects a keyword to specify the components. Recieved ~a instead" item)

(deferror multi-func-stemcells () (func-name)
    "Multiple functions found name ~a that match arguments.~%However varjo cannot decide which function to use because n of the arguments passed in are of stemcell type" func-name)

(deferror uniform-in-sfunc () (func-name)
    "Must not have uniforms in shader functions, only appropriate in shader stages: ~a"
  func-name)

(deferror invalid-v-defun-template () (func-name template)
    "Template passed to vdefun must be a format string : ~a~%~a~%"
  func-name template)

(deferror keyword-in-function-position () (form)
    "Keyword cannot appear in function name position : ~s"
  form)

(deferror invalid-symbol-macro-form () (name form)
    "Symbol macros must expand to a list or atom form : ~s -> ~s~%"
    name form)

(deferror stage-order-error () (stage-type)
    "stage of type ~s is not valid at this place in the pipeline, this is either out of order or a stage of this type already exists"
    stage-type)

(deferror multi-val-bind-mismatch () (bindings val-form)
    "Multiple Value Bind - Number of values returned from value form does not match bindings:
Bindings: ~a
Value Form: ~a"
    bindings val-form)

(deferror merge-env-func-scope-mismatch () (env-a env-b)
  "Attempting to merge two environements with different function scopes ~s~%~s~%~s"
  (cons (v-function-scope env-a) (v-function-scope env-b)) env-a env-b)

(deferror symbol-unidentified () (sym)
    "Symbol '~s' is unidentified." sym)

(deferror if-form-type-mismatch () (test-form then-form then-type
					      else-form else-type)
    "The result if ~a is true is ~a which has type ~a
however the false case returns ~a which has type ~a
This is incompatible"
    test-form then-form then-type else-form else-type)

(deferror bad-make-function-args () (func-name arg-specs)
    "Trying to define the function ~s but the following argument specifications
are a bit odd:
~{~s~^~%~}

Generally arguments are defined in the format (arg-name arg-type)
e.g. (~a :vec3)"
    func-name arg-specs
    ;; this bit below is so that, where possible, the error uses one of
    ;; your arg names in the example of a valid arg spec.
    ;; I hope this makes the error message a bit more relevent and approachable
    (let* ((potential-spec (first arg-specs))
           (potential-name (cond
                             ((listp potential-spec) (first potential-spec))
                             ((and (symbolp potential-spec)
                                   (not (null potential-spec))
                                   (not (keywordp potential-spec)))
                              potential-spec))))
      (if (and potential-name
               (symbolp potential-name)
               (not (keywordp potential-name)))
          (string-downcase (symbol-name potential-name))
          "x")))

(deferror none-type-in-out-vars () (glsl-name)
    "One of the values being returned from the shader (~s) is of type :none."
    glsl-name)

(deferror body-block-empty () (form-name)
    "In varjo it is not valid to have a ~s with an empty body."
    form-name)

(deferror flow-id-must-be-specified-vv (:error-type varjo-critical-error) ()
    "v-values must be given a flow id when created")

(deferror flow-id-must-be-specified-co (:error-type varjo-critical-error) ()
    "code objects must be given a flow id when created")

(deferror multiple-flow-ids-regular-func (:error-type varjo-critical-error)
    (func-name func)
    "the function ~s is a regular function but has multiple flow ids.
this is an error as only functions with multiple returns are allowed
multiple flow-ids.
This is a compiler bug

~s" func-name func)
