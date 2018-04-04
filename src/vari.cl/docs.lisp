(in-package :vari.cl)

(defvar *vari-additional-form-docs*
  (make-hash-table :test #'eq))

(defun janky-parse-package (symbol/string)
  (etypecase symbol/string
    (null nil)
    (symbol (find-package (symbol-name symbol/string)))
    (string (or (find-package symbol/string)
                (find-package (string-upcase symbol/string))
                (let* ((name (string symbol/string))
                       (pos (position #\: name :test #'char=)))
                  (when (or (eql pos 0)
                            (eql pos 1))
                    (let ((name (subseq name (1+ pos))))
                      (or (find-package name)
                          (find-package (string-upcase name))))))))))

(defun janky-parse-name (str try-package)
  (flet ((inner (str package)
           (let* ((pos (position #\: str :test #'char=)))
             (cond
               ((null pos) (find-symbol str package))
               ((= pos 0) (find-symbol (subseq str 1) :keyword))
               (t
                (let ((pkg (find-package (subseq str 0 pos))))
                  (when pkg
                    (find-symbol (subseq str (1+ pos)) pkg))))))))
    ;;
    (let ((package (or (janky-parse-package try-package)
                       *package*)))
      (values
       (or (inner str package)
           (inner (string-upcase str) package)
           (let ((package (find-package "GLSL-SYMBOLS")))
             (or (inner str package)
                 (inner (string-upcase str) package))))))))

(defun form-binding-signature (binding)
  (labels ((aesthetic-pair (pair)
             (format nil "(~a ~s)"
                     (first pair)
                     (second pair))))
    (handler-case
        (etypecase binding
          (external-function
           (format nil "(~a ~{~a~^ ~})"
                   (name binding)
                   (append (mapcar #'aesthetic-pair (in-args binding))
                           (when (uniforms binding)
                             (cons '&uniform (mapcar #'aesthetic-pair
                                                     (uniforms binding)))))))
          (v-function
           (format nil "(~a ~{~s~^ ~})"
                   (name binding)
                   (mapcar #'type->type-spec (v-argument-spec binding))))
          (v-regular-macro (arguments binding))
          (v-compiler-macro (arguments binding)))
      (error () nil))))

(defun vari-describe (name &optional (stream *standard-output*)
                             try-package-name)
  (labels ((get-overload-pairs (form-set)
             (if (typep form-set 'v-function-set)
                 (loop
                    :for binding :in (functions form-set)
                    :for sig := (form-binding-signature binding)
                    :when sig :collect (cons binding sig))
                 (form-binding-signature form-set)))
           (format-glsl-func-doc (glsl-doc)
             (let* ((decl (search "Declaration" glsl-doc))
                    (param (search "Parameter" glsl-doc))
                    (see-also (search "See Also" glsl-doc))
                    (copyright (search "Copyright" glsl-doc)))
               (cond
                 ((and decl param see-also copyright)
                  (format nil "Official GLSL Documentaion:~%~%~a~a~a"
                          (subseq glsl-doc 0 decl)
                          (subseq glsl-doc param see-also)
                          (subseq glsl-doc copyright)))
                 ((and decl param)
                  (format nil "Official GLSL Documentaion:~%~%~a~a"
                          (subseq glsl-doc 0 decl)
                          (subseq glsl-doc param)))
                 (t (format nil "Official GLSL Documentaion:~%~%~a" glsl-doc)))))
           (format-glsl-var-doc (glsl-doc)
             (let* ((decl (search "Declaration" glsl-doc))
                    (desc (search "Description" glsl-doc))
                    (see-also (search "See Also" glsl-doc))
                    (copyright (search "Copyright" glsl-doc)))
               (cond
                 ((and decl desc see-also copyright)
                  (format nil "Official GLSL Documentaion:~%~%~a~a~a"
                          (subseq glsl-doc 0 decl)
                          (subseq glsl-doc desc see-also)
                          (subseq glsl-doc copyright)))
                 ((and decl desc)
                  (format nil "Official GLSL Documentaion:~%~%~a~a"
                          (subseq glsl-doc 0 decl)
                          (subseq glsl-doc desc)))
                 (t (format nil "Official GLSL Documentaion:~%~%~a" glsl-doc)))))
           (try-get-var-type (name)
             (let ((spec
                    (loop :for (nil . vars) :in varjo.internals::*glsl-variables*
                       :for match := (find name vars :key #'first)
                       :when match :return (third match))))
               (varjo.internals::alternate-name-for spec)))
           (try-get-ext-func-docs (overload-pairs)
             (loop :for (func . sig) :in overload-pairs
                :when (and (typep func 'external-function)
                           (v-doc-string func))
                :collect (format nil "~%Overload: ~a~%~a" sig (v-doc-string func)))))

    (let* ((name (if (stringp name)
                     (janky-parse-name name try-package-name)
                     name))
           (var (gethash name glsl-docs:*variables*))
           (form-binding-set
            (unless (or var (string= name :declare))
              (varjo.internals::find-global-form-binding-by-literal name t))))
      (cond
        (var
         (format stream
                 "~a~%~%~@[Type:~%~%~s~%~%~]~a"
                 name
                 (try-get-var-type name)
                 (format-glsl-var-doc var)))
        (form-binding-set
         (let* ((glsl-doc (gethash name glsl-docs:*functions*))
                (v-doc (gethash name *vari-additional-form-docs*))
                (overload-pairs (get-overload-pairs form-binding-set))
                (overload-docs (unless (or glsl-doc v-doc)
                                 (try-get-ext-func-docs overload-pairs)))
                (doc (cond
                       (v-doc v-doc)
                       (glsl-doc
                        (format-glsl-func-doc glsl-doc)))))
           (format stream "~a~%~%~@[Overloads:~%~{~a~%~}~]~@[~%~a~]~@[~{~%Overload Docs:~%~a~}~]"
                   name
                   (mapcar #'cdr overload-pairs)
                   doc
                   overload-docs)))))))

(setf (gethash '* *vari-additional-form-docs*)
      "Return the product of its arguments. With no args, returns 1.")

(setf (gethash '+ *vari-additional-form-docs*)
      "Return the sum of its arguments. With no args, returns 0.")

(setf (gethash '- *vari-additional-form-docs*)
      "Subtract the second and all subsequent arguments from the first;
  or with one argument, negate the first argument.")

(setf (gethash '/ *vari-additional-form-docs*)
      "Divide the first argument by each of the following arguments, in turn.
  With one argument, return reciprocal.")

(setf (gethash '/= *vari-additional-form-docs*)
      "Return T if no two of its arguments are numerically equal, NIL otherwise.")

(setf (gethash '1+ *vari-additional-form-docs*)
      "Return NUMBER + 1.")

(setf (gethash '1- *vari-additional-form-docs*)
      "Return NUMBER - 1.")

(setf (gethash '< *vari-additional-form-docs*)
      "Return T if its arguments are in strictly increasing order, NIL otherwise.")

(setf (gethash '<= *vari-additional-form-docs*)
      "Return T if arguments are in strictly non-decreasing order, NIL otherwise.")

(setf (gethash '= *vari-additional-form-docs*)
      "Return T if all of its arguments are numerically equal, NIL otherwise.")

(setf (gethash '> *vari-additional-form-docs*)
      "Return T if its arguments are in strictly decreasing order, NIL otherwise.")

(setf (gethash '>= *vari-additional-form-docs*)
      "Return T if arguments are in strictly non-increasing order, NIL otherwise.")

(setf (gethash 'adjustable-array-p *vari-additional-form-docs*)
      "Returns T is the array is adjustable, always returns NIL in Vari")

(setf (gethash 'aref *vari-additional-form-docs*)
      "Return the element of the ARRAY specified by the SUBSCRIPT.")

(setf (gethash 'array-has-fill-pointer-p *vari-additional-form-docs*)
      "Return T if the given ARRAY has a fill pointer, or NIL otherwise. Always returns NIL in Vari")

(setf (gethash 'array-rank *vari-additional-form-docs*)
      "Return the number of dimensions of ARRAY. Always returns 1 for Vari arrays as GLSL doesnt support
multi-dimensional arrays.")

(setf (gethash 'array-total-size *vari-additional-form-docs*)
      "Return the total number of elements in the Array.")

(setf (gethash 'arrayp *vari-additional-form-docs*)
      "Return true if OBJECT is an ARRAY, and NIL otherwise.")

(setf (gethash 'break *vari-additional-form-docs*)
      "break terminates the execution of the nearest enclosing for, switch, or while")

(setf (gethash 'case *vari-additional-form-docs*)
      "CASE Keyform {(Key Form*)}*
Evaluates the Forms in the first clause with a Key EQL to the value of
Keyform. If a singleton key is T then the clause is a default clause.

Currently all Keys must be constantp")

(setf (gethash 'coerce *vari-additional-form-docs*)
      "Coerce the Object to an object of type Output-Type-Spec.
Output-Type-Spec should not be quoted")

(setf (gethash 'compiled-function-p *vari-additional-form-docs*)
      "Return true if OBJECT is a COMPILED-FUNCTION, and NIL otherwise.
All functions in Vari are compiled so this will return T for all functions")

(setf (gethash 'complement *vari-additional-form-docs*)
      "Return a new function that returns T whenever FUNCTION returns NIL and
NIL whenever FUNCTION returns non-NIL.")

(setf (gethash 'complex *vari-additional-form-docs*)
      "Return a complex number with the specified real and imaginary components.")

(setf (gethash 'complexp *vari-additional-form-docs*)
      "Return true if OBJECT is a COMPLEX, and NIL otherwise.")

(setf (gethash 'conjugate *vari-additional-form-docs*)
      "Return the complex conjugate of NUMBER. For non-complex numbers, this is
  an identity.")

(setf (gethash 'continue *vari-additional-form-docs*)
      "The continue statement passes control to the next iteration of the for or
while statement in which it appears, bypassing any remaining statements.")

(setf (gethash 'DECF *vari-additional-form-docs*)
      "The first argument is some location holding a number. This number is
  decremented by the second argument, DELTA, which defaults to 1.")

(setf (gethash 'DENOMINATOR *vari-additional-form-docs*)
      "Return the denominator of NUMBER, which must be rational.")

(setf (gethash 'DO *vari-additional-form-docs*)
      "DO ({(Var Init Step)}+) (Test Exit-Form+) Declaration* Form*
  Iteration construct. On subsequent iterations, the Vars are assigned the
  value of the Step form in parallel. The Test is evaluated before
  each evaluation of the body Forms. When the Test is true, the Exit-Forms
  are evaluated as a PROGN, with the result being the value of the DO.")

(setf (gethash 'EVENP *vari-additional-form-docs*)
      "Is this integer even?")

(setf (gethash 'EXPT *vari-additional-form-docs*)
      "Return BASE raised to the POWER.")

(setf (gethash 'FLET *vari-additional-form-docs*)
      "FLET ({(name lambda-list declaration* form*)}*) declaration* body-form*

Evaluate the BODY-FORMS with local function definitions. The bindings do
not enclose the definitions; any use of NAME in the FORMS will refer to the
lexically apparent function definition in the enclosing environment.")

(setf (gethash 'FLOAT *vari-additional-form-docs*)
      "Converts any REAL to a float. If OTHER is not provided, it returns a
  SINGLE-FLOAT if NUMBER is not already a FLOAT. If OTHER is provided, the
  result is the same float format as OTHER.")

(setf (gethash 'FLOAT-RADIX *vari-additional-form-docs*)

      "Return (as an integer) the radix b of its floating-point argument.")

(setf (gethash 'FLOAT-SIGN *vari-additional-form-docs*)
      "Return a floating-point number that has the same sign as
   FLOAT1 and, if FLOAT2 is given, has the same absolute value
   as FLOAT2.")

(setf (gethash 'FLOATP *vari-additional-form-docs*)
      "Return true if OBJECT is a FLOAT, and NIL otherwise.")


(setf (gethash 'FUNCTION *vari-additional-form-docs*)
      "FUNCTION name

Return the lexically apparent definition of the function NAME. If NAME is
a symbol the result will include all possible overloads of the function. To
specify a single function use the full signatures e.g. #'(sin :float)

NAME may also be a lambda expression.")

(setf (gethash 'FUNCTIONP *vari-additional-form-docs*)
      "Return true if OBJECT is a FUNCTION, and NIL otherwise.")

(setf (gethash 'IDENTITY *vari-additional-form-docs*)
      "This function simply returns what was passed to it.")

(setf (gethash 'IF *vari-additional-form-docs*)
      "IF predicate then [else]

If PREDICATE evaluates to true, evaluate THEN and return its values,
otherwise evaluate ELSE and return its values. ELSE defaults to NIL.

If the branches evaluate to values of different type then the type
of the result will be (or type-from-then-branch type-from-else-branch)
This will cause issues if you attempt to return this value from a function
or pass it as an argument but is harmless in the non-tail position of a progn.

If `discard` is called in one branch then the result type of the IF is the
type of the branch that didnt discard")

(setf (gethash 'IMAGPART *vari-additional-form-docs*)
      "Extract the imaginary part of a number.")

(setf (gethash 'INCF *vari-additional-form-docs*)
      "The first argument is some location holding a number. This number is
  incremented by the second argument, DELTA, which defaults to 1.")

(setf (gethash 'INTEGER-LENGTH *vari-additional-form-docs*)

      "Return the number of non-sign bits in the twos-complement representation
  of INTEGER.")

(setf (gethash 'INTEGERP *vari-additional-form-docs*)
      "Return true if OBJECT is an INTEGER, and NIL otherwise.")

(setf (gethash 'ISQRT *vari-additional-form-docs*)

      "Return the greatest integer less than or equal to the square root of N.")

(setf (gethash 'KEYWORDP *vari-additional-form-docs*)

      "Return true if Object is a symbol in the \"KEYWORD\" package. Always NIL
in Vari")

(setf (gethash 'LABELS *vari-additional-form-docs*)

      "LABELS ({(name lambda-list declaration* form*)}*) declaration* body-form*

Evaluate the BODY-FORMS with local function definitions. The bindings enclose
each subsequent definitions, so the defined functions can call the functions
defined before them in the definitions list.

Recursion (direct or indirect) is illegal in GLSL")

(setf (gethash 'LET *vari-additional-form-docs*)

      "LET ({(var [value]) | var}*) declaration* form*

During evaluation of the FORMS, bind the VARS to the result of evaluating the
VALUE forms. The variables are bound in parallel after all of the VALUES forms
have been evaluated.")

(setf (gethash 'LET* *vari-additional-form-docs*)

      "LET* ({(var [value]) | var}*) declaration* form*

Similar to LET, but the variables are bound sequentially, allowing each VALUE
form to reference any of the previous VARS.")

(setf (gethash 'LOCALLY *vari-additional-form-docs*)

      "LOCALLY declaration* form*

Sequentially evaluate the FORMS in a lexical environment where the
DECLARATIONS have effect. If LOCALLY is a top level form, then the FORMS are
also processed as top level forms.")

(setf (gethash 'LOGAND *vari-additional-form-docs*)
      "Return the bit-wise and of its arguments.")
(setf (gethash 'LOGCOUNT *vari-additional-form-docs*)
      "Count the number of 1 bits if INTEGER is non-negative,
and the number of 0 bits if INTEGER is negative.")
(setf (gethash 'LOGIOR *vari-additional-form-docs*)
      "Return the bit-wise or of its arguments")
(setf (gethash 'LOGNOT *vari-additional-form-docs*)
      "Return the bit-wise logical not of the input")
(setf (gethash 'LOGXOR *vari-additional-form-docs*)

      "Return the bit-wise exclusive or of its arguments.")
(setf (gethash 'MACROLET *vari-additional-form-docs*)
      "MACROLET ({(name lambda-list form*)}*) body-form*

Evaluate the BODY-FORMS in an environment with the specified local macros
defined. NAME is the local macro name, LAMBDA-LIST is a DEFMACRO style
destructuring lambda list, and the FORMS evaluate to the expansion.")
(setf (gethash 'MINUSP *vari-additional-form-docs*)
      "Is this real number strictly negative?")
(setf (gethash 'MULTIPLE-VALUE-CALL *vari-additional-form-docs*)
      "MULTIPLE-VALUE-CALL function values-form*

Call FUNCTION, passing all the values of each VALUES-FORM as arguments,
values from the first VALUES-FORM making up the first argument, etc.")
(setf (gethash 'MULTIPLE-VALUE-PROG1 *vari-additional-form-docs*)
      "MULTIPLE-VALUE-PROG1 values-form form*

Evaluate VALUES-FORM and then the FORMS, but return all the values of
VALUES-FORM.")
(setf (gethash 'NTH-VALUE *vari-additional-form-docs*)
      "Evaluate FORM and return the Nth value (zero based)
 without consing a temporary list of values.")
(setf (gethash 'NUMBERP *vari-additional-form-docs*)
      "Return true if OBJECT is a NUMBER, and NIL otherwise.")
(setf (gethash 'NUMERATOR *vari-additional-form-docs*)
      "Return the numerator of NUMBER, which must be rational.")
(setf (gethash 'ODDP *vari-additional-form-docs*)
      "Is this integer odd?")
(setf (gethash 'PLUSP *vari-additional-form-docs*)
      "Is this real number strictly positive?")
(setf (gethash 'PROGN *vari-additional-form-docs*)

      "PROGN form*

Evaluates each FORM in order, returning the values of the last form.

It is not legal to have an empty PROGN in Vari")
(setf (gethash 'RATIONALP *vari-additional-form-docs*)
      "Return true if OBJECT is a RATIONAL, and NIL otherwise.")
(setf (gethash 'REALP *vari-additional-form-docs*)
      "Return true if OBJECT is a REAL, and NIL otherwise.")
(setf (gethash 'REALPART *vari-additional-form-docs*)
      "Extract the real part of a number.")
(setf (gethash 'ROW-MAJOR-AREF *vari-additional-form-docs*)

      "Return the element of array corresponding to the row-major index. This is
   SETFable.")
(setf (gethash 'SETF *vari-additional-form-docs*)
      "Takes pairs of arguments like SETQ. The first is a place and the second
  is the value that is supposed to go into that place. Returns the last
  value. The place argument may be any of the access forms for which SETF
  knows a corresponding setting form.")
(setf (gethash 'SIGNUM *vari-additional-form-docs*)

      "If NUMBER is zero, return NUMBER, else return (/ NUMBER (ABS NUMBER)).")
(setf (gethash 'SIMPLE-BIT-VECTOR-P *vari-additional-form-docs*)

      "Return true if OBJECT is a SIMPLE-BIT-VECTOR, and NIL otherwise.
Always NIL in Vari")
(setf (gethash 'SIMPLE-VECTOR-P *vari-additional-form-docs*)

      "Return true if OBJECT is a SIMPLE-VECTOR, and NIL otherwise.
All arrays are simple-vectors in Vari")
(setf (gethash 'SVREF *vari-additional-form-docs*)
      "Return the INDEXth element of the given Simple-Vector.")
(setf (gethash 'SYMBOL-MACROLET *vari-additional-form-docs*)
      "SYMBOL-MACROLET ({(name expansion)}*) decl* form*

Define the NAMES as symbol macros with the given EXPANSIONS. Within the
body, references to a NAME will effectively be replaced with the EXPANSION."
      )
(setf (gethash 'SYMBOLP *vari-additional-form-docs*)
      "Return true if OBJECT is a SYMBOL, and NIL otherwise.
Always NIL in Vari")

(setf (gethash 'THE *vari-additional-form-docs*)
      "Specifies that the values returned by FORM conform to the VALUE-TYPE.

This becomes a compile-time assertion of the type, except in cases of global
variable capture where it can be used to inform the compiler of the type.")

(setf (gethash 'TYPECASE *vari-additional-form-docs*)
      "TYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true.")
(setf (gethash 'UNLESS *vari-additional-form-docs*)
      "If the first argument is not true, the rest of the forms are
evaluated as a PROGN.")

(setf (gethash 'VALUES *vari-additional-form-docs*)
      "Return all arguments, in order, as values.

You may also qualify the values. If the value is a list where the first symbol
is a qualifier-form then the last form is the value and the butlast forms are
treated as qualifiers.

For example:

    (values (v! 1 2 3 4)  ←[0]
            (:flat 10) ←[1]
            ((:feedback 1) (+ position (v! pos 0 0))) ←[2]
            ((:feedback 0) (v! 0.1 0 1)))             ←[2]

- [0] unqualified value
- [1] the value 10 is qualified as flat
- [2] two forms are qualfied as being the transform feedback of the stage
")
(setf (gethash 'VECTOR *vari-additional-form-docs*)
      "Construct a SIMPLE-VECTOR from the given objects.")
(setf (gethash 'VECTORP *vari-additional-form-docs*)
      "Return true if OBJECT is a VECTOR, and NIL otherwise.")
(setf (gethash 'WHEN *vari-additional-form-docs*)
      "If the first argument is true, the rest of the forms are
evaluated as a PROGN.")
(setf (gethash 'ZEROP *vari-additional-form-docs*)
      "Is this number zero?")
(setf (gethash 'AND *vari-additional-form-docs*)
 "The macro and evaluates each form one at a time from left to right.
As soon as any form evaluates to nil, and returns nil without evaluating the
remaining forms. If all forms but the last evaluate to true values, and returns
the results produced by evaluating the last form. ")

(setf (gethash 'ARRAY-ROW-MAJOR-INDEX *vari-additional-form-docs*)

 "Computes the position according to the row-major ordering of array for the
element that is specified by subscripts, and returns the offset of the element
in the computed position from the beginning of array.

For a one-dimensional array, the result of array-row-major-index equals
subscript.")

(setf (gethash 'COND *vari-additional-form-docs*)

  "Syntax:

cond {clause}+ => result*

clause::= (test-form form+)

Arguments and Values:

test-form---a form.

forms---an implicit progn.

results---the values of the forms in the first clause whose test-form yields
          true, or the primary value of the test-form if there are no forms in
          that clause, or else void if no test-form yields true.

Description:

cond allows the execution of forms to be dependent on test-form.

Test-forms are evaluated one at a time in the order in which they are given in
the argument list until a test-form is found that evaluates to true.

If there are no forms in that clause, the primary value of the test-form is
returned by the cond form. Otherwise, the forms associated with this test-form
are evaluated in order, left to right, as an implicit progn, and the values
returned by the last form are returned by the cond form.

Once one test-form has yielded true, no additional test-forms are evaluated.
If no test-form yields true, the COND evaluates to void.")

(setf (gethash 'DOTIMES *vari-additional-form-docs*)

    "Syntax:

dotimes (var count-form) declaration* {statement}+

=> void

Arguments and Values:

var---a symbol.

count-form---a form.

declaration---a declare expression; not evaluated.

statement---a compound form;

Description:

dotimes iterates over a series of integers.")

(setf (gethash 'EQL *vari-additional-form-docs*)

 "Returns T is both arguments are numbers with the same value

GLSL does not provide a equivalent of EQ.")

(setf (gethash 'EQUAL *vari-additional-form-docs*)

 "Returns T is both arguments are numbers with the same value, or in the case
of aggregates, if the are component-wise EQUAL.

EQUAL does not work on GLSL's opaque types")

(setf (gethash 'FLOAT-DIGITS *vari-additional-form-docs*)

 "Returns the number of radix b digits used in the representation of float (including
any implicit digits, such as a ``hidden bit''). ")

(setf (gethash 'LAMBDA *vari-additional-form-docs*)

    "lambda lambda-list [[declaration* | documentation]] form+ => function

Arguments and Values:

lambda-list---an ordinary lambda list.

declaration---a declare expression; not evaluated.

documentation---a string; not evaluated.

form---a form.

function---a function.

Description:

Provides a shorthand notation for a function special form involving a lambda expression such that:

    (lambda lambda-list [[declaration* | documentation]] form*)
 ==  (function (lambda lambda-list [[declaration* | documentation]] form*))
 ==  #'(lambda lambda-list [[declaration* | documentation]] form*)
")

(setf (gethash 'MAKE-ARRAY *vari-additional-form-docs*)

 "WARNING: API of MAKE-ARRAY subject to change.

Description:

Creates and returns an array.

Dimensions represents the dimensionality of the new array. The array must
be single-dimensional and dimensions must be a number.

element-type indicates the type of the elements intended to be stored in the
new-array

If initial-element is supplied, it is used to initialize each element of
new-array. If initial-element is supplied, it must be of the type given by
element-type. initial-element must be constantp

initial-contents is used to initialize the contents of array. It must currently
be a quoted list, this is an issue and will be changed.")

(setf (gethash 'MULTIPLE-VALUE-BIND *vari-additional-form-docs*)

      "Creates new variable bindings for the vars and executes a series of forms that use
these bindings.

The variable bindings created are lexical.

Values-form is evaluated, and each of the vars is bound to the respective value
returned by that form. The number of values returns by values-form must match
the number of forms in 'vars'. The vars are bound to the values over the
execution of the forms, which make up an implicit progn.

The scopes of the name binding and declarations do not include the values-form.")

(setf (gethash 'MULTIPLE-VALUE-SETQ *vari-additional-form-docs*)

    "multiple-value-setq assigns values to vars.

The form is evaluated, and each var is assigned to the corresponding value
returned by that form. The number of values returns by values-form must match
the number of forms in 'vars'

If any var is the name of a symbol macro, then it is assigned as if by setf.")

(setf (gethash 'OR *vari-additional-form-docs*)

 "or evaluates each form, one at a time, from left to right. The evaluation of
all forms terminates when a form evaluates to true (i.e., something other than
nil).")

(setf (gethash 'PROG1 *vari-additional-form-docs*)

    "prog1 evaluates first-form and then forms, yielding as its only value the primary
value yielded by first-form." )

(setf (gethash 'PROG2 *vari-additional-form-docs*)

    "prog2 evaluates first-form, then second-form, and then forms, yielding as
its only value the primary value yielded by second-form.")

(setf (gethash 'RANDOM-STATE-P *vari-additional-form-docs*)

 "Returns true if object is of type random-state; otherwise, returns false.")

(setf (gethash 'RETURN *vari-additional-form-docs*)
  "return terminates the execution of a function and returns control and values to the calling
function. The types from all return forms in a stage must be idential.")

(setf (gethash 'SETQ *vari-additional-form-docs*)
 "Assigns a value to a variable.

    (setq var1 form1)

is the simple variable assignment statement of Lisp.
First form is evaluated and then the result is stored in the variable var1.

If any var refers to a binding made by symbol-macrolet, then that var is
reated as if setf (not setq) had been used.")

(setf (gethash 'SLOT-VALUE *vari-additional-form-docs*)
 "The function slot-value returns the value of the slot named slot-name in the struct.

`setf` can be used with slot-value to change the value of a slot.")

(setf (gethash 'WITH-ACCESSORS *vari-additional-form-docs*)
      "Creates a lexical environment in which the slots specified by slot-entry are
lexically available through their accessors as if they were variables.

The macro with-accessors invokes the appropriate accessors to access the slots
specified by slot-entry. Both setf and setq can be used to set the value of the slot.")

(setf (gethash 'WITH-SLOTS *vari-additional-form-docs*)
      "The macro with-slots establishes a lexical environment for referring to the slots in
the struct named by the given slot-names as though they were variables.

Within such a context the value of the slot can be specified by using its
slot name, as if it were a lexically bound variable.

Both setf and setq can be used to set the value of the slot.

The macro with-slots translates an appearance of the slot name as a variable
into a call to slot-value. ")
