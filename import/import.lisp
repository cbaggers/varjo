(in-package :varjo.import)
(in-readtable :varjo.import.readtable)

(defmacro assert-match (pattern arg &body body)
  (with-gensyms (form)
    `(let ((,form ,arg))
       (optima.extra:if-match ,pattern ,form
         (progn ,@body)
         (error "Match Assertion Failure:~%~s~%~s" ',pattern ,form)))))

(defvar *test-glsl*
  "#version 450

int FOO(int X);

int FOO(int X) {
    return (X * 10);
}

void main() {
    int A = 10;
    int B = FOO(A);
    gl_Position = vec4(float(A),float(B),float(0),float(1));
}")

(defun val-p (x)
  (not (eq x 'no-value)))

(defun import-shader (shader-stage-glsl)
  (let* ((glsl shader-stage-glsl)
         (ast (parse glsl)))
    (match ast
      ;;
      (`(shader ,@body)
        (post-process (mapcar #'import-shader-body-element body)))
      ;;
      (_ (error "Varjo.Import: This does not appear to be a shader:~%~s"
                glsl)))))

(defun post-process (forms)
  (labels ((func (x) (match x (`(%label ,@rest) rest))))
    (let* ((forms (remove nil forms))
           (funcs (remove nil (mapcar #'func forms)))
           (main (find :main funcs :key #'first :test #'string=))
           (funcs (remove main funcs :test #'equal))
           (other (set-difference forms funcs))
           (version (second (find :version other :key #'first))))
      ;; {TODO} the non func forms
      (dbind (nil nil . main-body) main
        (values
         `(labels ,funcs
            ,@main-body)
         '(:uniforms-placeholder)
         version)))))

(defun import-shader-body-element (body)
  (match body
    ;;
    (`(preprocessor-directive ,directive)
      (import-directive directive))
    ;;
    (`(function-declaration ,_)
      nil)
    ;;
    (`(function-definition ,prototype ,body)
      (import-function prototype body))
    ;;
    (_ `(:unknown-shader-body-element ,body))))

(defun import-directive (directive-string)
  (dbind (kind . args) (split-sequence:split-sequence #\space directive-string)
    (cond
      ((string= kind "#version")
       `(:version ,(intern (first args) :keyword)))
      (t `(:unknown-directive ,kind ,args)))))

(defun import-function (prototype body)
  (let ((form (import-compound-statement body)))
    `(%label ,@(import-function-prototype prototype)
             ,form)))

(defun import-function-prototype (prototype)
  (assert-match `(function-prototype
                  ,type-qualifier ,type-specifier ,identifier ,@args)
      prototype
    (assert (not (val-p type-qualifier)))
    (append
     (let ((return-type (import-type-specifier type-specifier))
           (name (import-function-identifier identifier))
           (args (mapcar #'import-function-arg args)))
       (declare (ignore return-type))
       `(,name ,args)))))

(defun import-compound-statement (form)
  (assert-match `(compound-statement ,@statements) form
    (reduce #'import-statement (reverse statements)
            :initial-value nil)))

(defun import-statement (accum form)
  (match form
    (`(variable-declaration ,@decl) (import-variable-declaration decl accum))
    (_ (import-form form))))

(defun import-variable-declaration (form body-form)
  (assert-match `(,type-qualifier ,type-specifier ,@initializers) form
    (let ((qualifier type-qualifier)
          (type (import-type-specifier type-specifier))
          (initializers (mapcar #'import-initializer
                                (varjo::group initializers 3))))
      (declare (ignore qualifier type))
      `(let ,initializers
         ,body-form))))

(defun import-initializer (initializer)
  (assert-match `(,id ,array-specifier ,form) initializer
    array-specifier ;; {TODO} hack
    (list (import-var-identifier id)
          (import-form form))))

(defun import-form (form)
  (cond
    ;; binary operators
    ((bin-op-form-p form) (import-binary-operator form))
    ;; other
    (t (match form
         (`(return ,form)
           ;;`(varjo::%return ,(import-form form))
           (import-form form)) ;; {TODO} hack

         (`(assignment ,@form) (import-assignment form))
         (`(modified-reference ,primary ,@modifiers)
           (dispatch-reference primary modifiers))
         ((type string) (import-var-identifier form))
         ((type number) form)
         (_ `(:unknown-expression ,form))))))

(defun import-assignment (form)
  (assert-match `(,id := ,expr) form
    `(setf ,(import-var-identifier id)
           ,(import-form expr))))

(defun import-type-specifier (specifier-form)
  (assert-match `(type-specifier ,type) specifier-form
    type))

(defun import-function-arg (arg)
  (assert-match `(,type-specifier ,name) arg
    (list (import-var-identifier name)
          (import-type-specifier type-specifier))))

(defun import-function-identifier (id)
  (let ((name (varjo::parse-gl-func-name id)))
    (or (find-symbol name :varjo-lang)
        (intern name))))

(defun import-var-identifier (id)
  (let ((name (if (uiop:string-prefix-p "gl_" id)
                  (varjo::parse-gl-var-name id)
                  (varjo::%parse id))))
    (or (find-symbol name :varjo-lang)
        (intern name))))

(defvar *binary-op-lookup*
  '((addition . +)
    (subtraction . -)
    (division . /)
    (multiplication . *)
    (modulus . mod)
    (left-shift . foo)
    (right-shift . foo)
    (less-than . <)
    (greater-than . >)
    (less-equal-than . <=)
    (greater-equal-than . >=)
    (equal . ==)
    (not-equal . !=)
    (bitwise-and . foo)
    (exclusive-or . foo)
    (inclusive-or . foo)
    (logical-and . foo)
    (logical-xor . foo)
    (logical-or . foo)))

(defun import-binary-operator (form)
  (dbind (op left right) form
    (let ((op (varjo::assocr op *binary-op-lookup*)))
      (assert (and (not (null op)) (not (eq op 'foo))))
      `(,op ,(import-form left) ,(import-form right)))))

(defun bin-op-form-p (form)
  (and (listp form)
       (not (null (find (first form) *binary-op-lookup* :key #'first)))))

(defun dispatch-reference (primary modifiers)
  (assert (= (length modifiers) 1) ()
          "Oh so THIS is when we have multiple modifiers")
  (let ((modifier (first modifiers))
        (table `((call-modifier . ,#'import-call)
                 (field-modifier . ,#'import-field)
                 (array-modifier . ,#'import-array)
                 (increment-modifier . ,#'import-increment)
                 (decrement-modifier . ,#'import-decrement))))
    (dbind (kind . args) modifier
      (funcall (varjo::assocr kind table) primary args))))

(defun import-call (primary args)
  (let ((primary
         (or (cdr (assoc primary '((:vec4 . rtg-math:v4!)
                                   (:vec3 . rtg-math:v3!)
                                   (:vec2 . rtg-math:v2!))))
             (import-function-identifier
              (etypecase primary
                (string primary)
                (symbol (glsl-keyword-to-string primary)))))))
    `(,primary ,@(mapcar #'import-form args))))

(defun glsl-keyword-to-string (kwd)
  (nth (position kwd *glsl-keyword-symbols*)
       *glsl-keywords*))

(defun import-field (primary args)
  (declare (ignore primary args))
  (error "not implemented"))

(defun import-array (primary args)
  (declare (ignore primary args))
  (error "not implemented"))

(defun import-increment (primary args)
  (declare (ignore primary args))
  (error "not implemented"))

(defun import-decrement (primary args)
  (declare (ignore primary args))
  (error "not implemented"))
