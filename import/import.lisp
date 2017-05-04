(in-package :varjo.import)
(in-readtable :varjo.import.readtable)

(defmacro assert-match (pattern arg &body body)
  (with-gensyms (form)
    `(let ((,form ,arg))
       (optima.extra:if-match ,pattern ,form
         (progn ,@body)
         (error "Match Assertion Failure:~%~%Pattern: ~s~%Form: ~s"
                ',pattern ,form)))))

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

(defvar *func-glsl*
  "void SGPP_hash_2D( vec2 gridcell, out vec4 hash_0, out vec4 hash_1 )	//	generates 2 random numbers for each of the 4 cell corners
{
    //    gridcell is assumed to be an integer coordinate
    vec4 hash_coord = SGPP_coord_prepare( vec4( gridcell.xy, gridcell.xy + 1.0 ) );
    hash_0 = SGPP_permute( SGPP_permute( hash_coord.xzxz ) + hash_coord.yyww );
    hash_1 = SGPP_resolve( SGPP_permute( hash_0 ) );
    hash_0 = SGPP_resolve( hash_0 );
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

(defun import-glsl-function (function-glsl)
  (let* ((glsl function-glsl)
         (ast (parse glsl)))
    (match ast
      ;;
      (`(shader ,@body)
        (post-process-func (mapcar #'import-shader-body-element body)))
      ;;
      (_ (error "Varjo.Import: This does not appear to be a shader:~%~s"
                glsl)))))

(defun post-process-func (forms)
  (let* ((forms (remove nil forms)))
    ;; {TODO} the non func forms
    (dbind ((label name args &body body)) forms
      (assert (eq label '%label) ()
              "Does not appear to contain a glsl function~%~s"
              (first forms))
      `(:defun-g ,name ,args
         ,@(mapcar #'code-cleaner body)))))

(defun code-cleaner (form)
  (match form
    ((guard x (constantp x)) x)
    ((guard x (symbolp x)) x)
    (`(let ,@a) (code-cleaner `(let* ,@a)))
    (`(let* (,@a) (let* (,@b) ,@c))
      (code-cleaner `(let* (,@a ,@b) ,@c)))
    (`(let* (,@a) (let (,@b) ,@c))
      (code-cleaner `(let* (,@a ,@b) ,@c)))
    (`(let* ,a (progn ,@b) ,@c)
      (code-cleaner `(let* ,a ,@b ,@c)))
    (`(let* ,a ,b (progn ,@c) ,@d)
      (code-cleaner `(let* ,a ,b ,@c ,@d)))
    (`(let* ,a ,b ,c (progn ,@d) ,@e)
      (code-cleaner `(let* ,a ,b ,c ,@d ,@e)))
    (`(let* ,a ,b ,c ,d (progn ,@e) ,@f)
      (code-cleaner `(let* ,a ,b ,c ,d ,@e ,@f)))
    (`(progn (progn ,@a) ,@b)
      (code-cleaner
       `(progn
          ,@(mapcar #'code-cleaner a)
          ,@(mapcar #'code-cleaner b))))
    (`(progn ,a (progn ,@b) ,@c)
      (code-cleaner `(progn ,a ,@b ,@c)))
    (`(progn ,a ,b (progn ,@c) ,@d)
      (code-cleaner `(progn ,a ,b ,@c ,@d)))
    (`(progn ,a) (code-cleaner a))
    (`(,@a) (mapcar #'code-cleaner a))))

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
            ,@main-body
            (values))
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
    (_ (if accum
           `(progn
              ,(import-form form)
              ,accum)
           (import-form form)))))

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
  (ematch form
    (`(,id := ,expr)
      `(setf ,(import-var-identifier id)
             ,(import-form expr)))
    (`(,id :+= ,expr)
      `(incf ,(import-var-identifier id)
             ,(import-form expr)))
    (`(,id :-= ,expr)
      `(decf ,(import-var-identifier id)
             ,(import-form expr)))
    (`(,id :*= ,expr)
      `(varjo::multf ,(import-var-identifier id)
             ,(import-form expr)))
    (`(,id :/= ,expr)
      `(varjo::divf ,(import-var-identifier id)
             ,(import-form expr)))))

(defun import-type-specifier (specifier-form)
  (assert-match `(type-specifier ,type) specifier-form
    (import-type type)))

(defun import-type (type)
  type)

(defun skip (x)
  (format t "Skipped: ~a~%" x)
  '<SKIPPED>)

(defun import-function-arg (arg)
  (ematch arg
    (`((type-specifier ,type) ,name)
      (list (import-var-identifier name)
            (import-type type)))

    (`((type-qualifier ,qualifier)
       (type-specifier, type)
       ,name)
      (list name type qualifier)
      (skip arg))))

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

(defun swizzle-p (str)
  (labels ((comp (set c) (find c set)))
    (let ((len (length str)))
      (and (> len 1)
           (<= len 4)
           (or (every λ(comp "xyzw" _) str)
               (every λ(comp "rgba" _) str)
               (every λ(comp "stpq" _) str))
           str))))

(defun import-field (primary args)
  (match args
    ((guard `(,field-name) (swizzle-p field-name))
     (import-swizzle primary field-name))
    (`(,field-name)
      (import-field-access primary field-name))
    (_ (error "not implemented"))))

(defun import-field-access (primary field-name)
  `(,(import-function-identifier field-name)
     ,(import-place primary)))

(defun import-place (primary)
  (typecase primary
    (string (import-var-identifier primary))
    (t (error "not implemented"))))

(defun import-swizzle (primary swizzle)
  `(varjo-lang::s~
    ,(import-swizzlable-form primary)
    ,(intern (string-upcase swizzle) :keyword)))

(defun import-swizzlable-form (primary)
  (typecase primary
    (string (import-var-identifier primary))
    (t (error "not implemented"))))

(defun import-array (primary args)
  (declare (ignore primary args))
  (error "not implemented"))

(defun import-increment (primary args)
  (declare (ignore primary args))
  (error "not implemented"))

(defun import-decrement (primary args)
  (declare (ignore primary args))
  (error "not implemented"))
