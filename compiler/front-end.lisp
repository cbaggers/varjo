;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)
(named-readtables:in-readtable fn_::fn_lambda)

;;----------------------------------------------------------------------

;; These two are example macros to show how to use the compiler.
;; They dont provide anything

(defmacro defshader (name args &body body)
  (declare (ignore name))
  (destructuring-bind (in-args uniforms context)
      (split-arguments args '(&uniform &context))
    `(translate ',in-args ',uniforms ',context '(progn ,@body))))

;; (defpipeline blah
;;     (:vertex ((pos :vec3) &uniform (a :float))
;;              (values (v! pos 1.0) a))
;;     (:fragment ((hmm :float))
;;                (labels ((fun ((x :float))
;;                           (* x x)))
;;                  (v! 1.0 1.0 hmm (fun a)))))

(defmacro defpipeline (name &body body)
  (declare (ignore name))
  (destructuring-bind (in-args first-uniforms first-context)
      (split-arguments (second (first body)) '(&uniform &context))
    (declare (ignore in-args))
    `(format
      nil "~{~a~%~}"
      (mapcar #'glsl-code
              (rolling-translate
               ',(mapcar λ(destructuring-bind
                                (stage-in-args stage-uniforms stage-context)
                              (split-arguments (second _) '(&uniform &context))
                            (declare (ignore stage-context))
                            (list stage-in-args
                                  (if (equal first-uniforms stage-uniforms)
                                      stage-uniforms
                                      (concatenate 'list stage-uniforms
                                                   first-uniforms))
                                  (cons (first _) first-context)
                                  (third _)))
                         body))))))

(defun v-macroexpand (form &optional (env (make-varjo-environment)))
  (identity
   (pipe-> (form env)
     (equalp #'symbol-macroexpand-pass
             #'macroexpand-pass
             #'compiler-macroexpand-pass))))

;;----------------------------------------------------------------------

(defparameter *stage-types* '(:vertex :geometry :tess-eval :tess-control :fragment))

(defmacro with-stage ((&optional (in-args (symb :in-args))
                                 (uniforms (symb :uniforms))
                                 (context (symb :context))
                                 (code (symb :code)))
                         stage &body body)
  `(destructuring-bind (,in-args ,uniforms ,context ,code) ,stage
     (declare (ignorable ,in-args ,uniforms ,context ,code))
     ,@body))

(defun rolling-translate (stages)
  (let ((result (reduce #'compile-stage stages :initial-value nil)))
    (reverse (cons (caar result) (rest result)))))

(defun merge-in-previous-stage-args (previous-stage stage)
  (if previous-stage
      (with-stage () stage
        (list (if (args-compatiblep stage previous-stage)
                  (mapcar #'%merge-in-arg
                          (out-vars previous-stage) in-args)
                  (error 'args-incompatible in-args (out-vars previous-stage)))
              uniforms
              context
              code))
      stage))

(defmacro with-arg ((&optional (name (gensym "name")) (type (gensym "type"))
                                  (qualifiers (gensym "qualifiers"))
                                  (glsl-name (gensym "glsl-name")))
                          arg-form &body body)
  (let ((qn (gensym "qn")))
    `(destructuring-bind (,name ,type . ,qn) ,arg-form
       (declare (ignorable ,name ,type))
       (let ((,qualifiers (if (stringp (last1 ,qn)) (butlast ,qn) ,qn))
             (,glsl-name (when (stringp (last1 ,qn)) (last1 ,qn))))
         (declare (ignorable ,qualifiers ,glsl-name))
         ,@body))))

(defun %merge-in-arg (previous current)
  (with-arg (c-name c-type c-qual c-glsl-name) current
    (with-arg (p-name p-type p-qual p-glsl-name) previous
      `(,c-name
        ,(or c-type p-type)
        ,@(union c-qual p-qual)
        ,(or p-glsl-name c-glsl-name)))))

(defun compile-stage (accum stage)
  (destructuring-bind (last-stage remaining-stage-types)
      (or (first accum) `(nil ,*stage-types*))
    (let ((remaining-stage-types (check-order (extract-stage-type stage)
                                              remaining-stage-types)))
      (cons (list (apply #'translate (transform-stage-args
                                      (merge-in-previous-stage-args last-stage
                                                                    stage)))
                  remaining-stage-types)
            (cons last-stage (cddr accum))))))

(defun extract-stage-type (stage)
  (let ((context (typecase stage
                   (list (with-stage () stage context))
                   (varjo-compile-result (context stage)))))
    (find-if λ(when (member _ context) _) *stage-types*)))

(defun args-compatiblep (stage previous-stage)
  (with-stage () stage
    (and (loop :for p :in (out-vars previous-stage)
            :for c :in in-args :always
            (and (v-type-eq (type-spec->type (second p))
                            (type-spec->type (second c)))
                 (%suitable-qualifiersp p c)))
         (loop :for u :in (uniforms previous-stage) :always
            (let ((match (find (first u) uniforms :key #'first)))
              (if match
                  (v-type-eq (type-spec->type (second u))
                             (type-spec->type (second match)))
                  t)))
         (context-ok-given-restriction
          (remove (extract-stage-type previous-stage) (context previous-stage))
          (remove (extract-stage-type stage) context)))))

(defun in-arg-qualifiers (in-arg)
  (with-arg (_ _1 q) in-arg q))

(defun %suitable-qualifiersp (prev-stage-in-arg in-arg)
  (let ((pq (in-arg-qualifiers prev-stage-in-arg))
        (cq (in-arg-qualifiers in-arg)))
    (every λ(member _ pq) cq)))



(let ((arg-transformers (list '(:geometry . (lambda (i u c code)
                                              (list i u c code))))))
  (defun transform-stage-args (stage)
    (let* ((stage-type (extract-stage-type stage))
           (transform (cdr (assoc stage-type arg-transformers))))
      (if transform (apply transform stage) stage))))

;;{TODO} make proper error
(defun check-order (stage-type remaining-stage-types)
  (let ((check (member stage-type remaining-stage-types)))
    (if check
        (rest check)
        (error 'stage-order-error stage-type))))

(defun translate (in-args uniforms context body)
  (let ((env (make-instance 'environment)))
    (pipe-> (in-args uniforms context body env)
      #'split-input-into-env
      #'process-context
      #'process-in-args
      #'process-uniforms
      #'wrap-in-main-function
      #'add-context-glsl-vars
      (equalp #'symbol-macroexpand-pass
              #'macroexpand-pass
              #'compiler-macroexpand-pass)
      #'compile-pass
      #'filter-used-items
      #'check-stemcells
      #'gen-in-arg-strings
      #'gen-out-var-strings
      #'final-uniform-strings
      #'dedup-strings
      #'final-string-compose
      #'code-obj->result-object)))

;;----------------------------------------------------------------------


(defun check-arg-forms (in-args) (every #'check-arg-form in-args))
(defun check-arg-form (arg)
  (unless
      (and
       ;; needs to at least have name and type
       (>= (length arg) 2)
       ;; of the rest of the list it must be keyword qualifiers and optionally a
       ;; string at the end. The string is a declaration of what the name of the
       ;; var will be in glsl. This feature is intended for use only by the compiler
       ;; but I see not reason to lock this away.
       (every #'keywordp (in-arg-qualifiers arg)))
    (error "Declaration ~a is badly formed.~%Should be (-var-name- -var-type- &optional qualifiers)" arg))
  t)

;;[TODO] Move these errors vvv^^^^^
(defun check-for-dups (in-args uniforms)
  (if (intersection (mapcar #'first in-args) (mapcar #'first uniforms))
      (error "Varjo: Duplicates names found between in-args and uniforms")
      t))

;;{TODO} fix error message
(defun check-for-stage-specific-limitations (env)
  (cond ((or (and (member :vertex (v-context env))
                  (some #'third (v-raw-in-args env))))
         (error "In args to vertex shaders can not have qualifiers")))
  t)

(defun split-input-into-env (in-args uniforms context body env)
  (when (and (check-arg-forms uniforms) (check-arg-forms in-args)
             (check-for-dups in-args uniforms))
    (setf (v-raw-in-args env) in-args)
    (setf (v-raw-uniforms env) uniforms)
    (setf (v-raw-context env) context)
    (when (not context) (setf (v-raw-context env) *default-context*))
    (when (check-for-stage-specific-limitations env)
      (values body env))))

;;----------------------------------------------------------------------

(defun process-context (code env)
  ;; ensure there is a version
  (unless (loop :for item :in (v-raw-context env)
             :if (find item *supported-versions*) :return item)
    (push *default-version* (v-raw-context env)))
  (setf (v-context env)
        (loop :for item :in (v-raw-context env)
           :if (find item *valid-contents-symbols*) :collect item
           :else :do (error 'invalid-context-symbol :context-symb item)))
  (values code env))

;;----------------------------------------------------------------------

;; {TODO} get rid of all this ugly imperitive crap, what was I thinking?
(defun process-in-args (code env)
  "Populate in-args and create fake-structs where they are needed"
  (let ((in-args (v-raw-in-args env)))
    (loop :for in-arg :in in-args :do
       (with-arg (name type qualifiers declared-glsl-name) in-arg
         (let* ((type-obj (type-spec->type type :place t))
                (glsl-name (or declared-glsl-name (safe-glsl-name-string name))))
           (if (typep type-obj 'v-struct)
               (add-in-arg-fake-struct name glsl-name type-obj qualifiers env)
               (progn
                 (add-var name (make-instance 'v-value :type type-obj
                                              :glsl-name glsl-name)
                          env t)
                 (setf (v-in-args env)
                       (append (v-in-args env)
                               `((,name ,(type->type-spec type-obj) ,qualifiers
                                        ,glsl-name)))))))))
    (values code env)))

;;----------------------------------------------------------------------

(defun process-uniforms (code env)
  (let ((uniforms (v-raw-uniforms env)))
    (mapcar
     λ(with-arg (name type qualifiers glsl-name) _
        (case-member qualifiers
          (:ubo (process-ubo-uniform name glsl-name type qualifiers env))
          (:fake (process-fake-uniform name glsl-name type qualifiers env))
          (otherwise (process-regular-uniform name glsl-name type qualifiers env))))
     uniforms)
    (values code env)))

;; mutates env
(defun process-regular-uniform (name glsl-name type qualifiers env)
  (let* ((true-type (v-true-type (type-spec->type type))))
    (add-var name
             (v-make-value (set-place-t true-type) env
                           (or glsl-name (safe-glsl-name-string name)))
             env t))
  (push (list name type qualifiers glsl-name) (v-uniforms env))
  env)

;; mutates env
(defun process-ubo-uniform (name glsl-name type qualifiers env)
  (let* ((true-type (v-true-type (type-spec->type type))))
    (add-var name (make-instance
                   'v-value
                   :glsl-name (or glsl-name (safe-glsl-name-string name))
                   :type (set-place-t true-type))
             env t))
  (push (list name type qualifiers glsl-name) (v-uniforms env))
  env)

;; mutates env
(defun process-fake-uniform (name glsl-name type qualifiers env)
  (let ((type-obj (type-spec->type type :place t)))
    (add-uniform-fake-struct name glsl-name type-obj qualifiers env))
  env)

;;----------------------------------------------------------------------

(defun wrap-in-main-function (code env)
  (values `(%make-function :main () ,code)
          env))

;;----------------------------------------------------------------------

(defun add-context-glsl-vars (code env)
  (values code (add-glsl-vars env *glsl-variables*)))

;;----------------------------------------------------------------------

(defun v-symbol-macroexpand-all (form &optional (env :-GENV-))
  (cond ((null form) nil)
        ((atom form)
         (let ((sm (get-symbol-macro form env)))
           (or (first sm) form)))
        ((consp form) (cons (v-symbol-macroexpand-all (car form))
                            (v-symbol-macroexpand-all (cdr form))))))

(defun symbol-macroexpand-pass (form env)
  (values (v-symbol-macroexpand-all form env) env))

;;----------------------------------------------------------------------

(defun v-macroexpand-all (code &optional (env :-GENV-))
  (cond ((atom code) code)
        (t (let* ((head (first code))
                  (m (get-macro head env)))
             (if m
                 (v-macroexpand-all (apply m (rest code)) env)
                 (loop :for c :in code :collect (v-macroexpand-all c env)))))))

(defun macroexpand-pass (code env)
  (values (v-macroexpand-all code env) env))

;;----------------------------------------------------------------------

(defun v-compiler-macroexpand-all (code &optional (env :-GENV-))
  (cond ((atom code) code)
        (t (let* ((head (first code))
                  (m (get-compiler-macro head env)))
             (if m
                 (v-compiler-macroexpand-all (apply m (rest code)) env)
                 (loop :for c :in code :collect (v-compiler-macroexpand-all c env)))))))

(defun compiler-macroexpand-pass (code env)
  (values (v-compiler-macroexpand-all code env) env))

;;----------------------------------------------------------------------

(defun compile-pass (code env)
  (varjo->glsl code env))

;;----------------------------------------------------------------------

(defun filter-used-items (code env)
  "This changes the code-object so that used-types only contains used
   'user' defined structs."
  (setf (used-types code)
        (loop :for i :in (remove-duplicates (find-used-user-structs code env))
           :collect (type-spec->type i :env env)))
  (values code env))

;;----------------------------------------------------------------------

(defun calc-locations (types)
;;   "Takes a list of type objects and returns a list of positions
;; - usage example -
;; (let ((types (mapcar #'type-spec->type '(:mat4 :vec2 :float :mat2 :vec3))))
;;          (mapcar #'cons types (calc-positions types)))"
  (labels ((%calc-location (sizes type)
             (cons (+ (first sizes) (v-glsl-size type)) sizes)))
    (reverse (reduce #'%calc-location (butlast types) :initial-value '(0)))))



(defun gen-in-arg-strings (code env)
  (let* ((types (mapcar #'second (v-in-args env)))
         (type-objs (mapcar #'type-spec->type types))
         (locations (if (member :vertex (v-context env))
                        (calc-locations type-objs)
                        (loop for i below (length type-objs) collect nil))))
    (setf (v-in-args env)
          (loop :for (name type-spec qualifiers glsl-name) :in (v-in-args env)
             :for location :in locations :for type :in type-objs :collect
             `(,name ,type ,qualifiers
                     ,(gen-in-var-string (or glsl-name name) type
                                         qualifiers location)))))
  (values code env))

;;----------------------------------------------------------------------

(defun dedup-out-vars (out-vars)
  (let ((seen (make-hash-table))
        (deduped nil))
    (loop :for (name qualifiers value) :in out-vars
       :do (let ((tspec (type->type-spec (v-type value))))
             (if (gethash name seen)
                 (unless (equal tspec (gethash name seen))
                   (error 'out-var-type-mismatch :var-name name
                          :var-types (list tspec (gethash name seen))))
                 (setf (gethash name seen) tspec
                       deduped (cons (list name qualifiers value)
                                     deduped)))))
    (reverse deduped)))

(defun gen-out-var-strings (code env)
  (let* ((out-vars (dedup-out-vars (out-vars code)))
         (out-types (mapcar λ(v-type (third _)) out-vars))
         (locations (if (member :fragment (v-context env))
                        (calc-locations out-types)
                        (loop for i below (length out-types) collect nil))))
    (setf (out-vars code)
          (loop :for (name qualifiers value) :in out-vars
             :for type :in out-types
             :for location :in locations
             :collect (let ((glsl-name (v-glsl-name value)))
                        `(,name ,(type->type-spec (v-type value))
                                ,@qualifiers ,glsl-name
                                ,(gen-out-var-string glsl-name type qualifiers
                                                     location)))))
    (values code env)))

;;----------------------------------------------------------------------

(defun check-stemcells (code env)
  (let ((stemcells (stemcells code)))
    (mapcar
     (lambda (x)
       (dbind (name string type) x
         (declare (ignore string))
         (when (remove-if-not (lambda (x)
                                (and (equal name (first x))
                                     (not (equal type (third x)))))
                              stemcells)
           (error "Symbol ~a used with different implied types" name))))
     ;; {TODO} Proper error here
     stemcells)
    (setf (stemcells code) (remove-duplicates stemcells :test #'equal
                                              :key #'first)))
  (values code env))

;;----------------------------------------------------------------------

(defun final-uniform-strings (code env)
  (let ((final-strings nil)
        (structs (used-types code))
        (uniforms (v-uniforms env))
        (implicit-uniforms nil))

    (loop :for (name type qualifiers glsl-name) :in uniforms
       :for type-obj = (type-spec->type type) :do
       (push `(,name ,type
                     ,(if (member :ubo qualifiers)
                          (varjo::write-interface-block
                           :uniform (or glsl-name (safe-glsl-name-string name))
                           (v-slots type-obj))
                          (gen-uniform-decl-string
                           (or glsl-name (safe-glsl-name-string name))
                           type-obj
                           qualifiers)))
             final-strings)
       (when (and (v-typep type-obj 'v-user-struct)
                  (not (find (type->type-spec type-obj) structs
                             :key #'type->type-spec :test #'equal)))
         (push type-obj structs)))


    (loop :for (name string-name type) :in (stemcells code)
       :for type-obj = (type-spec->type type) :do

       (push `(,name ,string-name ,type
                     ,(gen-uniform-decl-string
                       (or string-name (safe-glsl-name-string name))
                       ;;{TODO} ^^^ this is wrong
                       type-obj
                       nil))
             implicit-uniforms)

       (when (and (v-typep type-obj 'v-user-struct)
                  (not (find (type->type-spec type-obj) structs
                             :key #'type->type-spec :test #'equal)))
         (push type-obj structs)))

    (setf (used-types code) structs)
    (setf (v-uniforms env) final-strings)
    (setf (stemcells code) implicit-uniforms))
  (values code env))

;;----------------------------------------------------------------------

(defun dedup-strings (code env)
  (setf (to-top code)
        (remove-duplicates (to-top code) :test #'equal))
  (setf (signatures code)
        (remove-duplicates (signatures code) :test #'equal))
  (setf (used-types code)
        (remove-duplicates (mapcar #'v-signature (used-types code))
                           :test #'equal))
  (values code env))

;;----------------------------------------------------------------------

(defun final-string-compose (code env)
  (setf (current-line code) (gen-shader-string code env))
  (values code env))

;;----------------------------------------------------------------------

(defun code-obj->result-object (code env)
  (make-instance 'varjo-compile-result
                 :glsl-code (current-line code)
                 :stage-type (loop for i in (v-context env)
                                :if (find i *supported-stages*) :return i)
                 :in-args (mapcar #'butlast (v-in-args env))
                 :out-vars (mapcar #'butlast (out-vars code))
                 :uniforms (mapcar #'butlast (v-uniforms env))
                 :implicit-uniforms (stemcells code)
                 :context (v-context env)))
