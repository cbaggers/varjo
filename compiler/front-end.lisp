;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)
(in-readtable fn:fn-reader)

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
               ',(mapcar (lambda (_)
                           (destructuring-bind
                                 (stage-in-args stage-uniforms stage-context)
                               (split-arguments (second _) '(&uniform &context))
                             (declare (ignore stage-context))
                             (list stage-in-args
                                   (if (equal first-uniforms stage-uniforms)
                                       stage-uniforms
                                       (concatenate 'list stage-uniforms
                                                    first-uniforms))
                                   (cons (first _) first-context)
                                   (third _))))
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
    (find-if (lambda (_)
               (when (member _ context) _))
             *stage-types*)))

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
    (every (lambda (_)
             (member _ pq))
           cq)))



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
  (flow-id-scope
    (let ((env (%make-base-environment)))
      (pipe-> (in-args uniforms context body env)
	#'split-input-into-env
	#'process-context
	#'add-context-glsl-vars
	#'process-in-args
	#'process-uniforms
	#'wrap-in-main-function
	(equalp #'symbol-macroexpand-pass
		#'macroexpand-pass
		#'compiler-macroexpand-pass)
	#'compile-pass
	#'make-post-process-obj
	#'filter-used-items
	#'check-stemcells
	#'gen-in-arg-strings
	#'gen-out-var-strings
	#'final-uniform-strings
	#'dedup-strings
	#'final-string-compose
	#'code-obj->result-object))))

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
  (let* ((raw-context (v-raw-context env))
         (raw-context (if (member :no-iuniforms raw-context)
                          (remove :iuniforms raw-context)
                          raw-context)))
    (setf (v-context env)
          (loop :for item :in raw-context
             :if (find item *valid-contents-symbols*) :collect item
             :else :do (error 'invalid-context-symbol :context-symb item))))
  (values code env))

;;----------------------------------------------------------------------

(defun add-context-glsl-vars (code env)
  (values code (add-glsl-vars env *glsl-variables*)))

;;----------------------------------------------------------------------

;; {TODO} get rid of all this ugly imperitive crap, what was I thinking?
(defun process-in-args (code env)
  "Populate in-args and create fake-structs where they are needed"
  (let ((in-args (v-raw-in-args env)))
    (loop :for in-arg :in in-args :do
       (with-arg (name type qualifiers declared-glsl-name) in-arg
         (let* ((type-obj (type-spec->type type))
                (glsl-name (or declared-glsl-name (safe-glsl-name-string name))))
           (if (typep type-obj 'v-struct)
               (add-in-arg-fake-struct name glsl-name type-obj qualifiers env)
               (progn
                 (%add-var name (v-make-value type-obj env :glsl-name glsl-name)
			   env)
                 (setf (v-in-args env)
                       (append (v-in-args env)
                               `((,name ,(type->type-spec type-obj) ,qualifiers
                                        ,glsl-name)))))))))
    (values code env)))

;;----------------------------------------------------------------------

(defun process-uniforms (code env)
  (let ((uniforms (v-raw-uniforms env)))
    (mapcar
     (lambda (_)
       (with-arg (name type qualifiers glsl-name) _
         (case-member qualifiers
           (:ubo (process-ubo-uniform name glsl-name type qualifiers env))
           (:fake (process-fake-uniform name glsl-name type qualifiers env))
           (otherwise (process-regular-uniform name glsl-name type
                                               qualifiers env)))))
     uniforms)
    (values code env)))

;; mutates env
(defun process-regular-uniform (name glsl-name type qualifiers env)
  (let* ((true-type (v-true-type (type-spec->type type))))
    (%add-var name
	      (v-make-value true-type env :glsl-name
			    (or glsl-name (safe-glsl-name-string name))
			    :read-only t)
	      env))
  (push (list name type qualifiers glsl-name) (v-uniforms env))
  env)

;; mutates env
(defun process-ubo-uniform (name glsl-name type qualifiers env)
  (let* ((true-type (v-true-type (type-spec->type type))))
    (%add-var name (v-make-value
		    true-type env
		    :glsl-name (or glsl-name (safe-glsl-name-string name))
		    :flow-ids (flow-id!) :function-scope 0 :read-only t)
	      env))
  (push (list name type qualifiers glsl-name) (v-uniforms env))
  env)

;; mutates env
(defun process-fake-uniform (name glsl-name type qualifiers env)
  (let ((type-obj (type-spec->type type)))
    (add-uniform-fake-struct name glsl-name type-obj qualifiers env))
  env)

;;----------------------------------------------------------------------

(defun wrap-in-main-function (code env)
  (values `(%make-function-no-implicit :main () ,code)
          env))

;;----------------------------------------------------------------------

(defun v-symbol-macroexpand-all (form &optional (env :-GENV-))
  (cond ((null form) nil)
        ((atom form)
         (let ((sm (get-symbol-macro form env)))
	   (if sm
	       (values (first sm) `(,form))
	       form)))
        ((consp form)
	 (vbind (expanded-a found-a) (v-symbol-macroexpand-all (car form))
	   (vbind (expanded-b found-b) (v-symbol-macroexpand-all (cdr form))
	     (values (cons expanded-a expanded-b)
		     (append found-a found-b)))))))

(defun symbol-macroexpand-pass (form env)
  (vbind (form used) (v-symbol-macroexpand-all form env)
    (push used (used-symbol-macros env))
    (values form env)))

(defun dedup-used (used)
  (remove-duplicates (flatten used)))

;;----------------------------------------------------------------------

(defun v-macroexpand-all (code &optional (env :-GENV-))
  (cond ((atom code) code)
        (t (let* ((head (first code))
                  (m (get-macro head env)))
             (if m
                 (vbind (f u) (v-macroexpand-all (apply m (rest code)) env)
		   (values f (cons head u)))
                 (let ((i (mapcar λ(vlist (v-macroexpand-all _ env))
				  code)))
		   (values (mapcar #'first i) (mapcar #'second i))))))))

(defun macroexpand-pass (code env)
  (vbind (form used) (v-macroexpand-all code env)
    (push used (used-macros env))
    (values form env)))

;;----------------------------------------------------------------------

(defun v-compiler-macroexpand-all (code &optional (env :-GENV-))
  (cond ((atom code) code)
        (t (let* ((head (first code))
                  (m (get-compiler-macro head env)))
             (if m
		 (vbind (f u)
		     (v-compiler-macroexpand-all (apply m (rest code)) env)
		   (values f (cons head u)))
		 (let ((i (mapcar λ(vlist (v-compiler-macroexpand-all _ env))
				  code)))
		   (values (mapcar #'first i) (mapcar #'second i))))))))

(defun compiler-macroexpand-pass (code env)
  (vbind (form used) (v-compiler-macroexpand-all code env)
    (push used (used-compiler-macros env))
    (values form env)))

;;----------------------------------------------------------------------

(defun compile-pass (code env)
  (varjo->glsl code env))

;;----------------------------------------------------------------------

(defclass post-compile-process ()
  ((code :initarg :code :accessor code)
   (env :initarg :env :accessor env)
   (in-args :initarg :in-args :accessor in-args)
   (out-vars :initarg :out-vars :accessor out-vars)
   (uniforms :initarg :uniforms :accessor uniforms)
   (stemcells :initarg :stemcells :accessor stemcells)
   (used-types :initarg :used-types :accessor used-types)
   (used-symbol-macros :initarg :used-symbol-macros
		       :accessor used-symbol-macros)
   (used-macros :initarg :used-macros :accessor used-macros)
   (used-compiler-macros :initarg :used-compiler-macros
			 :accessor used-compiler-macros)
   (ast :initarg :ast :reader ast)))

(defun make-post-process-obj (code env)
  (make-instance 'post-compile-process :code code :env env
		 :used-symbol-macros (dedup-used (used-symbol-macros env))
		 :used-macros (dedup-used (used-macros env))
		 :used-compiler-macros (dedup-used (used-compiler-macros env))
		 :ast (node-tree code)))

;;----------------------------------------------------------------------

(defun filter-used-items (post-proc-obj)
  "This changes the code-object so that used-types only contains used
   'user' defined structs."
  (with-slots (code env) post-proc-obj
    (setf (used-types post-proc-obj)
	  (loop :for i :in (find-used-user-structs code env)
	     :collect (type-spec->type i :env env))))
  post-proc-obj)

;;----------------------------------------------------------------------

(defun check-stemcells (post-proc-obj)
  (with-slots (code) post-proc-obj
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
      (setf (stemcells post-proc-obj)
	    (remove-duplicates stemcells :test #'equal
			       :key #'first))
      post-proc-obj)))

;;----------------------------------------------------------------------

(defun calc-locations (types)
;;   "Takes a list of type objects and returns a list of positions
;; - usage example -
;; (let ((types (mapcar #'type-spec->type '(:mat4 :vec2 :float :mat2 :vec3))))
;;          (mapcar #'cons types (calc-positions types)))"
  (labels ((%calc-location (sizes type)
             (cons (+ (first sizes) (v-glsl-size type)) sizes)))
    (reverse (reduce #'%calc-location (butlast types) :initial-value '(0)))))


(defun gen-in-arg-strings (post-proc-obj)
  (with-slots (env) post-proc-obj
    (let* ((types (mapcar #'second (v-in-args env)))
	   (type-objs (mapcar #'type-spec->type types))
	   (locations (if (member :vertex (v-context env))
			  (calc-locations type-objs)
			  (loop for i below (length type-objs) collect nil))))
      (setf (in-args post-proc-obj)
	    (loop :for (name type-spec qualifiers glsl-name) :in (v-in-args env)
	       :for location :in locations :for type :in type-objs :collect
	       `(,name ,type ,qualifiers
		       nil ;;,(flow-ids (get-var name env)) removed until structs supported
		       ,(gen-in-var-string (or glsl-name name) type
					   qualifiers location))))))
  post-proc-obj)

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

(defun gen-out-var-strings (post-proc-obj)
  (with-slots (code env) post-proc-obj
    (let* ((out-vars (dedup-out-vars (out-vars code)))
	   (out-types (mapcar (lambda (_)
				(v-type (third _)))
			      out-vars))
	   (locations (if (member :fragment (v-context env))
			  (calc-locations out-types)
			  (loop for i below (length out-types) collect nil))))
      (setf (out-vars post-proc-obj)
	    (loop :for (name qualifiers value) :in out-vars
	       :for type :in out-types
	       :for location :in locations
	       :collect (let ((glsl-name (v-glsl-name value)))
			  `(,name ,(type->type-spec (v-type value))
				  ,@qualifiers ,glsl-name
				  ,(gen-out-var-string glsl-name type qualifiers
						       location)))))
      post-proc-obj)))

;;----------------------------------------------------------------------

(defun final-uniform-strings (post-proc-obj)
  (with-slots (code env) post-proc-obj
    (let ((final-strings nil)
	  (structs (used-types post-proc-obj))
	  (uniforms (v-uniforms env))
	  (implicit-uniforms nil))
      (loop :for (name type qualifiers glsl-name) :in uniforms
	 :for type-obj = (type-spec->type type) :do
	 (push `(,name ,type ,(flow-ids (get-var name env))
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

      (loop :for (name string-name type) :in (stemcells post-proc-obj) :do
	 (when (eq type :|unknown-type|) (error 'symbol-unidentified :sym name))
	 (let ((type-obj (type-spec->type type)))
	   (push `(,name ,type
			 ,(gen-uniform-decl-string
			   (or string-name (error "stem cell without glsl-name"))
			   type-obj
			   nil)
			 ,string-name)
		 implicit-uniforms)

	   (when (and (v-typep type-obj 'v-user-struct)
		      (not (find (type->type-spec type-obj) structs
				 :key #'type->type-spec :test #'equal)))
	     (push type-obj structs))))

      (setf (used-types post-proc-obj) structs)
      (setf (uniforms post-proc-obj) final-strings)
      (setf (stemcells post-proc-obj) implicit-uniforms)
      post-proc-obj)))

;;----------------------------------------------------------------------

(defun dedup-strings (post-proc-obj)
  (with-slots (code) post-proc-obj
    (setf code
	  (copy-code
	   code
	   :to-top (remove-duplicates (to-top code) :test #'equal)
	   :signatures (remove-duplicates (signatures code) :test #'equal)))
    (setf (used-types post-proc-obj)
	  (remove-duplicates (mapcar #'v-signature (used-types post-proc-obj))
			     :test #'equal)))
  post-proc-obj)

;;----------------------------------------------------------------------

(defun final-string-compose (post-proc-obj)
  (values (gen-shader-string post-proc-obj)
	  post-proc-obj))

;;----------------------------------------------------------------------

(defun code-obj->result-object (final-glsl-code post-proc-obj)
  (with-slots (env) post-proc-obj
    (let* ((context (v-context env)))
      (make-instance
       'varjo-compile-result
       :glsl-code final-glsl-code
       :stage-type (find-if λ(find _ *supported-stages*) context)
       :in-args (mapcar #'butlast (in-args post-proc-obj))
       :out-vars (mapcar #'butlast (out-vars post-proc-obj))
       :uniforms (mapcar #'butlast (uniforms post-proc-obj))
       :implicit-uniforms (stemcells post-proc-obj)
       :context context
       :function-calls (function-call-flow-tracking (env post-proc-obj))
       :used-symbol-macros (used-symbol-macros post-proc-obj)
       :used-macros (used-macros post-proc-obj)
       :used-compiler-macros (used-compiler-macros post-proc-obj)
       :ast (ast post-proc-obj)))))
