;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)

;;----------------------------------------------------------------------

;; These two are example macros to show how to use the compiler.
;; They dont provide anything 

(defmacro defshader (name args &body body)
  (declare (ignore name))
  (destructuring-bind (in-args uniforms context)
      (split-arguments args '(&uniform &context))
    `(translate ',in-args ',uniforms ',context '(progn ,@body))))

(defmacro defpipeline (name args &body stages)
  (declare (ignore name))
  (destructuring-bind (in-args uniforms context)
      (split-arguments args '(&uniform &context))
    `(format nil "~{~a~%~}" (mapcar #'glsl-code (rolling-translate ',in-args
                                                                   ',uniforms
                                                                   ',context
                                                                   ',stages)))))

;;----------------------------------------------------------------------

;;(defvar *arg-transformers* (list (:vertex . #'identity) ))

;; {TODO} check in args on have one stream
(defun rolling-translate (in-args uniforms context stages)
  (compile-stages in-args uniforms context stages
                  '(:vertex :geometry :tess-eval :tess-control :fragment)
                  ;;*arg-transformers*
                  nil))

(defun compile-stages (in-args uniforms context stages remaining-stage-types accum)
  (let ((stage (first stages)))
    (typecase stage
      (varjo-compile-result 
       (precompiled-stage in-args uniforms context
                          stages remaining-stage-types accum))
      (list (if (null stages)
                (reverse accum)
                (compile-stage in-args uniforms context
                               stages remaining-stage-types accum)))
      (t (error 'invalid-shader-stage :stage stage)))))

(defun precompiled-stage (in-args uniforms context
                          stages remaining-stage-types accum)
  (let* ((stage (first stages))
         (remaining-stage-types 
          (check-order (stage-type stage) remaining-stage-types)))
    (if (args-compatiblep in-args uniforms context stage)        
        (compile-stages (gen-in-args-for-next-stage stage)
                        uniforms
                        context
                        (rest stages)
                        remaining-stage-types
                        (cons stage accum))
        (error 'args-incompatible :current-args (in-args stage)
               :previous-args in-args))))

(defun compile-stage (in-args uniforms context
                      stages remaining-stage-types accum)
  (let ((stage (first stages)))
    (destructuring-bind (stage-type &rest code) stage
      (let* ((remaining-stage-types 
              (check-order stage-type remaining-stage-types))
             (result (translate in-args uniforms (cons stage-type context)
                                `(progn ,@code))))
        (compile-stages (gen-in-args-for-next-stage result)
                        uniforms
                        context
                        (rest stages)
                        remaining-stage-types
                        (cons result accum))))))

(defun args-compatiblep (in-args uniforms context stage)
  (and (loop :for p :in in-args :for c :in (in-args stage) :always
          (and (equal (first p) (first c))
               (v-type-eq (type-spec->type (second p)) 
                          (type-spec->type (second c)))
               (equal (third p) (third c))))
       (loop :for u :in (uniforms stage) :always (find u uniforms :test #'equal))
       (context-ok-given-restriction (context stage) context)))

(defun gen-in-args-for-next-stage (compiled-stage)
  (loop :for (name qualifiers value) :in (out-vars compiled-stage)
     :collect `(,name ,(type->type-spec (v-type value))
                      ,@qualifiers)))

;;{TODO} make proper error
(defun check-order (stage-type remaining-stage-types)
  (if (member stage-type remaining-stage-types)
      (subseq remaining-stage-types (position stage-type remaining-stage-types))
      (error "stage of type ~s is not valid at this place in the pipeline, this is either out of order or a stage of this type already exists" stage-type)))

(defun translate (in-args uniforms context body)
  (let ((env (make-instance 'environment)))
    (pipe-> (in-args uniforms context body env)
      #'split-input-into-env
      #'process-context
      #'process-in-args
      #'process-uniforms
      #'wrap-in-main-function
      #'add-context-glsl-vars
      (equal #'macroexpand-pass
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

;;[TODO] Move these errors
(defun check-arg-forms (in-args &aux )
  (loop for stream in in-args :do 
       (when (or (not (every #'keywordp (cddr stream))) (< (length stream) 2))
         (error "Declaration ~a is badly formed.~%Should be (-var-name- -var-type- &optional qualifiers)" stream)))
  t)

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
    (loop :for (name type . qualifiers) :in in-args :do
       (let* ((type (if (and (not (keywordp type))
                             (not (vtype-existsp type))
                             (vtype-existsp (sym-down type)))
                        (sym-down type)
                        type))
              (type-obj (type-spec->type type :place t)))
         (if (typep type-obj 'v-struct)
             (add-fake-struct name type-obj qualifiers env)
             (progn
               (add-var name (make-instance 'v-value :type type-obj 
                                            :glsl-name (safe-glsl-name-string name))
                        env t)               
               (setf (v-in-args env) 
                     (append (v-in-args env)
                             `((,name ,(type->type-spec type-obj) ,qualifiers))))))))
    (values code env)))

;;----------------------------------------------------------------------

(defun process-uniforms (code env)
  (let ((uniforms (v-raw-uniforms env)))
    (loop :for (name type) :in uniforms :do
       (let* ((type (if (and (not (keywordp type))
                             (not (vtype-existsp type))
                             (vtype-existsp (sym-down type)))
                        (sym-down type)
                        type))
              (true-type (v-true-type (type-spec->type type))))
         (add-var name (make-instance 'v-value
                                      :glsl-name (safe-glsl-name-string name)
                                      :type (set-place-t true-type)) 
                  env t))
       (push (list name type) (v-uniforms env)))
    (values code env)))

;;----------------------------------------------------------------------

(defun wrap-in-main-function (code env)
  (values `(%make-function :main () ,code)
          env))

;;----------------------------------------------------------------------

(defun add-context-glsl-vars (code env)
  (values code (add-glsl-vars env *glsl-variables*)))

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

(defun gen-in-arg-strings (code env &aux position)
  ;;`(,fake-slot-name ,slot-type ,qualifiers)
  (when (find :vertex (v-context env)) (setf position 0))
  (setf (v-in-args env) 
        (loop :for (name type qualifiers) :in (v-in-args env)
           :for type-obj = (type-spec->type type :env env)           
           :collect `(,name ,type ,qualifiers
                      ,(gen-in-var-string name type-obj qualifiers position))
           :if position :do (incf position (v-glsl-size  type-obj))))
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
                       deduped (cons (list name qualifiers value) deduped)))))
    deduped))

(defun gen-out-var-strings (code env)
  (let ((out-vars (dedup-out-vars (out-vars code))))
    (setf (out-vars code)
          (loop :for (name qualifiers value) :in out-vars
             :collect (list name qualifiers value 
                            (gen-out-var-string name qualifiers value))))
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
    (loop :for (name type) :in uniforms
       :for type-obj = (type-spec->type type) :do
       (if (uniform-string-gen type-obj)
           (loop :for x :in (funcall (uniform-string-gen type-obj) 
                                     name type)
              :do (push x final-strings))
           (push `(,name ,type ,(gen-uniform-decl-string name type-obj))
                 final-strings))
       (when (and (v-typep type-obj 'v-user-struct)
                  (not (find (type->type-spec type-obj) structs
                             :key #'type->type-spec :test #'equal)))
         (push type-obj structs)))
    
    
    (loop :for (name string-name type) :in (stemcells code)
       :for type-obj = (type-spec->type type) :do

       (push `(,name ,string-name ,type ,(gen-uniform-decl-string name type-obj))
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
                 :in-args (loop :for i :in (v-in-args env) :collect
                             (subseq i 0 3))
                 :out-vars (loop :for i :in (out-vars code) :collect
                             (subseq i 0 3))
                 :uniforms (loop :for i :in (v-uniforms env) :collect
                              (subseq i 0 2))
                 :implicit-uniforms (stemcells code)
                 :context (v-context env)
                 :used-external-functions (used-external-functions code)))

