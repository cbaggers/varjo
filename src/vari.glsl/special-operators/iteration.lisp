(in-package :vari.glsl)
(in-readtable fn:fn-reader)

;;------------------------------------------------------------
;; Related Statements

(v-def-glsl-template-fun break () "break" () v-void)
(v-def-glsl-template-fun continue () "continue" () v-void)

;;------------------------------------------------------------
;; While

(v-defspecial while (test &rest body)
  :args-valid t
  :return
  (vbind (test-obj test-env) (compile-form test env)
    (vbind (body-obj final-env) (search-for-flow-id-fixpoint `(progn ,@body)
                                                             test-env)
      (if (v-typep (primary-type test-obj) 'v-bool)
          (let ((type-set (make-type-set)))
            (values (merge-compiled
                     (list body-obj test-obj)
                     :type-set type-set
                     :current-line nil
                     :to-block (gen-while-chunk test-obj (end-line body-obj)))
                    final-env))
          (error 'loop-will-never-halt :test-code test :test-obj test-obj)))))

;;------------------------------------------------------------
;; For
;;
;;   (for (a 0) (< a 10) (++ a)
;;     (* a 2))
;;
(v-defspecial for (var-form condition update &rest body)
  :args-valid t
  :return
  (progn
    (assert (not (consp (first var-form))) () 'for-loop-only-one-var)
    (multiple-value-bind (decl-obj decl-env)
        (with-v-let-spec var-form
          (compile-let name type-spec value-form env))
      ;;
      ;; We have to do some hackery below. compile-let has not current-line
      ;; but the assignment will be the last entry in to-block. We grab
      ;; this line for the decl glsl and put the other to-block entries
      ;; above the glsl statement
      ;;
      (let* ((to-block-lines (glsl-chunk-lines (to-block decl-obj)))
             (decl-chunk (glsl-chunk* (butlast to-block-lines)))
             (decl-assign-line (last1 to-block-lines))
             (condition-obj (end-line (compile-form condition decl-env)))
             (update-obj (compile-form update decl-env)))
        (vbind (body-obj final-env)
            (search-for-flow-id-fixpoint `(progn ,@body) decl-env)
          (if (and (glsl-chunk-emptyp (to-block condition-obj))
                   (glsl-chunk-emptyp (to-block update-obj)))
              (let ((loop-chunk (gen-for-loop-chunk
                                 decl-chunk
                                 decl-assign-line
                                 condition-obj
                                 update-obj
                                 body-obj))
                    (type-set (make-type-set)))
                (values (copy-compiled
                         body-obj
                         :type-set type-set
                         :current-line nil
                         :to-block loop-chunk
                         :place-tree nil)
                        final-env))
              (error 'for-loop-simple-expression)))))))

;;------------------------------------------------------------
;; Flow-ID resolution
;;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;
;; Purpose of code
;;
;; Ok so iirc the goal of this step is to calculate the flow ids for variables
;; being mutated from within a loop.
;;
;; The base example is below
;;
;;     (let ((a foo)
;;           (b bar))
;;       (for (i 0) (< i 10) (++ i)
;;         (setf a b)))
;;
;; if `a` starts with flow-id 1 and `b` starts with flow-id 2 then after the
;; first iteration `a`'s flow id is 2. Any number of additional loops wont
;; change that.
;;
;; So for the above code the flow is:
;;
;;  | a b
;; =|====
;; 0| 1 2
;; 1| 2 2
;; .| . .
;; n| 2 2
;;
;; and we can say that after the first iteration a fixpoint has been reached.
;;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;
;; Hey future baggers, it's past baggers. I see you've come back to revisit
;; this again, no doubt we've left it just long enough to forget how this
;; works.. thats fair, it's a monstrosity (have we started writing a sane
;; compiler yet?) ANYHOO you are looking for an 'in' to get started with this
;; code. Here is your path:
;;
;; - read #'get-changed-var/flow-id-pairs. it's the first call of note in
;;   #'search-for-flow-id-fixpoint and is fairly easy to follow.
;;
;; - next it's good to know that #'accumulate-flow-ids-into-maps does two
;;   jobs. The first is is to combine the flow-ids, but it also calls
;;   ensure-starting-flow-ids-for-var, so we have a record of the starting
;;   flow-ids these are used by #'fixpoint-reached
;;
;; - search-for-flow-id-fixpoint is best read by looking through the loop,
;;   it's pretty much pseudocode at this point.
;;
;;  - the flow-id-map var is a map of var-name to flow-id
;;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defvar *max-resolve-loop-flow-id-pass-count* 100)

(defun search-for-flow-id-fixpoint (code starting-env)
  ;;
  (let ((current-env starting-env)
        (last-code-obj nil)
        (checkpoint (checkpoint-flow-ids))

        (starting-env-flow-map (make-hash-table :test #'eq))
        (flow-id-map (make-hash-table :test #'eq)))
    ;;
    (labels ((ensure-starting-flow-ids-for-var (var-name)
               (unless (gethash var-name starting-env-flow-map)
                 (let* ((starting-binding (get-symbol-binding var-name
                                                              nil
                                                              starting-env)))
                   (setf (gethash var-name starting-env-flow-map)
                         (flow-ids starting-binding))))
               (values))

             (accumulate-flow-ids-into-maps (new-var/flow-id-pairs)
               (loop
                  :for (var-name . new-flow-id) :in new-var/flow-id-pairs
                  :for combined-flow-id := (flow-id! (gethash var-name flow-id-map)
                                                     new-flow-id)
                  :do
                  (setf (gethash var-name flow-id-map) combined-flow-id)
                  (ensure-starting-flow-ids-for-var var-name))
               (values))

             (compile-with-current-env (pass-count)
               ;;
               ;; Compile the body of the new with checkpointed flow-ids
               ;;
               (assert (< pass-count *max-resolve-loop-flow-id-pass-count*) ()
                       'loop-flow-analysis-failure)
               ;; This gives us deterministic flow-id values, which we need
               ;; for comparison (and thus accumulation) to work.
               ;;
               ;; It woudlnt hurt to do this for pass-count 0, but it would be
               ;; needless work.
               (when (> pass-count 0)
                 (reset-flow-ids-to-checkpoint checkpoint))
               (vbind (new-obj new-env) (compile-form code current-env)
                 (cons new-obj new-env))))
      ;;
      (loop
         :for pass-count :from 0
         :for (new-obj . new-env) := (compile-with-current-env pass-count)
         :for changed-var/flow-id-pairs := (get-changed-var/flow-id-pairs
                                            new-env
                                            current-env)
         :do
         (accumulate-flow-ids-into-maps changed-var/flow-id-pairs)
         (setf last-code-obj new-obj)
         (setf current-env new-env)
         :until (fixpoint-reached changed-var/flow-id-pairs
                                  starting-env-flow-map)))
    ;;
    (values last-code-obj
            (create-post-loop-env flow-id-map starting-env))))


(defun fixpoint-reached (new-flow-ids starting-flow-map)
  (or
   ;; if no variable from outer scope changed then we stop
   (not new-flow-ids)
   ;; if none of the variables that we changed were set to
   ;; values from the outer scope we stop (as information
   ;; has stopped flowing into the loop, there is nothing
   ;; else to glean)
   (let* ((names-of-changed-variables (mapcar #'car new-flow-ids))
          (starting-super-id
           (reduce (lambda (accum name)
                     (flow-id! accum (gethash name starting-flow-map)))
                   names-of-changed-variables
                   :initial-value nil)))
     ;; if none of the new-flow-ids are already in the starting-super-id
     ;; then (as the above said) we are done
     (notany λ(id~= _ starting-super-id)
             (mapcar #'cdr new-flow-ids)))))

(defun create-post-loop-env (accumulated-flow-id-map starting-env)
  (labels ((splice-in-flow-id (accum-env id-pair)
             (dbind (vname . new-flow-id) id-pair
               (replace-flow-ids-for-single-var vname
                                                new-flow-id
                                                accum-env))))
    (reduce #'splice-in-flow-id
            (alexandria:hash-table-alist accumulated-flow-id-map)
            :initial-value starting-env)))

(defun get-changed-var/flow-id-pairs (new-env last-env)
  ;; find-env-bindings looks at every variable binding in both the supplied
  ;; environments and returns the names of the bindings that match. As we
  ;; want the changes we pass in the complement of #'eq
  ;;
  ;; The 'match' in the above description is a comparison of two v-values
  ;; (assuming vars which we can because of variables-only) using our
  ;; test function.
  ;;
  ;; The result of find-env-bindings is a list of names
  (let* ((variables-changed (find-env-bindings new-env last-env
                                               :test (complement #'eq)
                                               :stop-at-base t
                                               :variables-only t)))
    ;; now we need to take these and remove any which have the
    ;; same flow-id. This can happen if a variable is set to
    ;; itself from within a loop
    ;;
    ;; For the vars have changes (have a new flow-id) we collect
    ;; the name and it's flow-ids as a cons-pair
    ;;
    ;; We don't worry about recieving a macro here as we called
    ;; find-env-bindings with :variables-only t
    ;;                                      ↓↓↓
    (loop
       :for var-name :in variables-changed
       :for last-var := (get-symbol-binding var-name nil last-env)
       :for new-var := (get-symbol-binding var-name nil new-env)
       :unless (or (not last-var)
                   (not new-var)
                   (id= (flow-ids last-var)
                        (flow-ids new-var)))
       :collect (cons var-name (flow-ids new-var)))))
