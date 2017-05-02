(in-package :varjo)
(in-readtable fn:fn-reader)

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
                     :to-block (list (gen-while-string
                                      test-obj (end-line body-obj)))
                     :node-tree (ast-node!
                                 'while (mapcar #'node-tree
                                                (list test-obj body-obj))
                                 type-set env final-env))
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
  (if (consp (first var-form))
      (error 'for-loop-only-one-var)
      (multiple-value-bind (code new-env)
          (with-v-let-spec var-form
            (compile-let name type-spec value-form env))
        (let* ((var-string (subseq (first (to-block code))
                                   0
                                   (1- (length (first (to-block code))))))
               (decl-obj (compile-form (second var-form) new-env))
               (condition-obj (compile-form condition new-env))
               (update-obj (compile-form update new-env)))
          (unless (or (v-typep (primary-type decl-obj) 'v-uint)
                      (v-typep (primary-type decl-obj) 'v-int)
                      (v-typep (primary-type decl-obj) 'v-float))
            (error 'invalid-for-loop-type :decl-obj decl-obj))
          (vbind (body-obj final-env)
              (search-for-flow-id-fixpoint `(progn ,@body) new-env)
            (if (and (null (to-block condition-obj))
                     (null (to-block update-obj)))
                (let ((loop-str (gen-for-loop-string
                                 var-string condition-obj update-obj
                                 (end-line body-obj)))
                      (type-set (make-type-set)))
                  (values (copy-compiled
                           body-obj
                           :type-set type-set
                           :current-line nil
                           :to-block (list loop-str)
                           :node-tree (ast-node!
                                       'for (cons var-form
                                                  (mapcar #'node-tree
                                                          (list condition-obj
                                                                update-obj
                                                                body-obj)))
                                       type-set env final-env)
                           :place-tree nil)
                          final-env))
                (error 'for-loop-simple-expression)))))))

;;------------------------------------------------------------
;; For's flow-id resolution



(defun search-for-flow-id-fixpoint (code starting-env)
  ;; Lets document this a bit and work out how to debug it from a crash
  (labels ((names-to-new-flow-bindings (x)
             (let ((binding (get-symbol-binding x nil starting-env)))
               ;; This should never be an issue as we are working from
               ;; #'get-new-flow-ids which itself works on variables.
               ;;                       ↓↓↓↓
               (assert (not (typep binding 'v-symbol-macro)))
               `(,x . ,(flow-ids binding)))))
    ;;
    (let ((envs (list starting-env))
          (last-code-obj nil)
          (flow-ids nil)
          (checkpoint (checkpoint-flow-ids)))
      (loop :for pass :from 0
         :for current-env = (first envs)
         :until (vbind (o new-env) (compile-form code current-env)
                  (let* ((new-flow-ids (get-new-flow-ids new-env current-env))
                         (f-ids (or flow-ids
                                    (mapcar #'names-to-new-flow-bindings
                                            (mapcar #'car new-flow-ids)))))
                    (setf last-code-obj o
                          envs (cons new-env envs)
                          flow-ids (accumulate-flow-ids f-ids new-flow-ids))
                    (let ((done (fixpoint-reached
                                 new-flow-ids starting-env pass)))
                      (unless done (reset-flow-ids-to-checkpoint checkpoint))
                      done))))
      (values last-code-obj
              (create-post-loop-env flow-ids starting-env)))))

;; defun replace-flow-ids (old-var-name old-val flow-ids old-env env)
(defun create-post-loop-env (new-flow-id-pairs starting-env)
  (labels ((splice-in-flow-id (accum-env id-pair)
             (dbind (vname . new-flow-id) id-pair
               (vbind (old-val old-env) (get-symbol-binding vname nil accum-env)
                 (replace-flow-ids vname old-val new-flow-id
                                   old-env accum-env)))))
    (reduce #'splice-in-flow-id new-flow-id-pairs :initial-value starting-env)))

(defun accumulate-flow-ids (flow-ids new-flow-ids)
  (labels ((work (accum y)
             (dbind (vname . fid) y
               (acons vname (flow-id! (assocr vname accum)
                                      fid)
                      accum))))
    (remove-duplicates
     (reduce #'work new-flow-ids :initial-value flow-ids)
     :test #'eq :key #'first :from-end t)))

(defvar *max-resolve-loop-flow-id-pass-count* 100)

(defun get-new-flow-ids (latest-env last-env)
  (let* ((variables-changed (find-env-bindings latest-env last-env
                                               :test (complement #'eq)
                                               :stop-at-base t
                                               :variables-only t))
         ;; now we need to take these a remove any which have the
         ;; same flow-id. This can happen if a variable is set to
         ;; itself from within a loop
         (trimmed-changes
          ;; We don't worry about recieving a macro here as we called
          ;; find-env-bindings with :variables-only t
          ;;                           ↓↓↓↓↓↓↓↓
          (mapcar λ(let ((last-var (get-symbol-binding _ nil last-env))
                         (new-var (get-symbol-binding _ nil latest-env)))
                     (unless (or (not last-var)
                                 (not new-var)
                                 (id= (flow-ids last-var)
                                      (flow-ids new-var)))
                       _))
                  variables-changed)))
    (mapcar λ`(,_ . ,(flow-ids (get-symbol-binding _ nil latest-env)))
            (remove nil trimmed-changes))))

(defun fixpoint-reached (new-flow-ids starting-env pass)
  (unless (< pass *max-resolve-loop-flow-id-pass-count*)
    (error 'loop-flow-analysis-failure))
  (let* ((variables-changed (mapcar #'car new-flow-ids)))
    (or
     ;; if no variable from outer scope changed then we stop
     (not variables-changed)
     ;; if none of the variables that we changed were set to
     ;; values from the outer scope we stop (as information
     ;; has stopped flowing into the loop, there is nothing
     ;; else to glean)
     (let* ((starting-flow-ids
             (mapcar λ(flow-ids (get-symbol-binding _ nil starting-env))
                     variables-changed))
            (starting-super-id
             (reduce #'flow-id! starting-flow-ids)))
       (not (some λ(id~= _ starting-super-id)
                  (mapcar #'cdr new-flow-ids)))))))
