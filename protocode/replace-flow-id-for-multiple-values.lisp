;; stopped working on this as it was getting slow again and i should be
;; focussing on other things in varjo today

(defun replace-flow-ids-for-multiple-vars (var-name-to-flow-id-map
                                           env)
  (let ((scope-env-name-val-flowid-list nil))
    ;;
    ;; Make the list of things that need changing
    (maphash (lambda (var-name new-flow-ids)
               (vbind (current-value env-holding-var)
                   (get-symbol-binding var-name nil env)
                 ;; We dont replace things in the base environment
                 (unless (typep env-holding-var 'base-environment)
                   (push (list (v-function-scope env-holding-var)
                               env-holding-var
                               var-name
                               current-value
                               new-flow-ids)
                         scope-env-name-val-flowid-list))))
             var-name-to-flow-id-map)
    ;;
    ;; Sort the things that need changing so that they are in order of which
    ;; enivornment is nearest base.
    ;;
    ;; (remember, sort is destructive)
    (setf scope-env-name-val-flowid-list
          (sort scope-env-name-val-flowid-list #'< :key #'first))
    ;;
    (if scope-env-name-val-flowid-list
        (let* ((most-ancestoral-env (second ;; ← env from sub-list
                                     (first ;; ← nearest env to base
                                      scope-env-name-val-flowid-list)))
               ;; all-envs-to-replace is the list of environments that are the
               ;; children of the 'oldest' env we need to replace (in order of
               ;; nearest decendant to furthest)
               (all-envs-to-process nil)
               ;; cur-env is the current environment we are visiting
               (cur-env most-ancestoral-env))
          ;; Walking up the tree until we find the earliest env that need
          ;; changing. This loop pushes that env and all its children onto
          ;; all-envs-to-process
          (loop
             :do
             (let ((parent (v-parent-env cur-env)))
               (push cur-env all-envs-to-process)
               (setf cur-env parent))
             :until (eq cur-env most-ancestoral-env))
          ;;
          ;; Time to do the surgery
          (let ((new-parent (v-parent-env most-ancestoral-env)))
            (loop :for env :in all-envs-to-process :do
               (let ((foo (find env scope-env-name-val-flowid-list
                                :key #'second :test #'eq)))
                 ;;
                 ;; THIS IS NOT FINISHED
                 ;; foo is should be a list of things from this env to chage
                 ;; the code below should handle those multiple changes.
                 ;; blech, this is balls
                 ;;
                 (if foo
                     (setf new-parent
                           (dbind (scope this-env var-name current-value new-flow-ids)
                               foo
                             (declare (ignore scope this-env))
                             (env-replace-symbol-bindings
                              env
                              (a-add var-name
                                     (copy-value current-value
                                                 :type (replace-flow-id
                                                        (v-type-of current-value)
                                                        new-flow-ids))
                                     (copy-list (v-symbol-bindings env))))))
                     (setf new-parent
                           (env-replace-parent env new-parent)))))
            new-parent))
        env)))
