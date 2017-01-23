(in-package :varjo)
(in-readtable fn:fn-reader)

;;
;; Environment Public API
;;
;; In macros we have access to the environment, we should have safe ways to
;; interact with that environment.
;;

;;-------------------------------------------------------------------------
;; Variables

(defmethod variables-in-scope ((env environment))
  (let* ((all (foo env :stop-at-base t))
         (bindings (mapcar λ(list _ (get-symbol-binding _ t env)) all))
         (visible (remove nil bindings :key #'second)))
    (mapcar #'first visible)))

;;-------------------------------------------------------------------------


(defun foo (env &key stop-at-base)
  (let ((result (v-symbol-bindings env)))
    (labels ((stop-p (e)
               (or (and stop-at-base (typep e 'base-environment))
                   (eq e *global-env*))))
      (let ((current-env env))
        (loop :until (stop-p current-env) :do
           (setf current-env (v-parent-env current-env))
           (loop :for binding :in (v-symbol-bindings current-env) :do
              (push binding result)))))
    (let* ((dedup (remove-duplicates result :key #'first))
           (accessible (remove-if-not λ(apply #'binding-accesible-p env _)
                                      dedup))
           (names (mapcar #'first accessible)))
      names)))
