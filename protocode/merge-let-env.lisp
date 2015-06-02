(in-package :varjo)

;; (v-defun %merge-let-envs (bindings include-type-declarations)
;;   :special
;;   :args-valid t
;;   :return
;;   (let* ((res (loop :for binding :in bindings
;;                  :for new-env = (clone-environment original-env) :collect
;;                  (multiple-value-list (varjo->glsl `(progn ,@body) new-env))))
;;          (decl-objs (mapcar #'first res))
;;          (decl-envs (mapcar #'second res)))
;;     (values (if include-type-declarations
;;                   (merge-obs decl-objs
;;                              :type (make-instance 'v-none)
;;                              :to-block (append (mapcan #'to-block decl-objs)
;;                                                (mapcar #'end-line decl-objs))
;;                              :to-top (mapcan #'to-top decl-objs))
;;                   (make-instance 'code :type 'v-none))
;;             (let ((var-len (length (v-variables env))))
;;               (setf (v-variables env)
;;                     (loop :for decl-env :in decl-envs :append
;;                        (subseq (v-variables decl-env) 0
;;                                (- (length (v-variables decl-env) var-len)))))
;;               env))))
;; meh dont need this now :)
;; well that is odd, body doesnt even exists, maybe I was drunk..who knows
