
;; ## Make a varjo-interop package
;;
;; In here we will have the from-lisp special form
;;
;; by default you use it like this

(from-lisp (+ 1 2 3) :int)

;; and a implicit uniform will be added and that form stored
;; any forms that are #'equal are deduped.
;;
;; if the form is a symbol naming an existing uniform then the dedup stage
;; will merge it with the existing uniform.
;;
;; if the type is not know this will become a stemcell

;; Next we have expand-to-entrypoint It's used like this

(expand-to-entrypoint a)

;; shader-val is special. The form must be a symbol It looks at the flow-id
;; (there must be only one) of the val bound to 'a in the environment AND if it
;; can be shown that that flow-id maps to a uniform or stemcell then the
;; shader-val form is replaced with the name of the uniform.
;; Also if there is there is no bound var then it expands to the argument, so:
(expand-to-entrypoint *foo*)
;; becomes
*foo*
;; which then becomes a stemcell form
(from-lisp *foo*)

;; With this we can implement spaces more easily
(defmacro-g in (space &body body)
  (let ((*current-space* (expand-to-entrypoint ,space)))
    ,@body))

(defmacro-g get-transform (space-a space-b)
  `(from-lisp (get-transform (expand-to-entrypoint ,space-a)
                             (expand-to-entrypoint ,space-b))))

;; this is wrong unless from-lisp specifically expands expand-to-entrypoint
;; forms.


;;----------------------------------------------------------------------
