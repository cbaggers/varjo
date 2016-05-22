(in-package :varjo)
(in-readtable fn:fn-reader)


(defun v-compile (uniforms context
		  &key vertex tesselation-control
		    tesselation-evaluation geometry fragment
		    (compile-func #'translate))
  (asserting
   ((a vertex)
    (b fragment)
    (c (if (or tesselation-control tesselation-evaluation)
	   (and tesselation-control tesselation-evaluation)
	   t)))
   "The compile function has found the following issues:"
   "- The vertex shader argument is mandatory"
   "- The fragment shader argument is mandatory"
   "- When either tesselation-evaluation or tesselation-control are
  specified, both must be specified")
  (get-stage-arg-transformers tesselation-control geometry fragment)
  (loop :for step :in steps :collect
     (if (typep stage 'varjo-compile-result)
	 (use-precompiled-stage &&&)
	 )))


(defun compile-single-stage (in-args uniforms stage-name version code
			     &key (compile-func #'translate))
  )

(defun get-stage-arg-transformer (tesselation-control geometry fragment)

  (cond
    (tesselation-control
     (append
      (list #'vertex->tess-control #'tess-control->tess-eval)
      (if geometry
	  (list #'tess-eval->geometry #'geometry->fragment)
	  (list #'identity #'tess-eval->fragment))))
    (geometry
     (list #'identity #'identity #'vertex->geometry #'geometry->fragment))
    (fragment
     (list #'identity #'identity #'identity #'vertex->fragment))))


(defun geometry->fragment ()
  nil)

(defun tess-control->tess-eval ()
  nil)

(defun tess-eval->fragment ()
  nil)

(defun tess-eval->geometry ()
  nil)

(defun vertex->fragment ()
  nil)

(defun vertex->geometry ()
  nil)

(defun vertex->tess-control ()
  nil)

(defun merge-in-arg (previous current)
  (with-v-arg (c-name c-type c-qual c-glsl-name) current
    (with-v-arg (p-name p-type p-qual p-glsl-name) previous
      `(,c-name
        ,(or c-type p-type)
        ,@(union c-qual p-qual)
        ,(or p-glsl-name c-glsl-name)))))

(defun out-vars->in-args (in-args previous-stage)
  (mapcar #'merge-in-arg (out-vars previous-stage) in-args))
