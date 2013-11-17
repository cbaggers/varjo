(defun first-symbols (tree)
  (when (not (atom tree))
    (cons (when (symbolp (first tree)) (first tree))
          (loop for e in tree :if (listp e) :append (first-symbols e)))))

(defun sort-functions-by-usage (func-specs)
  (let* ((names (mapcar #'first func-specs))
         (bodies (mapcar #'cddr func-specs))
         (scores
          (loop :for current-func :in names :for index :from 0 :collect
             (list index
                   (loop :for other-body :in bodies
                      :for other-index :from 0 :summing
                      (if (member current-func (first-symbols other-body))
                          (if (eql index other-index)
                              (error 'recursive-function-call
                                     (nth index func-specs))
                              (expt 2 other-index))
                          0))))))
    (if (lists-contain-duplicates-p (mapcar #'second scores))
        (error 'recursive-function-call func-specs)
        (loop :for (n) :in (sort scores #'< :key #'second)
           :collect (nth n func-specs)))))


;; this doesnt work as due to lisp-n you cant work out which forms are 
;; function calls or not.

;; The answer for glsl was to ignore the function order and just produce a
;; function signature that floated to the top of the code
