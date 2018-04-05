(in-package :varjo.utils)

(defun cons-end (thing list)
  (concatenate 'list list (list thing)))

(defun listify (x) (if (listp x) x (list x)))
(defun nth-or-self (n x) (if (listp x) (nth n x) x))

(defun lambda-list-get-names (l-list)
  (let ((keywords '(&allow-other-keys &environment &rest &aux &key &whole &body
                    &optional)))
    (loop :for i :in l-list
       :if (not (member i keywords))
       :collect (if (listp i) (first i) i))))

;; [TODO] fully implement positions-if to match position-if spec
;; [TODO] also add positions-if-not and positions: could be all be useful
(defun positions-if (predicate sequence)
  (let ((i -1))
    (labels ((f (accum x)
               (incf i)
               (if (funcall predicate x)
                   (cons i accum)
                   accum)))
      (reverse (reduce #'f sequence :initial-value nil)))))

(defun elt* (sequence &rest indicies)
  (labels ((_elt* (sequence indicies accum)
             (if indicies
                 (_elt* sequence
                        (rest indicies)
                        (cons (elt sequence (first indicies)) accum))
                 (reverse accum))))
    (_elt* sequence indicies nil)))


(defmacro pipe-> (args &body stages)
  "\(pipe-> \(1 2 3\) #'a #'b #'c #'d\)
   Calls first function with args provided and uses result as
   arguments for next function. Uses multiple-value-call so you
   can use (values) to specify complex lambda-args."
  (let ((stages (reverse stages)))
    (when stages
      (let ((stage (first stages)))
        (if (eq 'function (first stage))
            `(multiple-value-call ,stage
               ,(if (rest stages)
                    `(pipe-> ,args ,@(reverse (rest stages)))
                    (if (listp args)
                        `(values ,@args)
                        `(values-list ,args))))
            (destructuring-bind (check-func &rest steps) stage
              `(let ((rest (multiple-value-list
                            ,(if (rest stages)
                                 `(pipe-> ,args ,@(reverse (rest stages)))
                                 (if (listp args)
                                     `(values ,@args)
                                     `(values-list ,args))))))
                 (let ((args rest))
                   (let ((passes nil))
                     (loop :do (let ((results (multiple-value-list
                                               (pipe-> ,@(cons 'args steps)))))
                                 (setf args results)
                                 (push results passes))
                        :until (,check-func (first passes) (second passes))))
                   (values-list args)))))))))

(defmacro dbind (lambda-list expression &body body)
  `(destructuring-bind ,lambda-list ,expression ,@body))

(defmacro vbind (vars value-form &body body)
  ;; {TODO} handle declare forms properly. It is complicated
  ;;        as the declare has to be the first thing in the scope
  ;;        but the vars are now split across multiple binds
  (let* ((list? (mapcar #'listp vars))
         (mvb-vars (mapcar (lambda (v l?) (if l? (gensym) v)) vars list?))
         (d-vars (mapcar (lambda (v l?) (when l? v)) vars list?))
         (d-forms (mapcar (lambda (mvb d)
                            (when d `(dbind ,d ,mvb)))
                          mvb-vars d-vars))
         (d-forms (remove nil d-forms)))
    `(multiple-value-bind ,mvb-vars ,value-form
       (declare (ignorable ,@mvb-vars))
       ,@(reduce (lambda (accum x)
                   (list (append x accum)))
                 (cons body d-forms)))))

(defmacro vlist (value-form)
  `(multiple-value-list ,value-form))

(defun kwd (&rest args)
  (intern (format nil "~{~a~}" args) 'keyword))

(defun group (source n)
  "This takes a  flat list and emit a list of lists, each n long
   containing the elements of the original list"
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n)
                                   acc))
                   (nreverse (cons source acc))))))
    (if source
        (rec source nil)
        nil)))

(defun group-by (key sequence &key (test #'equal))
  "Groups by key and maintains order"
  (let ((groups (make-hash-table :test test))
        (id 0))
    (labels ((do-it (e)
               (let ((kval (funcall key e)))
                 (if (gethash kval groups)
                     (push e (gethash kval groups))
                     (setf (gethash kval groups) (list e (incf id)))))))
      (map nil #'do-it sequence))
    (mapcar #'rest
            (sort (mapcar #'reverse (hash-table-values groups))
                  #'< :key #'first))))

(defun symb (&rest args)
  "This takes a list of symbols (or strings) and outputs one
   symbol.
   If the input is symbol/s then the output is a regular symbol
   If the input is string/s, then the output is
   a |symbol like this|"
  (values (intern (format nil "~{~a~}" args))))

(defun p-symb (package &rest args)
  "This takes a list of symbols (or strings) and outputs one
   symbol.
   If the input is symbol/s then the output is a regular symbol
   If the input is string/s, then the output is
   a |symbol like this|"
  (values (intern (format nil "~{~a~}" args) package)))

(defun assocr (item alist &key (key nil keyp) (test nil testp)
                            (test-not nil notp))
  (cdr (apply #'assoc item alist (append (when keyp (list :key key))
                                         (when testp (list :test test))
                                         (when notp (list test-not))))))


(define-compiler-macro assocr (item alist &key (key nil keyp)
                                    (test nil testp)
                                    (test-not nil notp))
  `(cdr (assoc ,item ,alist
               ,@(when keyp (list :key key))
               ,@(when testp (list :test test))
               ,@(when notp (list test-not)))))

(defun find-duplicates (list)
  (let ((map (make-hash-table)))
    (loop :for e :in list :do
       (incf (gethash e map 0)))
    (let (result)
      (maphash (lambda (k v)
                 (when (> v 1)
                   (push k result)))
               map)
      result)))

(defun last1 (list)
  (car (last list)))

(defun lambda-list-split (template lam-list)
  (labels ((kwd (x) (intern (format nil "~a" x) :keyword))
           (symbol-name= (x y) (equal (symbol-name x) (symbol-name y)))
           (collector (lam-list &optional current-modifier accum)
             (let ((item (first lam-list)))
               (cond ((null lam-list) accum)
                     ((and (symbolp item) (eql (elt (symbol-name item) 0) #\&))
                      (collector (rest lam-list)
                                 (kwd item)
                                 accum))
                     (t (collector (rest lam-list)
                                   current-modifier
                                   (acons current-modifier
                                          (cons item
                                                (cdr (assoc current-modifier
                                                            accum)))
                                          accum))))))
           (clean-alist (alist &optional accum)
             (let ((item (first alist)))
               (cond ((null alist) accum)
                     ((atom item) (clean-alist (rest alist) accum))
                     ((not (assoc (first item) accum))
                      (clean-alist (rest alist) (cons item accum)))
                     (t (clean-alist (rest alist) accum)))))
           (reverse-results (r)
             (loop for (n . rst) in r collect (cons n (reverse rst))))
           (first-in-template-p (x) (or (null (first x))
                                        (member (first x) template
                                                :test #'symbol-name= ))))
    (let ((template (when template (cons nil (mapcar #'kwd template))))
          (split (collector lam-list)))
      (if (or (null template)
              (every #'first-in-template-p split))
          (reverse-results (clean-alist split))
          (let* ((&-syms (remove-if-not
                          (lambda (x)
                            (when (symbolp x) (eq (elt (symbol-name x) 0) #\&)))
                          lam-list))
                 (unknown (remove-if (lambda (x) (member x template))
                                     &-syms)))
            (error "~%Varjo: Found the symbol~a ~a. Given that it starts with '&' it looks
like a lambda list keyword. Unfortunately the only lambda list keywords that
are supported in this context are: ~s"
                   (if (> (length unknown) 1) "s" "")
                   (if (= (length unknown) 1) (first unknown) unknown)
                   (remove nil template)))))))

(defun split-arguments (args
                        &optional (template '(&uniform &context)))
  (let* ((split (lambda-list-split template args))
         (in-args (cdr (assoc nil split))))
    (cons in-args
          (loop :for kw :in template :collect
             (cdr (assoc (kwd kw) split))))))

(defmacro case-member (member-form (&key (test #'eql)) &body cases)
  `(cond
     ,@(loop :for (item . rest) :in cases :collect
          (if (string-equal item 'otherwise)
              `(t ,@rest)
              `((member ,item ,member-form :test ,test) ,@rest)))))

(defun n-of (thing count)
  (loop :for i :below count :collect thing))

(defmacro case= (form &body cases)
  (let ((g (gensym "val")))
    (labels ((wrap-case (c) `((= ,g ,(first c)) ,@(rest c))))
      (let* ((cases-but1 (mapcar #'wrap-case (butlast cases)))
             (last-case (car (last cases)))
             (last-case (if (eq (car last-case) 'otherwise)
                            `(t ,@(rest last-case))
                            (wrap-case last-case)))
             (cases (append cases-but1 (list last-case))))
        `(let ((,g ,form))
           (cond ,@cases))))))

(defun find-similarly-named-symbol (source-symb candidates-list)
  (when (symbolp source-symb)
    (let ((sn (symbol-name source-symb)))
      (remove-duplicates
       (remove-if-not
        (lambda (x)
          (let ((x (symbol-name x)))
            (or (string= sn x)
                (> (vas-string-metrics:jaro-winkler-distance sn x) 0.9))))
        candidates-list)))))

;;-------------------------------------------------------------------------

(declaim (inline a-get))
(defun a-get (name list)
  (assocr name list))

(declaim (inline a-get1))
(defun a-get1 (name list)
  (first (assocr name list)))

(defmacro a-add (name value list-place)
  `(acons ,name
          (cons ,value (assocr ,name ,list-place))
          ,list-place))


(defmacro a-set (name value list-place)
  (let ((g-list-place (gensym "list-place")))
    `(let ((,g-list-place (remove ,name ,list-place :key #'first)))
       (acons ,name (list ,value) ,g-list-place))))

(defmacro a-remove-all (name list-place)
  `(remove ,name ,list-place :key #'first))

;;-------------------------------------------------------------------------

(defmacro %peek (thing)
  (let ((gthing (gensym "thing")))
    `(let ((,gthing ,thing))
       (break "Lisp Peek:~%:obj ~s~%:type ~s"
              ,gthing
              (type-of ,gthing))
       ,gthing)))
