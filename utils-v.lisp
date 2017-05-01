(in-package :varjo)

(defun cons-end (thing list)
  (concatenate 'list list (list thing)))

(defun listify (x) (if (listp x) x (list x)))
(defun delistify (x) (if (listp x)
                         (progn
                           (assert (= (length x) 1))
                           (first x))
                         x))

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

(defmacro vbind* (var-value-form-pairs &body body)
  (let ((pairs (group var-value-form-pairs 2)))
    (reduce (lambda (accum x) `(vbind ,@x ,accum))
            pairs :initial-value `(progn ,@body))))

(defmacro dbind* (lambda-list-expr-pairs &body body)
  (let ((pairs (group lambda-list-expr-pairs 2)))
    (reduce (lambda (accum x) `(dbind ,@x ,accum))
            pairs :initial-value `(progn ,@body))))

(defmacro vlist (value-form)
  `(multiple-value-list ,value-form))

;; [TODO] should dissapear as refactor goes on
(defun acons-many (data a-list)
  (if data (let* ((func (first data))
                  (name (first func))
                  (body (second func)))
             (acons name (cons body (rest (assoc name a-list)))
                    (acons-many (rest data) a-list)))
      a-list))

(defun kwd (&rest args)
  (intern (format nil "~{~a~}" args) 'keyword))

;; [TODO] is this used anywhere?
(defun print-hash (hash-table)
  (loop for x being the hash-keys of hash-table
     :do (print (format nil "~s -> ~s" x (gethash x hash-table))))
  hash-table)

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

(defun symbol-name-equal (x y)
  (when (and (symbolp x) (symbolp y))
    (equal (symbol-name x) (symbol-name y))))

;;[TODO] why is this ever needed?
(defun truep (x) (not (null x)))

;;[TODO] these are candidates for loop always
(defun eqp! (x)
  (lambda (val) (eq val x)))

(defun eqlp! (x)
  (lambda (val) (eql val x)))

(defun equalp! (x)
  (lambda (val) (equal val x)))

(defun eq-elements (list)
  (or (null list) (every (eqp! (car list)) list)))

(defun eql-elements (list)
  (or (null list) (every (eqlp! (car list)) list)))

(defun equal-elements (list)
  (or (null list) (every (equalp! (car list)) list)))

(defun symbol-name-position (symbol list)
  (let ((symb-name (string-upcase symbol)))
    (position-if #'(lambda (x) (when (symbolp x)
                                 (equal (symbol-name x) symb-name)))
                 list)))

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

(defun list-contains-duplicates-p (list &key (key #'identity) (test #'eq))
  (loop :for i :in list :do
     (when (> (count i list :key key :test test) 1) (return t))))

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
                        &optional (template '(&uniform &context &instancing)))
  (let* ((split (lambda-list-split template args))
         (in-args (cdr (assoc nil split))))
    (cons in-args
          (loop :for kw :in template :collect
             (cdr (assoc (kwd kw) split))))))

(defun apply-tree (func tree)
  (multiple-value-bind (val changed) (funcall func tree)
    (if changed
        val
        (if (listp tree)
            (mapcar (lambda (x) (apply-tree func x)) tree)
            tree))))

(defun range (n) (loop for i below n collect i))

(defun lastr (x) (car (last x)))

(defmacro case-member (member-form &body cases)
  (let ((member-form (listify member-form)))
    `(cond
       ,@(loop :for (item . rest) :in cases :collect
            (if (string-equal item 'otherwise)
                `(t ,@rest)
                `((member ,item ,@member-form) ,@rest))))))

(defun n-of (thing count)
  (loop :for i :below count :collect thing))

(defun split-seq (predicate sequence &key keep-split)
  (let* ((start -1)
         (r (loop :for end = (position-if predicate sequence :start (1+ start))
               :collect (prog1 (subseq sequence (max 0 start) end)
                          (when end
                            (setf start
                                  (if keep-split
                                      end
                                      (1+ end)))))
               :while end)))
    (remove-if (lambda (x) (= 0 (length x))) r)))

(defmacro asserting (assert-forms error-form &rest error-args)
  `(let ,assert-forms
     (unless (and ,@(mapcar #'first assert-forms))
       ,(typecase
         error-form
         (symbol `(error ',error-form ,@error-args))
         (string `(error ',(format nil "~a~%~{~a~%~}" error-form
                                   (n-of "~@[~a~]" (length error-args)))
                         ,@(loop :for e :in error-args
                              :for f :in assert-forms :collect
                              `(unless ,(first f) ,e))))
         (otherwise (error "The error-form used in the asserting macro must be a symbol or a string"))))))

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
