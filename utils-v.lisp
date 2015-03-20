(in-package :varjo)

(defun cons-end (thing list)
  (concatenate 'list list (list thing)))

(defun listify (x) (if (listp x) x (list x)))

(defun lambda-list-get-names (l-list)
  (let ((keywords '(&allow-other-keys &environment &rest &aux &key &whole &body
                    &optional)))
    (loop :for i :in l-list
       :if (not (member i keywords))
       :collect (if (listp i) (first i) i))))

;; [TODO] fully implement positions-if to match position-if spec
;; [TODO] also add positions-if-not and positions: could be all be useful
(defun positions-if (predicate sequence)
  (loop :for element :in sequence :for i :from 0
     :if (funcall predicate element) :collect i))


(define-compiler-macro mapcat (function &rest lists)
  `(apply #'concatenate 'list (mapcar ,function ,@lists)))

(defun mapcat (function &rest lists)
  (apply #'concatenate 'list (apply #'mapcar function lists)))

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
  `(multiple-value-bind ,vars ,value-form ,@body))


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

;; [TODO] areas where this is used probably need that part extracted
(defun fmt (control-string &rest format-args)
  (apply #'format `(nil ,control-string ,@format-args)))

;; [TODO] is this used anywhere?
(defun print-hash (hash-table)
  (loop for x being the hash-keys of hash-table
     :do (print (format nil "~s -> ~s" x (gethash x hash-table))))
  hash-table)

;; [TODO] as with fmt
(defun printf (control-string &rest format-arguments)
  (apply #'format (append (list t control-string) format-arguments)))

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

;;[TODO] what is it used for?
(defun identity-filter (list t-map)
  (mapcan (lambda (x m) (when m (list x))) list t-map))

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

(defun sym-down (symbol)
  (p-symb (symbol-package symbol)
          (string-downcase (symbol-name symbol))))

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
          (error "'&' symbol found that was not specified in template ~s"
                 (mapcar #'first split))))))

(defun split-arguments (args &optional (template '(&uniform &context &instancing)))
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
