(in-package :varjo)

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

(defmacro pipe-> (args &body stages)
  "\(pipe-> \(1 2 3\) #'a #'b #'c #'d\)
   Calls first function with args provided and uses result as 
   arguments for next function. Uses multiple-value-call so you
   can use (values) to specify complex lambda-args."
  (let ((stages (reverse stages)))
    (when stages
      (let ((stage (first stages)))
        `(multiple-value-call ,stage
           ,(if (rest stages)
                `(pipe-> ,args ,@(reverse (rest stages)))
                `(values ,@args)))))))

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
    (position-if #'(lambda (x) (when (symbolp x) (equal (symbol-name x) symb-name))) list)))

(defmacro assocr (item alist &key (key nil keyp) 
                               (test nil testp) 
                               (test-not nil notp))
  `(cdr (assoc ,item ,alist 
               ,@(when keyp (list :key key)) 
               ,@(when testp (list :test test))
               ,@(when notp (list test-not)))))

(defun lists-contain-duplicates-p (&rest lists)
  (let ((joined (apply #'append lists)))
    (not (eq (length joined) (length (remove-duplicates joined))))))
