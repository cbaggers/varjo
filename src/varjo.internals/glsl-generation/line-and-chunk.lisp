(in-package :varjo.internals)
(in-readtable :fn.reader)

;;------------------------------------------------------------
;; GLSL Lines & Chunks
;;
;; These objects allow us to handle indentation & composition easily
;; they certainly could be faster, but even now they provide a decent
;; enough way to manage the glsl.
;;
;; The current-line in objects of the 'compiled' type are still just
;; strings as this makes the composition easier, glsl-line is only used
;; when the line is complete and you are going to store it in a chunk.

(defstruct (glsl-line (:constructor %make-glsl-line))
  (indentation 0 :type (unsigned-byte 16))
  (string-part (error "string-part of glsl-line must be provided")
               :type string))

(declaim (ftype (function (string &rest t) glsl-line)
                glsl-line)
         (inline glsl-line))
(defun glsl-line (control-string &rest args)
  (declare (type string control-string))
  (%make-glsl-line :string-part (apply #'format nil control-string args)))

(defstruct (glsl-chunk (:constructor %make-glsl-chunk))
  (lines nil :type list))

(defun glsl-chunk (&rest lines)
  (assert (every #'glsl-line-p lines))
  (%make-glsl-chunk :lines lines))

(defun glsl-chunk* (lines)
  (assert (every #'glsl-line-p lines))
  (%make-glsl-chunk :lines lines))

(defun indent (glsl &optional (count 1))
  (etypecase glsl
    (null nil)
    (glsl-chunk
     (map nil (lambda (x) (indent x count))
          (glsl-chunk-lines glsl))
     glsl)
    (glsl-line
     (incf (glsl-line-indentation glsl) count)
     glsl)))

(defvar *indent-length* 4)

(defun glsl-chunk-emptyp (chunk)
  (if chunk
      (null (glsl-chunk-lines chunk))
      t))

(defun glsl-len (glsl)
  (etypecase glsl
    (glsl-line (+ (* (glsl-line-indentation glsl) *indent-length*)
                  (length (glsl-line-string-part glsl))
                  1))
    (glsl-chunk (loop :for line :in (glsl-chunk-lines glsl)
                   :summing (glsl-len line)))))

(defun join-glsl-chunks (chunks)
  (assert (every λ(or (null _) (glsl-chunk-p _)) chunks))
  (%make-glsl-chunk
   :lines (loop :for chunk :in chunks
             :append (when chunk
                       (glsl-chunk-lines chunk)))))

(defun join-glsl-of-compiled (compiled-objs)
  (assert (every λ(typep _ 'compiled) compiled-objs))
  (%make-glsl-chunk
   :lines (loop :for obj :in compiled-objs
             :for to-block := (to-block obj)
             :for line := (current-line obj)
             :append (when to-block (glsl-chunk-lines to-block))
             :when line
             :collect (glsl-line (end-line-str (current-line obj))))))

(defmacro glsl-chunk-splicing (&body key-form-pairs)
  (let ((gform (gensym "form")))
    `(%make-glsl-chunk
      :lines
      (append
       ,@(loop :for (key form) :on key-form-pairs :by #'cddr :collect
            (ecase key
              (:line `(let ((,gform ,form))
                        (when ,gform
                          (assert (typep ,gform 'glsl-line))
                          (list ,gform))))
              (:chunk `(let ((,gform ,form))
                         (when ,gform
                           (assert (typep ,gform 'glsl-chunk))
                           (glsl-chunk-lines ,gform))))))))))

(defun glsl-chunk-from-compiled (obj)
  (assert (typep obj 'compiled))
  (let ((to-block (to-block obj))
        (line (current-line obj)))
    (%make-glsl-chunk
     :lines (if to-block
                (append (glsl-chunk-lines (to-block obj))
                        (when line
                          (list (glsl-line (end-line-str line)))))
                (when line
                  (list (glsl-line (end-line-str line))))))))

(defun glsl-chunk-to-string (chunk)
  (assert (typep chunk 'glsl-chunk))
  (let* ((len (glsl-len chunk))
         (indent-len *indent-length*)
         (string (make-string len :initial-element #\space))
         (pos 0))
    (loop :for line :in (glsl-chunk-lines chunk) :do
       (incf pos (* (glsl-line-indentation line) indent-len))
       (loop :for char :across (glsl-line-string-part line) :do
          (setf (aref string pos) char)
          (incf pos))
       (setf (aref string pos) #\Newline)
       (incf pos))
    string))

;;------------------------------------------------------------
