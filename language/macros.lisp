;; This software is Copyright (c) 2012 Chris Bagley
;; (techsnuffle<at>gmail<dot>com)
;; Chris Bagley grants you the rights to
;; distribute and use this software as governed
;; by the terms of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :varjo)

(v-define-compiler-macro + (&rest numbers)
  (if (> (length numbers) 2)
      `(%+ ,(first numbers) (+ ,@(rest numbers)))
      `(%+ ,@numbers)))

(v-define-compiler-macro - (&rest numbers)
  (if (> (length numbers) 2)
      `(%- ,(first numbers) (- ,@(rest numbers)))
      `(%- ,@numbers)))

(v-define-compiler-macro * (&rest numbers)
  (if (> (length numbers) 2)
      `(%* ,(first numbers) (* ,@(rest numbers)))
      `(%* ,@numbers)))

(v-define-compiler-macro / (&rest numbers)
  (if (> (length numbers) 2)
      `(%/ ,(first numbers) (/ ,@(rest numbers)))
      `(%/ ,@numbers)))

(v-define-compiler-macro = (&rest numbers)
  (if (> (length numbers) 2)
      `(%= ,(first numbers) (= ,@(rest numbers)))
      `(%= ,@numbers)))
(v-define-compiler-macro eql (&rest numbers)
  (if (> (length numbers) 2)
      `(%eql ,(first numbers) (eql ,@(rest numbers)))
      `(%eql ,@numbers)))
(v-define-compiler-macro equal (&rest numbers)
  (if (> (length numbers) 2)
      `(%equal ,(first numbers) (equal ,@(rest numbers)))
      `(%equal ,@numbers)))

(v-define-compiler-macro > (&rest numbers)
  (if (> (length numbers) 2)
      `(%> ,(first numbers) (> ,@(rest numbers)))
      `(%> ,@numbers)))
(v-define-compiler-macro < (&rest numbers)
  (if (> (length numbers) 2)
      `(%< ,(first numbers) (< ,@(rest numbers)))
      `(%< ,@numbers)))
(v-define-compiler-macro >= (&rest numbers)
  (if (> (length numbers) 2)
      `(%>= ,(first numbers) (>= ,@(rest numbers)))
      `(%>= ,@numbers)))
(v-define-compiler-macro <= (&rest numbers)
  (if (> (length numbers) 2)
      `(%<= ,(first numbers) (<= ,@(rest numbers)))
      `(%<= ,@numbers)))

;; (defun segment-arg-list (list symbol)
;;   "Takes a form like \(+ 1 2 3 4\) and returns \(+ 1 \(+ 2 \(+ 3 4\)\)\)"
;;   (if (rest list) 
;;       (list symbol (first list) (segment-arg-list (rest list) symbol)) 
;;       (first list)))

;; (%vdefmacro v! t nil (&rest args)
;;   (let ((len (length args)))
;;     (when (or (>= len 2) (<= len 4)))
;;     `(%init-vec-or-mat ,(kwd (symb :vec (length args))) ,@args)))

;; (%vdefmacro m! t nil (&rest args)
;;   (let ((len (length args)))
;;     (if (or (eq len 4) (eq len 9) (eq len 16))
;;         `(%init-vec-or-mat ,(kwd (symb :mat (floor (sqrt len))))
;;                            ,@args)
;;         (error "Invalid number of arguemnts for matrix"))))

;; (%vdefmacro vec2 t nil (&rest args)
;;   `(%init-vec-or-mat :vec2 ,@args))

;; (%vdefmacro vec3 t nil (&rest args)
;;   `(%init-vec-or-mat :vec3 ,@args))

;; (%vdefmacro vec4 t nil (&rest args)
;;   `(%init-vec-or-mat :vec4 ,@args))

;; (%vdefmacro ivec2 t nil (&rest args)
;;   `(%init-vec-or-mat :ivec2 ,@args))

;; (%vdefmacro ivec3 t nil (&rest args)
;;   `(%init-vec-or-mat :ivec3 ,@args))

;; (%vdefmacro ivec4 t nil (&rest args)
;;   `(%init-vec-or-mat :ivec4 ,@args))

;; (%vdefmacro uvec2 t nil (&rest args)
;;   `(%init-vec-or-mat :uvec2 ,@args))

;; (%vdefmacro uvec3 t nil (&rest args)
;;   `(%init-vec-or-mat :uvec3 ,@args))

;; (%vdefmacro uvec4 t nil (&rest args)
;;   `(%init-vec-or-mat :uvec4 ,@args))

;; (%vdefmacro mat2 t nil (&rest args)
;;   `(%init-vec-or-mat :mat2 ,@args))

;; (%vdefmacro mat3 t nil (&rest args)
;;   `(%init-vec-or-mat :mat3 ,@args))

;; (%vdefmacro mat4 t nil (&rest args)
;;   `(%init-vec-or-mat :mat4 ,@args))

;; (%vdefmacro mat2x2 t nil (&rest args)
;;   `(%init-vec-or-mat :mat2x2 ,@args))

;; (%vdefmacro mat2x3 t nil (&rest args)
;;   `(%init-vec-or-mat :mat2x3 ,@args))

;; (%vdefmacro mat2x4 t nil (&rest args)
;;   `(%init-vec-or-mat :mat2x4 ,@args))

;; (%vdefmacro mat3x2 t nil (&rest args)
;;   `(%init-vec-or-mat :mat3x2 ,@args))

;; (%vdefmacro mat3x3 t nil (&rest args)
;;   `(%init-vec-or-mat :mat3x3 ,@args))

;; (%vdefmacro mat3x4 t nil (&rest args)
;;   `(%init-vec-or-mat :mat3x4 ,@args))

;; (%vdefmacro mat4x2 t nil (&rest args)
;;   `(%init-vec-or-mat :mat4x2 ,@args))

;; (%vdefmacro mat4x3 t nil (&rest args)
;;   `(%init-vec-or-mat :mat4x3 ,@args))

;; (%vdefmacro mat4x4 t nil (&rest args)
;;   `(%init-vec-or-mat :mat4x4 ,@args))

;; (%vdefmacro while t nil (test &rest body)
;;   `(while ,test (progn ,@body)))

;; (%vdefmacro let* t nil (bindings &rest body)
;;   (let* ((bindings (reverse bindings))
;;          (result `(let (,(first bindings))
;;                     ,@body)))
;;     (loop for binding in (rest bindings) do
;;          (setf result `(let (,binding) ,result)))
;;     result))


