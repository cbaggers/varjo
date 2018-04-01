;; Some hacky wip emacs intergration.. still feels good though :)
;; requires slime

(defun slime-vari-describe-symbol (symbol-name)
  "Describe the symbol at point."
  (interactive (list (slime-read-symbol-name "Describe symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (let ((pkg (slime-current-package)))
    (slime-eval-describe
     `(vari.cl::vari-describe ,symbol-name nil ,pkg))))

(define-key lisp-mode-map (kbd "C-c C-v C-v")
			    'slime-vari-describe-symbol)
