
(add-to-list 'major-mode-remap-alist
	     '(c-mode . c-ts-mode))

;;;###autoload
(defun my/c-setup ()
  "Sets up Treesitter for C."
  (setq-local electric-quote-comment nil)
  (setq-local electric-quote-string nil)
  (setq c-ts-mode-indent-offset 4
	c-ts-mode-indent-style 'k&r)
  (indent-tabs-mode)
  (treesit-font-lock-recompute-features
   '(function variable) '(definition)))
