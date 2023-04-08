
(add-to-list 'major-mode-remap-alist
	     '(c-mode . c-ts-mode))

(defun find-header-files ()
  (let ((header-files nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#include \"\\([a-zA-Z0-9]+\.h\\)\"" nil t)
	(setq header-files (cons
			    (buffer-substring-no-properties (match-beginning 1) (match-end 1)) header-files)))
      )
    header-files))


;;;###autoload
(defun c-ts-setup ()
  "Sets up Treesitter for C."
  (setq-local electric-quote-comment nil)
  (setq-local electric-quote-string nil)
  (setq c-ts-mode-indent-offset 4
	c-ts-mode-indent-style 'k&r)
  (indent-tabs-mode)
  (treesit-font-lock-recompute-features
   '(function variable) '(definition)))
