(elpaca-leaf paredit
  :hook
  ((
    emacs-lisp-mode-hook
    ielm-mode-hook
    lisp-interaction-mode-hook
    lisp-mode-hook
    scheme-mode-hook
    cider-repl-mode-hook
    )
   . paredit-mode)
  :bind (("C-M-u" . paredit-backward-up)
	 ("C-M-n" . paredit-forward-up)
	 ("C-)" . paredit-forward-slurp-sexp)
	 ("C-}" . paredit-forward-barf-sexp)
	 ("C-(" . paredit-backward-slurp-sexp)
	 ("C-{" . paredit-backward-barf-sexp)
	 ("M-S" . paredit-splice-sexp-killing-backward)
	 ("M-R" . paredit-raise-sexp)))

(provide 'init-lisp)
