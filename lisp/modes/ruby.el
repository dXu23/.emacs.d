(require 'project)

(setq inf-ruby-console-environment "development")

(elpaca-leaf inf-ruby
  :ensure t
  :hook
  (ruby-ts-mode . inf-ruby-minor-mode)
  )


;;;###autoload
(defun dx-ruby-hook ()
  (setq-local tab-width 2)
  (ruby-ts-mode))

(add-to-list 'auto-mode-alist '(".rb$". ruby-mode))
