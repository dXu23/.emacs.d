(require 'python)

(elpaca-leaf poetry
  :ensure t
  :custom
  (poetry-tracking-strategy . 'switch-buffer)
  :hook
  (python-mode . poetry-tracking-mode))
