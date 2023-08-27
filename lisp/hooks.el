(add-hook 'java-mode-hook 'my/java-setup)
(add-hook 'scheme-mode-hook 'my/scheme-setup)
(add-hook 'python-mode-hook 'my/python-setup)
(add-hook 'c-ts-mode-hook #'my/c-setup)

(provide 'hooks)
