(require 'init-lisp)

(elpaca-leaf geiser
  :hook scheme-mode-hook
  :custom
  ((geiser-mit-binary . "/usr/bin/scheme")
   (geiser-repl-skip-version-check-p . 't)
   (geiser-active-implementations . '(mit))))

(elpaca-leaf geiser-mit)

(provide 'init-scheme)
