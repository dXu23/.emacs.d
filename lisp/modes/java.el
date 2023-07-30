(require 'cc-mode)

;;;###autoload
(defun my/java-hook ()
  (hs-minor-mode)
  (setq c-basic-offset 4)
  (glasses-mode 1))

(provide 'my/java)
