


;;;###autoload
(defun python-hook ()
  (setq-default python-shell-interpreter "python3")
  (setq python-indent-guess-indent-offset-verbose nil)
  (message "Python-hook loaded"))
