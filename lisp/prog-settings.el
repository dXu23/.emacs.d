;;; prog-settings.el --- This file is for prog-mode configurations.

;;; Commentary:
;; 

(elpaca-leaf
    :custom
  (corfu-auto . nil)
  (corfu-auto-prefix . 2)
  (corfu-auto-delay 0.25)
  (coru-min-width . 80)
  (corfu-max-width . corfu-min-width)
  (corfu-count . 14)
  (corfu-scroll-margin . 4)
  (corfu-cycle . nil)
  (corfu-quit-at-boundary . nil)
  (corfu-preselect-first . t)
  (corfu-echo-documentation . t)
  )

;;; Code:

(provide 'prog-settings)

;;; prog-settings.el ends here
