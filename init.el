;;; init.el --- Entry point for my Emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;; init.el contains basic settings, as well as package declarations
;; that are not applicable to any one specific mode associated with
;; a filetype (e.g. Vertico). Modes will be stored in /lisp/modes,
;; with extensions/settings associated with built-in packages in
;; /lisp (e.g. /lisp/org-settings.el).

(require 'seq)

;;; Set emacs init file
;;; Code:

(defconst *emacs-init-file*
  (locate-user-emacs-file "init.el"))

;;; Visit config file
(defun config-visit ()
  "Visits config file, specified by *emacs-init-file."
  (interactive)
  (find-file *emacs-init-file*))

(global-set-key (kbd "C-c e") 'config-visit)

;;; Reload config file
(defun config-reload ()
  "Reloads config, specified by *emacs-init-file*."
  (interactive)
  (load *emacs-init-file*))

(global-set-key (kbd "C-c r") 'config-reload)

;;; Basic Settings
(setq inhibit-startup-message t)

(repeat-mode 1)

(setq create-lockfiles nil)

(setq
 make-backup-files t
 backup-by-copying nil
 version-control t
 keep-old-versions 10000
 ketp-new-versions kept-old-versions
 backup-directory-alist
   `("." . ,(expand-file-name "~/.backups")))

; Allows user to type y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)

(setq gc-cons-threshold 50000000)

(setq large-file-warning-threshold 100000000)

(save-place-mode 1)

; Get rid of all bars
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

; Fill column marker for line wraps
(setq fill-column 80)

; Buffer related settings
(setq switch-to-buffer-in-dedicated-window 'pop
      switch-to-buffer-obey-display-actions t)


;; Relative line numbers
(require 'display-line-numbers)

(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode help-mode paradox-mode comint-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
Exempt modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
	      (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))

(global-display-line-numbers-mode 1)

;; Highlight patterns found in file automatically
(global-hi-lock-mode 1)
(setq hi-lock-file-patterns-policy #'(lambda (dummy) t))

;;; Change modeline
(column-number-mode 1)
(setq mode-line-position
	'((line-number-mode ("%l" (column-number-mode ":%c")))))

(defun shorten-directory (dir max-length)
  "Show up to MAX-LENGTH characters of a directory name DIR."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
	  (output ""))
    (when (and path (equal "" (car path)))
	(setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
	(setq output (concat (car path) "/" output))
	(setq path (cdr path)))
    (when path
	(setq output (concat ".../" output)))
    output))

(defvar mode-line-directory
  '(:propertize
    (:eval (if (buffer-file-name)
	       (concat " " (shorten-directory default-directory 20)) " ")))
  "Formats the current directory.")

(put 'mode-line-directory 'risky-local-variable t)

(setq-default mode-line-buffer-identification
		(propertized-buffer-identification "%b "))

(setq-default mode-line-format
		'("%e"
		  mode-line-front-space
		  ;; mode-line-mule-info --
		  mode-line-client
		  mode-line-modified
		  ;; mode-line-remote -- no need to indicate this specially
		  ;; mode-line-frame-identification
		  " "
		  mode-line-directory
		  mode-line-buffer-identication
		  " "
		  mode-line-position
		  (flycheck-mode flycheck-mode-line)
		  " "
		  mode-line-modes
		  mode-line-misc-info
		  mode-line-end-spaces))

;;; load modus theme
(setq modus-themes-mode-line '(accented borderless padded)
      modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-fringes 'subtle
      modus-themes-tabs-accented t
      modus-themes-paren-match '(bold intense)
      modus-themes-prompts '(bold intense)
      modus-themes-completions
      '((matches . (extrabold))
      	(selection . (semibold italic text-also)))
      modus-themes-org-blocks 'tinted-background
      modus-themes-common-palete-overrides
      '((date-deadline magenta-warmer)
        (date-scheduled green-cooler)
        (date-weekday fg-main)
        (date-event fg-dim)
        (date-now blue)
        (prose-done fg-alt)
        (prose-todo yellow))
      modus-themes-region '(bg-only)
      modus-themes-syntax '(alt-syntax)
      modus-themes-headings
      '((1 . (rainbow overline background 1.4))
      	(2 . (rainbow background 1.3))
      	(3 . (rainbow bold 1.2))
      	(t . (semilight 1.1)))
      modus-themes-scale-headings t)

(load-theme 'modus-vivendi t)

;;; Set font
;; (let ((default-font
;;        (pcase system-type
;; 	 ('gnu/linux "Liberation Sans 16")
;; 	 ('darwin "Avenir 16")
;; 	 ((or 'windows-nt 'cygwin) "Corbel 16")
;; 	 (_ "Verdana"))))
;;   (cond
;;    ((find-font (font-spec :name "Fira Code"))
;;     (add-to-list 'default-frame-alist '(font . "Fira Code 16")))
;;    (t (set-frame-font default-font nil t))
;;    )
;;   )

; Necessary for Emacs client to work.
(add-to-list 'default-frame-alist '(font . "Fira Code 16"))

;;; Regex
(require 're-builder)
(setq reb-re-syntax 'string)

(add-hook 'elpaca-after-init-hook
	  (lambda ()
	    (message "Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract (current-time) before-init-time)))
		     gcs-done)))


;;; Add some subdirectories to load-path
(add-to-list 'load-path (concat (file-name-as-directory user-emacs-directory) "etc"))

(let ((default-directory (concat (file-name-as-directory user-emacs-directory) "lisp")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;;; Bootstrap Elpaca package manager
(defvar elpaca-installer-version 0.3)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))



(defmacro elpaca-leaf (order &rest body)
  "Execute BODY in `leaf' declaration after ORDER is finished.
If the :disabled keyword is present in body, the package is completely ignored.
This happens regardless of the value associated with :disabled.
The expansion is a string indicating the package has been disabled."
  (declare (indent 1))
  (if (memq :disabled body)
      (format "%S :disabled by elpaca-leaf" order)
    (let ((o order))
      (when-let ((ensure (seq-position body :ensure)))
	(setq o (if (null (nth (1+ ensure) body)) nil order)
	      body (append (seq-subseq body 0 ensure)
			   (seq-subseq body (+ ensure 2)))))
      `(elpaca ,o (leaf
		   ,(if-let (((memq (car-safe order) '(quote \`)))
			     (feature (flatten-tree order)))
			(cadr feature)
		      (elpaca--first order))
		   ,@body)))))

(elpaca leaf (require 'leaf))
(elpaca leaf-keywords (require 'leaf-keywords))

(elpaca-leaf nil
	     :init (leaf leaf-keywords
			 :init (leaf-keywords-init)))


;;; Vertical completion buffers with Vertico
(elpaca-leaf vertico
  :ensure t
  :custom
  (vertico-count . 13)
  (vertico-resize . t)
  (vertico-cycle . nil)
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)
	 (minibuffer-setup . vertico-repeat-save))
  :advice
  (:around vertico--format-candidate
	   (lambda (orig cand prefix suffix index _start)
	     (setq cand (funcall orig cand prefix suffix index _start))
	     (concat
	      (if (= vertico--index index)
		  (propertize "» " 'face 'vertico-current)
		"  ")
	      cand)))
  :init
  (vertico-mode))

;;;; Add annotations to completion buffers with Marginalia
(elpaca-leaf marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-max-relative-age . 0)
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators light nil))
  (marginalia-cycle . 'right)
  :init
  (marginalia-mode))

;;;; Get orderless
(elpaca-leaf orderless
  :custom
  (completion-styles . '(orderless))
  (completion-category-defaults . nil)
  (completion-category-overrides .
   '((file (styles basic partial-completion orderless))))
  (orderless-component-separator . 'orderless-escapable-split-on-space)
  (orderless-matching-styles .
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp)))

(elpaca-leaf corfu
  :bind
  (:corfu-map
   ("C-n" . corfu-next)
   ("C-p" . corfu-previous)
   ("<escape>" . corfu-quit)
   ("<return>" . corfu-insert)
   ("M-d" . corfu-show-documentation)
   ("M-l" . corfu-show-location))
  :custom
  (corfu-auto . nil)
  (corfu-auto-prefix . 2)
  (corfu-auto-delay . 0.25)
  (corfu-min-width . 80)
  (corfu-max-width . corfu-min-width)
  (corfu-count . 14)
  (corfu-scroll-margin . 4)
  (corfu-cycle . nil)
  (corfu-preselect-first . t)
  (corfu-echo-documentation . t)
  (corfu-quit-at-boundary . 'separator)
  (corfu-separator ?\s)
  (corfu-quit-no-match . 'separator)
  (corfu-preview-current . 'insert)
  (corfu-on-exact-match . nil)
  :init
  (corfu-global-mode)
)

(elpaca-leaf avy
  :bind
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("M-g f" . avy-goto-line)
  ("M-g w" . avy-goto-word-1)
  ("M-g e" . avy-goto-word-0)
  ("C-c C-j" . avy-resume)
  )

(elpaca-leaf magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch)
  :custom
  (git-commit-summary-max-length . 50)
  (git-commit-fill-column . 72))

(require 'treesit)

;;; Load modes with entrypoint autoloaded
(load "loaddefs" nil t)
(require 'hooks)

;;; Org mode GTD settings
(require 'org-settings)

;;; Stats related packages
(require 'stats)



;;; init.el ends here
;;
;; Local Variables:
;; eval: (outline-minor-mode 1)
;; End:
