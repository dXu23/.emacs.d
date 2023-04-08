;;; org-settings.el --- Settigs for GTD and org mode -*- lexical-binding: t; eval: (outline-minor-mode 1) -*-

;; Many thanks to the following Emacs content creators
;; and their articles/videos
;;
;; Nicholas P. Rougier
;; Website: <https://www.labri.fr/perso/nrougier/>
;; Article: <https://www.labri.fr/perso/nrougier/GTD/index.html>
;;
;; Ben J. Maughan
;; Website: <https://pragmaticemacs.wordpress.com>
;; Article: <https://pragmaticemacs.wordpress.com/2015/12/08/org-mode-basics-vii-a-todo-list-with-schedules-and-deadlines/>

;;; Requires
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-protocol)

;;; GTD Org Mode Paths
(setq org-directory "~/Documents/org")

;;; Set org-mode agenda files
(setq org-agenda-files
      (mapcar 'file-truename
	      (file-expand-wildcards (expand-file-name "*.org" org-directory))))

;;; Capturing
(setq org-capture-templates `(("t" "Todo [inbox]" entry
			       (file+headline "inbox.org" "Tasks")
			       ,(concat "* TODO [#A] %i%?\n"
					"SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n"))
			      ("T" "Tickler" entry
			       (file+headline "tickler.org" "Tickler")
			       "* %i%? \n %U")
			      ("p" "Protocol" entry
			       (file+headline "refile.org" "Notes")
			       ,(concat "* %:description :RESEARCH:\n"
					"#+BEGIN_QUOTE\n"
					"%i\n\n -- %:link %u\n"
					"#+END_QUOTE\n\n%?"))
			      ("L" "Protocol Link" entry
			       (file+headline "refile.org" "Notes")
			       "* %? [[%:link][%:description]] \nCaptured On: %u")
			      ("@" "Inbox [mu4e]" entry (file "inbox.org")
			       ,(concat "* TODO Process \"%a\" %?\n"
					"/Entered on/ %U"))
			      ("m" "Meeting" entry
			       (file+headline "agenda.org" "Future")
			       ,(concat "* %? :meeting:\n"
					"<%<%Y-%m-%d %a %H:00>>")
			       ("n" "Note" entry
				(file "notes.org")
				,(concat "* Note (%a)\n"
					 "/Entered on/ %U\n" "\n" "%?")))))


(defun org-capture-inbox ()
  "Captures a todo and puts it into inbox.org"
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))

(defun org-capture-mail ()
  "Creates and captures a reply todo from email and puts it into inbox.org"
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "@"))

;; Use full window for org-capture
(add-hook 'org-capture-mode-hook 'delete-other-windows)

;;; Org mode keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c i") 'org-capture-inbox)
(global-set-key (kbd "C-c b") 'org-switchb)

;;; Refiling
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")
			   ("someday.org" :level . 1)
			   ("tickler.org" :maxlevel . 2)))


(defun gtd-save-org-buffers ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers
   t
   (lambda ()
     (when (member (buffer-file-name) org-agenda-files)
       t)))
  (message "Saving org-agenda-files buffers... done"))


;; Add it after refile
(advice-add 'org-refile :after
	    (lambda (&rest _)
	      (gtd-save-org-buffers)))

;;;; Set org todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t@/!)" "NEXT(n)" "SOMEDAY(s)" "PROJ(p)" "WAITING(w@/!)" "|"
		  "DONE(d@/!)" "CANCELED(c)")))

(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
	     (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

;;; Agenda
(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
	 ((agenda ""
		  ((org-agenda-skip-function
		    '(org-agenda-skip-entry-if 'deadline))
		   (org-deadline-warning-days 0)))
	  (todo "NEXT"
		((org-agenda-skip-function
		  '(org-agenda-skip-entry-if 'deadline))
		 (org-agenda-prefix-format " %i %-12:c [%e] ")
		 (org-agenda-overriding-header "\nTasks\n")))
	  (agenda nil
		  ((org-agenda-entry-types '(:deadline))
		   (org-agenda-format-date "")
		   (org-deadline-warning-days 7)
		   (org-agenda-skip-function
		    '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
		   (org-agenda-overriding-header "\nDeadlines")))
	  (tags-todo "inbox"
		     ((org-agenda-prefix-format "  %?-12t% s")
		      (org-agenda-overriding-header "\nInbox\n")))
	  (tags "CLOSED>=\"<today\""
		((org-agenda-overriding-header "\nCompleted today\n")))))
	("u" "Unscheduled tasks" alltodo ""
	  (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>")
	  (org-agenda-overriding-header "Unscheduled TODO entries: "))
	("W" "Weekly Review"
	  ((agenda "" ((org-agenda-span 7)))
	   (stuck "")
	   (todo "PROJECT")
	   (todo "MAYBE")
	   (todo "WAITING")))))

;;; Org agenda

(setq org-agenda-hide-tags-regexp ".")

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
	(todo   . " ")
	(tags   . " %i %-12:c")
	(search . " %i %-12:c")))


;;; Basic Org Settings

;; Set priority range from A to C with default A
(setq org-highest-priority ?A
      org-lowest-priority ?C
      org-default-priority ?A)

;; Set org-log-done to true
(setq org-log-done 'note)

;; Open org agenda in current window
(setq org-agenda-window-setup (quote current-window))

;; Hide redundant tags in org-agenda
(setq org-agenda-hide-tags-regexp ".")

;; Warn about any deadline in 7 days
(setq org-deadline-warning-days 7)

;; Show tasks schedules/due in next fornight
(setq org-agenda-span 'fortnight)

;; Do not show tasks as scheduled if already shown as deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)

;; Do not give warning colors to tasks w/ impending deadlines
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

;;;; Turn off org-goto-auto-isearch
(setq org-goto-auto-isearch nil)

(setq org-protocol-default-template-key "1")

(provide 'org-settings)
