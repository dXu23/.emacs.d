.POSIX:

EMACS=emacs

lisp/loaddefs.el: lisp/hooks.el
	cd lisp && $(EMACS) -Q -batch -f loaddefs-generate-batch loaddefs.el . modes
