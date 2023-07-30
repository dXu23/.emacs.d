.POSIX:

EMACS=emacs

# Generates loaddefs.el from autoloads comments
lisp/loaddefs.el: lisp/hooks.el
	cd lisp && $(EMACS) -Q -batch -f loaddefs-generate-batch loaddefs.el . modes

clean:
	rm lisp/loaddefs.el
