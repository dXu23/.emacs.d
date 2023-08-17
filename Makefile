.POSIX:

EMACS=emacs

# Generates loaddefs.el from autoloads comments
lisp/loaddefs.el: lisp/hooks.el
	cd lisp && $(EMACS) -Q -batch -f loaddefs-generate-batch loaddefs.el . modes

install:
	install -m 755 -t $HOME/.config/systemd/user/emacs.service emacs.service

clean:
	rm lisp/loaddefs.el
