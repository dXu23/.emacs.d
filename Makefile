.POSIX:

EMACS=emacs

# Generates loaddefs.el from autoloads comments
lisp/loaddefs.el: lisp/hooks.el
	cd lisp && $(EMACS) -Q -batch -f loaddefs-generate-batch loaddefs.el . modes

install:
	mkdir -pv ${HOME}/.config/systemd/user ${HOME}/.config/environment.d && \
	install -m 755 -t ${HOME}/.config/systemd/user/ emacs.service && \
	install -m 755 -t ${HOME}/.config/environment.d/ 60-emacs.conf

clean:
	rm lisp/loaddefs.el
