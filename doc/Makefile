# make sure this is emacs23
EMACS=$(shell which emacs-snapshot || which emacs)
all: getting_started_dev.html
setup:
	sudo aptitude install emacs-snapshot emacs-goodies js2-mode
.PHONY: setup
getting_started_dev.html: getting_started_dev.org stylesheet.css
	$(EMACS) --batch -l org --visit getting_started_dev.org --funcall org-export-as-html-batch
