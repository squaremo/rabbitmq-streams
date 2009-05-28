# make sure this is emacs23
EMACS=$(shell which emacs-snapshot || which emacs)
DIA=dia
all: getting_started_dev.html bitmaps
setup:
	sudo aptitude install emacs-snapshot emacs-goodies js2-mode graphicsmagick dia
getting_started_dev.html: getting_started_dev.org stylesheet.css
	$(EMACS) --batch -l org --visit getting_started_dev.org --funcall org-export-as-html-batch

bitmaps: *.dia
	for f in $?; do \
		dia "$$f" --export=`basename $$f .dia`.png -s 714x495; \
	done
	gm mogrify -crop 714x339 flow-simplified.png

.PHONY: setup all bitmaps
