# make sure this is emacs23
EMACS=$(shell which emacs-snapshot || which emacs)
DIA=dia
all: getting_started_dev.html bitmaps
setup:
	sudo aptitude install emacs-snapshot emacs-goodies js2-mode graphicsmagick dia

getting_started_dev.html: getting_started_dev.org stylesheet.css
	$(EMACS) --batch -l ../share/emacs/io-mode.el -l org --visit getting_started_dev.org --funcall org-export-as-html-batch

bitmaps: *.dia
	for f in $?; do \
		dia "$$f" --export=`basename $$f .dia`.png; \
		gm mogrify -resize '50%' `basename $$f .dia`.png; \
	done
	gm mogrify -crop 'x339' flow-simplified.png

.PHONY: setup all bitmaps

clean:
	rm -f getting_started_dev.html
	rm -f *.png
