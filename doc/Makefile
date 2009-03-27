SHELL=/bin/bash

HTML_DIR=../build/html

SOURCES=$(wildcard *.md)
TARGETS=$(patsubst %.md, $(HTML_DIR)/%.html,$(SOURCES))

all: $(HTML_DIR) $(TARGETS)

$(HTML_DIR):
	mkdir -p $@

$(HTML_DIR)/%.html: %.md header.html buildtoc.py footer.html
	(title=`grep '^# ' $< | head -1 | sed -e 's:^# ::'` ;\
	 t=/tmp/$*.md ;\
	 sed -e "s:@TITLE@:$$title:g" < header.html > $@ ;\
	 python buildtoc.py < $< > $$t ;\
	 markdown $$t >> $@ ;\
	 rm $$t ;\
	 cat footer.html >> $@)

clean:
	rm -f $(HTML_DIR)/*.html
