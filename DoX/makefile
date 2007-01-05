#!/bin/make

.SUFFIXES:
.SUFFIXES: .html .md

OBJECTS = FoX.html Versioning.html Compilation.html Embedding.html FoX_common.html StringFormatting.html FoX_wxml.html FoX_wcml.html Debugging.html Information.html Licensing.html FoX_sax.html AttributeDictionaries.html

default: FoX_DoX.html
	ln -sf FoX.html index.html

.md.html: head tail
	sed -e 's/TITLE/$*/' head > $@; sed -e 's/|\(.*\)|/\1.html/g' $< | markdown >> $@; cat tail >> $@

FoX_DoX.html: $(OBJECTS)
	sed -e 's/TITLE/$*/' head > "$@"
	for i in $(OBJECTS); do echo '<a name="'$${i%.html}'"/>' >> $@; sed -e 's/|\(.*\)|/#\1/g' $${i%.html}.md | markdown >> $@; echo '</div><hr/><div class="DoX"/>' >> $@; done
	cat tail >> $@


clean:
	rm *.html
