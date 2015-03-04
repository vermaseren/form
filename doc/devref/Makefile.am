TEXFILES = \
	cvs.tex \
	devref.tex \
	formrun.tex \
	indepth.tex \
	source.tex \
	testsuite.tex

MAIN = devref

EXTRA_DIST = $(TEXFILES)

.PHONY: FORCE dvi html ps pdf clean-local

DATEFILE = devrefdate.tex
$(DATEFILE): FORCE
	$(SHELL) $(top_srcdir)/scripts/gendate.sh -t -r $(srcdir) -i $(srcdir)/$(DATEFILE).in -o $(DATEFILE) "$(DATE)"
FORCE:

dist-hook:
	$(SHELL) $(top_srcdir)/scripts/gendate.sh -t -r $(srcdir) -i $(srcdir)/$(DATEFILE).in -o $(distdir)/$(DATEFILE).in "$(DATE)"

CLEANFILES = \
	$(MAIN).4ct $(MAIN).4dx $(MAIN).4ix $(MAIN).4tc $(MAIN).aux $(MAIN).css $(MAIN).dvi \
	$(MAIN).html $(MAIN)2.html $(MAIN).idv $(MAIN).idx $(MAIN).ilg $(MAIN).ind $(MAIN).lg \
	$(MAIN).log $(MAIN).out $(MAIN).pdf $(MAIN).ps $(MAIN).tmp $(MAIN).toc $(MAIN).xref \
	idxmake.dvi idxmake.log texput.log $(DATEFILE)

clean-local:
	rm -rf html

#################### CONFIG_TEX
if CONFIG_TEX

dvi: $(DATEFILE) $(MAIN).dvi

$(MAIN).dvi: $(DATEFILE) $(TEXFILES)
	@set -e ;\
	${LATEX} $(MAIN).tex; \
	${LATEX} $(MAIN).tex

########## CONFIG_HTLATEX
if CONFIG_HTLATEX

$(MAIN).aux: $(DATEFILE) $(MAIN).dvi

html: $(DATEFILE) html/$(MAIN).html

html/$(MAIN).html: $(DATEFILE) $(MAIN).aux
	@set -e ;\
	mkdir -p html; \
	${HTLATEX} $(MAIN) "html,mathml-" "" "-dhtml/"

endif
########## CONFIG_HTLATEX

########## CONFIG_PS
if CONFIG_PS

ps: $(DATEFILE) $(MAIN).ps

$(MAIN).ps: $(DATEFILE) $(MAIN).dvi
	${DVIPS} -o $(MAIN).ps $(MAIN).dvi

endif
########## CONFIG_PS

########## CONFIG_PDF
if CONFIG_PS

pdf: $(DATEFILE) $(MAIN).pdf

$(MAIN).pdf: $(DATEFILE) $(TEXFILES)
	@set -e ;\
	${PDFLATEX} $(MAIN).tex; \
	${PDFLATEX} $(MAIN).tex

endif
########## CONFIG_PDF

endif
#################### CONFIG_TEX
