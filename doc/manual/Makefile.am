TEXSRC = \
	bracket.tex \
	calculus.tex \
	dict.tex \
	dollar.tex \
	external.tex \
	functions.tex \
	gamma.tex \
	metric.tex \
	module.tex \
	optim.tex \
	parallel.tex \
	pattern.tex \
	polynomials.tex \
	prepro.tex \
	setup.tex \
	sorting.tex \
	startup.tex \
	statements.tex \
	tablebas.tex \
	variable.tex

MAIN = manual

TEXFILES = $(TEXSRC) $(MAIN).tex

EXTRA_DIST = $(TEXSRC)

.PHONY: FORCE dvi html ps pdf clean-local

DATEFILE = manualdate.tex
$(DATEFILE): FORCE
	$(SHELL) $(top_srcdir)/scripts/gendate.sh -t $(srcdir)/$(DATEFILE).in $(DATEFILE) "$(DATE)"
FORCE:

dist-hook:
	$(SHELL) $(top_srcdir)/scripts/gendate.sh -t $(srcdir)/$(DATEFILE).in $(distdir)/$(DATEFILE).in "$(DATE)"

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

if CONFIG_MAKEINDEX
$(MAIN).dvi: $(DATEFILE) $(TEXFILES)
	@set -e ;\
	$(LATEX) $(MAIN).tex; while [ `grep -v 'Package: rerunfilecheck' $(MAIN).log | grep -c Rerun` -gt 0 ]; do $(LATEX) $(MAIN).tex; done; \
	${MAKEINDEX} $(MAIN); \
	$(LATEX) $(MAIN).tex; while [ `grep -v 'Package: rerunfilecheck' $(MAIN).log | grep -c Rerun` -gt 0 ]; do $(LATEX) $(MAIN).tex; done
else
$(MAIN).dvi: $(DATEFILE) $(TEXFILES)
	@set -e ;\
	$(LATEX) $(MAIN).tex; while [ `grep -v 'Package: rerunfilecheck' $(MAIN).log | grep -c Rerun` -gt 0 ]; do $(LATEX) $(MAIN).tex; done
endif

########## CONFIG_HTLATEX
if CONFIG_HTLATEX

$(MAIN).aux: $(DATEFILE) $(MAIN).dvi

html: $(DATEFILE) html/$(MAIN).html

if CONFIG_MAKEINDEX
html/$(MAIN).html: $(DATEFILE)
	@set -e ;\
	mkdir -p html; \
	${HTLATEX} $(MAIN) "html,mathml-" "" "-dhtml/"; \
	${TEX} '\def\filename{{$(MAIN)}{idx}{4dx}{ind}} \input  idxmake.4ht'; \
	${MAKEINDEX} -o $(MAIN).ind $(MAIN).4dx; \
	${HTLATEX} $(MAIN) "html,mathml-" "" "-dhtml/"; \
	sed -i 's/table.tabular {margin-left: auto; margin-right: auto;}/table.tabular {margin-left: inherit;}/' html/$(MAIN).css; \
	rm -f $(HTMLCLEANFILES)
else
html/$(MAIN).html: $(DATEFILE)
	@set -e ;\
	mkdir -p html; \
	${HTLATEX} $(MAIN) "html,mathml-" "" "-dhtml/"; \
	rm -f $(HTMLCLEANFILES)
endif

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
if CONFIG_PDF

pdf: $(DATEFILE) $(MAIN).pdf

if CONFIG_MAKEINDEX
$(MAIN).pdf: $(DATEFILE) $(TEXFILES)
	@set -e ;\
	$(PDFLATEX) $(MAIN).tex; while [ `grep -v 'Package: rerunfilecheck' $(MAIN).log | grep -c Rerun` -gt 0 ]; do $(PDFLATEX) $(MAIN).tex; done; \
	${MAKEINDEX} $(MAIN); \
	$(PDFLATEX) $(MAIN).tex; while [ `grep -v 'Package: rerunfilecheck' $(MAIN).log | grep -c Rerun` -gt 0 ]; do $(PDFLATEX) $(MAIN).tex; done
else
$(MAIN).pdf: $(DATEFILE) $(TEXFILES)
	@set -e ;\
	$(PDFLATEX) $(MAIN).tex; while [ `grep -v 'Package: rerunfilecheck' $(MAIN).log | grep -c Rerun` -gt 0 ]; do $(PDFLATEX) $(MAIN).tex; done
endif

endif
########## CONFIG_PDF

endif
#################### CONFIG_TEX
