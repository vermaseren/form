TEXFILES = \
	bracket.tex \
	calculus.tex \
	dollar.tex \
	external.tex \
	functions.tex \
	gamma.tex \
	manual.tex \
	metric.tex \
	module.tex \
	parallel.tex \
	pattern.tex \
	poly.tex \
	prepro.tex \
	setup.tex \
	sorting.tex \
	startup.tex \
	statements.tex \
	tablebas.tex \
	variable.tex

MAIN = manual

.PHONY: FORCE dvi html ps pdf clean-html clean-local

DATEFILE = manualdate.tex
$(DATEFILE): FORCE
	OLDDATE=0; if [ -f $(DATEFILE) ]; then OLDDATE=$$(cat $(DATEFILE)); OLDDATE=$$(date -d "$$OLDDATE" +%Y%m%d); fi; \
	NEWDATE=0; if [ "x$(DATE)" != x ]; then NEWDATE=$(DATE); else NEWDATE=$$(date +"%d %b %Y"); fi; \
	if [ $$(date -d "$$NEWDATE" +%Y%m%d) -gt $$OLDDATE ]; then echo $$NEWDATE > $(DATEFILE); echo DIDIT; fi
FORCE:

HTMLCLEANFILES = idxmake.dvi idxmake.log $(addprefix $(MAIN),.4ct .4dx .4ix .4tc .aux .css .dvi .html .idv .idx .ilg .ind .lg .log .tmp .xref)
CLEANFILES = $(MAIN).pdf $(MAIN).ps $(MAIN).toc $(DATEFILE) texput.log $(HTMLCLEANFILES)
clean-local:
	rm -rf html

#################### CONFIG_TEX
if CONFIG_TEX

dvi: $(DATEFILE) $(MAIN).dvi

if CONFIG_MAKEINDEX
$(MAIN).dvi: $(DATEFILE) $(TEXFILES)
	@set -e ;\
	${LATEX} $(MAIN).tex; while [ `cat $(MAIN).log | grep -c Rerun` -gt 0 ]; do ${LATEX} $(MAIN).tex; done; \
	${MAKEINDEX} $(MAIN); \
	${LATEX} $(MAIN).tex; while [ `cat $(MAIN).log | grep -c Rerun` -gt 0 ]; do ${LATEX} $(MAIN).tex; done
else
$(MAIN).dvi: $(DATEFILE) $(TEXFILES)
	@set -e ;\
	${LATEX} $(MAIN).tex; while [ `cat $(MAIN).log | grep -c Rerun` -gt 0 ]; do ${LATEX} $(MAIN).tex; done
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
	${PDFLATEX} $(MAIN).tex; while [ `cat $(MAIN).log | grep -c Rerun` -gt 0 ]; do ${PDFLATEX} $(MAIN).tex; done; \
	${MAKEINDEX} $(MAIN); \
	${PDFLATEX} $(MAIN).tex; while [ `cat $(MAIN).log | grep -c Rerun` -gt 0 ]; do ${PDFLATEX} $(MAIN).tex; done
else
$(MAIN).pdf: $(DATEFILE) $(TEXFILES)
	@set -e ;\
	${PDFLATEX} $(MAIN).tex; while [ `cat $(MAIN).log | grep -c Rerun` -gt 0 ]; do ${PDFLATEX} $(MAIN).tex; done
endif

endif
########## CONFIG_PDF

endif
#################### CONFIG_TEX
