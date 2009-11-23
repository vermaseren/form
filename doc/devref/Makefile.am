TEXFILES = \
	cvs.tex \
	devref.tex \
	formrun.tex \
	indepth.tex \
	source.tex \
	testsuite.tex

MAIN = devref

.PHONY: FORCE dvi html ps pdf clean-local

DATEFILE = devrefdate.tex
$(DATEFILE): FORCE
	if [ "x$(DATE)" != x ]; then echo $(DATE) > $(DATEFILE); else echo `date +"%d %b %Y"` > $(DATEFILE); fi
FORCE:

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

CLEANFILES = $(MAIN).aux $(MAIN).dvi $(MAIN).idx $(MAIN).log $(MAIN).ps $(MAIN).pdf $(MAIN).toc html/* $(DATEFILE)

clean-local:
	rm -rf html
