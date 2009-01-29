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

DATEFILE = manualdate.tex
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

########## CONFIG_LATEX2HTML
if CONFIG_LATEX2HTML

html: $(DATEFILE) html/$(MAIN).html

html/$(MAIN).html: $(DATEFILE) $(MAIN).aux
	mkdir -p html; \
	$(LATEX2HTML) -no_auto_link -dir html -no_navigation -split 0 -info 0 -no_address $(MAIN).tex

$(MAIN).aux: $(DATEFILE) $(MAIN).dvi

endif
########## CONFIG_LATEX2HTML

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
