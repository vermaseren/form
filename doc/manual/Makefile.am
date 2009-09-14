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

.PHONY: FORCE dvi html ps pdf clean-local

DATEFILE = manualdate.tex
$(DATEFILE): FORCE
	if [ "x$(DATE)" != x ]; then echo $(DATE) > $(DATEFILE); else echo `date +"%d %b %Y"` > $(DATEFILE); fi
FORCE:

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

########## CONFIG_LATEX2HTML
if CONFIG_LATEX2HTML

$(MAIN).aux: $(DATEFILE) $(MAIN).dvi

html: $(DATEFILE) html/$(MAIN).html

html/$(MAIN).html: $(DATEFILE) $(MAIN).aux
	mkdir -p html; \
	$(LATEX2HTML) -tex_defs -html_version 4.0,math,table -no_auto_link -dir html -no_navigation -split 0 -info 0 -no_address $(MAIN)
#  htlatex manual "xhtml,mathml-,index=2,0" "" "-cvalidate"
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

CLEANFILES = $(addprefix $(MAIN),.ind .ilg .aux .dvi .idx .log .ps .pdf .toc) $(DATEFILE)

clean-local:
	rm -rf html
