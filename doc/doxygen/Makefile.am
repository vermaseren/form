######################################## CONFIG_DOXYGEN
if CONFIG_DOXYGEN

html: html/index.html

html/index.html:
	@echo "Running ${DOXYGEN} DoxyfileHTML ..."; \
	${DOXYGEN} DoxyfileHTML

#################### CONFIG_TEX
if CONFIG_TEX
if CONFIG_MAKEINDEX

dvi: doxygen.dvi

doxygen.dvi: latex/doxygen.dvi
	cp latex/doxygen.dvi doxygen.dvi

latex/doxygen.dvi: latex/doxygen.tex
	@set -e ;\
	cd latex; \
	${LATEX} doxygen.tex; \
	${MAKEINDEX} doxygen.idx; \
	${LATEX} doxygen.tex

latex/doxygen.tex:
	@echo "Running ${DOXYGEN} DoxyfileLATEX ..."; \
	${DOXYGEN} DoxyfileLATEX; \
	mv latex/refman.tex latex/doxygen.tex

########## CONFIG_PS
if CONFIG_PS

ps: doxygen.ps

doxygen.ps: latex/doxygen.ps
	cp latex/doxygen.ps doxygen.ps

latex/doxygen.ps: latex/doxygen.dvi
	@echo "Running ${DVIPS} -o doxygen.ps doxygen.dvi ..."; \
	cd latex; \
	${DVIPS} -o doxygen.ps doxygen.dvi

endif
########## CONFIG_PS

########## CONFIG_PDF
if CONFIG_PDF

pdf: doxygen.pdf

doxygen.pdf: pdflatex/doxygen.pdf
	cp pdflatex/doxygen.pdf doxygen.pdf

pdflatex/doxygen.pdf: pdflatex/doxygen.tex
	@set -e ; \
	cd pdflatex; \
	${PDFLATEX} doxygen.tex; \
	${MAKEINDEX} doxygen.idx; \
	${PDFLATEX} doxygen.tex

pdflatex/doxygen.tex:
	@echo "Running ${DOXYGEN} DoxyfilePDFLATEX ..."; \
	${DOXYGEN} DoxyfilePDFLATEX; \
	mv pdflatex/refman.tex pdflatex/doxygen.tex

endif
########## CONFIG_PDF

endif
endif 
#################### CONFIG_TEX

endif
######################################## CONFIG_DOXYGEN

CLEANFILES = doxygen.dvi doxygen.ps doxygen.pdf

clean-local:
	rm -rf latex pdflatex html
