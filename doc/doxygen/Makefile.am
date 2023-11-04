.PHONY: update_version

# Automatic versioning.

version.sh: update_version
	$(UPDATE_VERSION)

dist-hook:
	$(DISTHOOK_VERSION)

if FIXED_VERSION

UPDATE_VERSION = \
	[ -f version.sh ] || $(LN_S) "$(srcdir)/version.sh.in" version.sh

DISTHOOK_VERSION = \
	cp "$(srcdir)/version.sh.in" "$(distdir)/version.sh.in"

else

UPDATE_VERSION = \
	$(SHELL) "$(top_srcdir)/scripts/git-version-gen.sh" -C "$(srcdir)" -s -o version.sh --date-format '%e %B %Y'

DISTHOOK_VERSION = \
	$(SHELL) "$(top_srcdir)/scripts/git-version-gen.sh" -C "$(srcdir)" -s -o "$(distdir)/version.sh.in" --date-format '%e %B %Y'

endif

######################################## CONFIG_DOXYGEN
if CONFIG_DOXYGEN

RUN_DOXYGEN = source ./version.sh && PROJECT_NUMBER=$$repo_version_desc ${DOXYGEN}

# HTML

html: html/index.html

html/index.html: version.sh
	@echo "Running ${DOXYGEN} DoxyfileHTML ..."; \
	${RUN_DOXYGEN} DoxyfileHTML

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

latex/doxygen.tex: version.sh
	@echo "Running ${DOXYGEN} DoxyfileLATEX ..."; \
	${RUN_DOXYGEN} DoxyfileLATEX; \
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

pdflatex/doxygen.tex: version.sh
	@echo "Running ${DOXYGEN} DoxyfilePDFLATEX ..."; \
	${RUN_DOXYGEN} DoxyfilePDFLATEX; \
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
