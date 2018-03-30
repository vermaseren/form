TEXSRC = \
	cvs.tex \
	devref.tex \
	formrun.tex \
	indepth.tex \
	source.tex \
	testsuite.tex

MAIN = devref

TEXFILES = $(TEXSRC) $(MAIN).tex version.tex

EXTRA_DIST = $(TEXSRC) .latex2html-init

.PHONY: dvi latex2html html ps pdf clean-local update_version_tex

# NOTE: htlatex invalidate .aux, .idx, .dvi files.
HTMLCLEANFILES = idxmake.dvi idxmake.log $(MAIN).4ct $(MAIN).4dx $(MAIN).4ix \
	$(MAIN).4tc $(MAIN).aux $(MAIN).css $(MAIN).dvi $(MAIN).html $(MAIN)2.html \
	$(MAIN).idv $(MAIN).idx $(MAIN).ilg $(MAIN).ind $(MAIN).lg $(MAIN).log \
	$(MAIN).tmp $(MAIN).xref

CLEANFILES = $(MAIN).pdf $(MAIN).ps $(MAIN).toc $(DATEFILE) texput.log \
	version.tex $(HTMLCLEANFILES)

clean-local:
	rm -rf html $(MAIN)

# Automatic versioning.

version.tex: update_version_tex
	$(UPDATE_VERSION_TEX)

dist-hook:
	$(DISTHOOK_VERSION_TEX)

if FIXED_VERSION

UPDATE_VERSION_TEX = \
	[ -f version.tex ] || $(LN_S) "$(srcdir)/version.tex.in" version.tex

DISTHOOK_VERSION_TEX = \
	cp "$(srcdir)/version.tex.in" "$(distdir)/version.tex.in"

else

UPDATE_VERSION_TEX = \
	$(SHELL) "$(top_srcdir)/scripts/git-version-gen.sh" -C "$(srcdir)" -t -o version.tex --date-format '%e %B %Y'

DISTHOOK_VERSION_TEX = \
	$(SHELL) "$(top_srcdir)/scripts/git-version-gen.sh" -C "$(srcdir)" -t -o "$(distdir)/version.tex.in" --date-format '%e %B %Y'

endif

#################### CONFIG_TEX
if CONFIG_TEX

dvi: $(MAIN).dvi

if CONFIG_MAKEINDEX
$(MAIN).dvi: $(TEXFILES)
	$(LATEX) $(MAIN).tex; while [ `grep -v 'Package: rerunfilecheck' $(MAIN).log | grep -c Rerun` -gt 0 ]; do $(LATEX) $(MAIN).tex; done
	$(MAKEINDEX) $(MAIN)
	$(LATEX) $(MAIN).tex; while [ `grep -v 'Package: rerunfilecheck' $(MAIN).log | grep -c Rerun` -gt 0 ]; do $(LATEX) $(MAIN).tex; done
else
$(MAIN).dvi: $(TEXFILES)
	$(LATEX) $(MAIN).tex; while [ `grep -v 'Package: rerunfilecheck' $(MAIN).log | grep -c Rerun` -gt 0 ]; do $(LATEX) $(MAIN).tex; done
endif

########## CONFIG_LATEX2HTML
if CONFIG_LATEX2HTML

latex2html: $(MAIN)/$(MAIN).html

$(MAIN)/$(MAIN).html: $(MAIN).dvi
	$(LATEX2HTML) -init_file $(srcdir)/.latex2html-init $(MAIN).tex
	cat $(MAIN)/index.html | sed 's/$(MAIN).html#/#/g' >$(MAIN)/index.html.tmp
	mv $(MAIN)/index.html.tmp $(MAIN)/index.html
	cat $(MAIN)/$(MAIN).html | sed 's/$(MAIN).html#/#/g' >$(MAIN)/$(MAIN).html.tmp
	mv $(MAIN)/$(MAIN).html.tmp $(MAIN)/$(MAIN).html

endif
########## CONFIG_LATEX2HTML

########## CONFIG_HTLATEX
if CONFIG_HTLATEX

html: html/$(MAIN).html

if CONFIG_MAKEINDEX
html/$(MAIN).html: $(TEXFILES)
	mkdir -p html
	$(HTLATEX) $(MAIN) "html,mathml-" "" "-dhtml/"
	$(TEX) '\def\filename{{$(MAIN)}{idx}{4dx}{ind}} \input  idxmake.4ht'
	$(MAKEINDEX) -o $(MAIN).ind $(MAIN).4dx
	$(HTLATEX) $(MAIN) "html,mathml-" "" "-dhtml/"
	sed 's/table.tabular {margin-left: auto; margin-right: auto;}/table.tabular {margin-left: inherit;}/' html/$(MAIN).css >html/$(MAIN).css.tmp
	mv html/$(MAIN).css.tmp html/$(MAIN).css
	rm -f $(HTMLCLEANFILES)
else
html/$(MAIN).html: $(DATEFILE)
	mkdir -p html
	$(HTLATEX) $(MAIN) "html,mathml-" "" "-dhtml/"
	rm -f $(HTMLCLEANFILES)
endif

endif
########## CONFIG_HTLATEX

########## CONFIG_PS
if CONFIG_PS

ps: $(DATEFILE) $(MAIN).ps

$(MAIN).ps: $(DATEFILE) $(MAIN).dvi
	$(DVIPS) -o $(MAIN).ps $(MAIN).dvi

endif
########## CONFIG_PS

########## CONFIG_PDF
if CONFIG_PDF

pdf: $(MAIN).pdf

if CONFIG_MAKEINDEX
$(MAIN).pdf: $(TEXFILES)
	$(PDFLATEX) $(MAIN).tex; while [ `grep -v 'Package: rerunfilecheck' $(MAIN).log | grep -c Rerun` -gt 0 ]; do $(PDFLATEX) $(MAIN).tex; done
	$(MAKEINDEX) $(MAIN)
	$(PDFLATEX) $(MAIN).tex; while [ `grep -v 'Package: rerunfilecheck' $(MAIN).log | grep -c Rerun` -gt 0 ]; do $(PDFLATEX) $(MAIN).tex; done
else
$(MAIN).pdf: $(TEXFILES)
	$(PDFLATEX) $(MAIN).tex; while [ `grep -v 'Package: rerunfilecheck' $(MAIN).log | grep -c Rerun` -gt 0 ]; do $(PDFLATEX) $(MAIN).tex; done
endif

endif
########## CONFIG_PDF

endif
#################### CONFIG_TEX
