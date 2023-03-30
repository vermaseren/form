SUBDIRS = doc sources check

EXTRA_DIST = README.md scripts/.latex2html-init scripts/run-latex2html.sh

dist-hook:
	$(DISTHOOK_VERSION)

if FIXED_VERSION

DISTHOOK_VERSION = \
	cp "$(srcdir)/.version" "$(distdir)/.version"

else

DISTHOOK_VERSION = \
	$(SHELL) "$(top_srcdir)/scripts/git-version-gen.sh" -C "$(srcdir)" -v -o "$(distdir)/.version"

endif
