TEST_BINS =
if BUILD_FORM
TEST_BINS += $(top_builddir)/sources/form
endif
if BUILD_TFORM
TEST_BINS += $(top_builddir)/sources/tform
endif
if BUILD_PARFORM
TEST_BINS += $(top_builddir)/sources/parform
endif

TEST_OPTS =

TESTS =
if CONFIG_RUBY
TESTS_ENVIRONMENT = \
	RUBY="$(RUBY)" \
	TEST_BINS="$(TEST_BINS)" \
	TEST_OPTS="$(TEST_OPTS)" \
	$(SHELL)
TESTS += check-help.sh
else
TESTS_ENVIRONMENT = \
	TEST_BINS="$(TEST_BINS)" \
	$(SHELL)
endif
TESTS += benchmark-fu.sh

EXTRA_DIST = \
	check-help.sh \
	benchmark-fu.sh \
	check.rb \
	examples.frm \
	features.frm \
	fixes.frm \
	forcer/forcer.frm \
	formunit/fu.frm
