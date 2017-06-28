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
	CHECK_RB="$(srcdir)/check.rb" \
	TEST_BINS="$(TEST_BINS)" \
	TEST_OPTS="$(TEST_OPTS)" \
	$(SHELL)
TESTS += check-help.sh
endif

EXTRA_DIST = check-help.sh check.rb examples.frm features.frm fixes.frm
