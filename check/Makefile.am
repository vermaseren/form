TEST_BINS =
if BUILD_FORM
TEST_BINS += form
endif
if BUILD_TFORM
TEST_BINS += tform
endif
if BUILD_PARFORM
TEST_BINS += parform
endif
if CONFIG_RUBY
TESTS_ENVIRONMENT = \
	PATH="$(top_builddir)/sources:$$PATH" \
	RUBY="$(RUBY)" \
	CHECK_RB="$(srcdir)/check.rb" \
	TEST_BINS="$(TEST_BINS)" \
	$(SHELL)
TESTS = check-help.sh
endif
EXTRA_DIST = check-help.sh check.rb examples.frm fixes.frm
