TEST_BINS =
if BUILD_FORM
TEST_BINS += $(top_builddir)/sources/form$(EXEEXT)
endif
if BUILD_TFORM
TEST_BINS += $(top_builddir)/sources/tform$(EXEEXT)
endif
if BUILD_PARFORM
TEST_BINS += $(top_builddir)/sources/parform$(EXEEXT)
endif

TEST_OPTS =

TESTS =
if CONFIG_RUBY
TESTS_ENVIRONMENT = \
	RUBY="$(RUBY)" \
	TEST_BINS="$(TEST_BINS)" \
	TEST_OPTS="$(TEST_OPTS)" \
	"$(SHELL)"
TESTS += check-help.sh
else
TESTS_ENVIRONMENT = \
	TEST_BINS="$(TEST_BINS)" \
	"$(SHELL)"
endif
TESTS += benchmark-fu.sh

EXTRA_DIST = \
	benchmark-fu.sh \
	check-help.sh \
	check.rb \
	checkpoint/checkpoint.frm \
	examples.frm \
	features.frm \
	fixes.frm \
	forcer/forcer.frm \
	formunit/fu.frm
