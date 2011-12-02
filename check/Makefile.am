if CONFIG_RUBY
if CONFIG_STRACE
TESTS_ENVIRONMENT = bindir="$(top_builddir)/sources"; export bindir; $(RUBY) -I $(srcdir)
TESTS = form.rb
endif
endif
EXTRA_DIST = form.rb examples.rb parser.rb symmetrize.rb table.rb
