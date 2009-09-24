check:
	@error_count=0; \
	tests=`find -name '*.rb'`; \
	for i in $$tests; do \
		if [ $$i != "./form.rb" ]; then \
			echo Starting $$i; \
			/usr/bin/env ruby $$i || let error_count=error_count+1; \
			echo; \
		fi; \
	done; \
	if [ $$error_count -gt 0 ]; then \
		echo "Some checks failed!"; \
		exit $$error_count; \
	fi

.PHONY: clean distclean

clean:
	rm -f *.str TEMP *.frm *.tmp

distclean: clean
	rm -f *~
