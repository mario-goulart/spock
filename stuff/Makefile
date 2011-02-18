##### Makefile for SPOCK


# use v8, if you can (rhino will die, for example)
JS = js
SPOCK = chicken-spock
TIME = /usr/bin/time --quiet -f '%U+%S'
JSMIN = jsmin

.PHONY: check checkprograms checklibrary


check: checklibrary checkprograms

checklibrary: test.scm spock-runtime.js spock-runtime-debug.js
	@echo "======================================== library tests (debug)"
	@$(SPOCK) $(SPOCK_OPTIONS) -debug test.scm library-tests.scm test-end.scm \
	  -o tests/library-test-debug.js
	@$(TIME) $(JS) -f spock-runtime-debug.js -f tests/library-test-debug.js
	@echo "======================================== library tests (debug + strict)"
	@$(SPOCK) $(SPOCK_OPTIONS) -debug -strict test.scm library-tests.scm test-end.scm \
	  -o tests/library-test-strict.js
	@$(TIME) $(JS) -f spock-runtime-debug.js -f tests/library-test-strict.js
	@echo "======================================== library tests (default)"
	@$(SPOCK) $(SPOCK_OPTIONS) test.scm library-tests.scm test-end.scm \
	  -o tests/library-test.js
	@$(TIME) $(JS) -f spock-runtime.js -f tests/library-test.js

checkprograms: test.scm spock-runtime-debug.js
	@for x in `ls tests/*.scm`; do \
	  tests/runtest.sh $(SPOCK) $$x $(SPOCK_OPTIONS) -debug \
	    -o tests/`basename $$x .scm`.js || exit 1; \
	  $(TIME) $(JS) -f spock-runtime-debug.js -f tests/`basename $$x .scm`.js || \
	  exit 1; \
	done

minify-tests:
	rm -f tmp.js
	for x in `ls *.js`; do \
	  $(JSMIN) <$$x >tmp.js; \
	  mv tmp.js $$x; \
	done
	rm -f tmp.js
