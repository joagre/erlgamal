LIBS=sstdlib elgamal
TESTS=elgamal/test

all:
	for lib in $(LIBS) ; do \
		(cd lib/$$lib && $(MAKE) all) || exit 1; \
	done
	for test in $(TESTS) ; do \
		(cd lib/$$test && $(MAKE) all) || exit 1; \
	done

clean:
	for test in $(TESTS) ; do \
		(cd lib/$$test && $(MAKE) clean) || exit 1; \
	done
	for lib in $(LIBS) ; do \
		(cd lib/$$lib && $(MAKE) clean) || exit 1; \
	done

mrproper: clean cleanfluff

cleanfluff:
	find . \( -name erl_crash.dump -or -name "*~" -or -name '#*' -or -name '.#*' \) -exec rm {} \;
