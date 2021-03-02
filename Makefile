PWD=$(shell pwd)
LIBRARY=$(shell basename $(PWD))

all: build

build:
	@cabal build -v0

install:
	@cabal install

clean:
	@cabal clean

docs:
	@RESULT=`cabal haddock --haddock-hyperlink-source unboxed def core 2>/dev/null | tail -n 2`; \
	if [[ `echo $$RESULT | head -c 22` = "Documentation created:" ]]; then \
		$(OPEN) `echo $$RESULT | tail -c +23`; \
	fi    

watch:
	@ghcid -p unboxed --color -c 'cabal repl unboxed --repl-options=-fno-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-Wno-prepositive-qualified-module --repl-options=-ignore-dot-ghci'

watch-core:
	@ghcid -p core --color -c 'cabal repl core --repl-options=-fno-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-Wno-prepositive-qualified-module --repl-options=-ignore-dot-ghci'

repl:
	@cabal repl unboxed --repl-options=-fno-it --repl-options=-interactive-print=print --repl-options=-fobject-code --repl-options=-v1 --repl-options=-ferror-spans

lint:
	@find . -name "*.hs" -print | xargs hlint

.PHONY: all build clean docs install watch repl lint

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	OPEN = open
else
	OPEN = echo
endif
