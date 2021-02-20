PWD=$(shell pwd)
LIBRARY=$(shell basename $(PWD))

all:
	build

build:
	cabal build

install:
	cabal install

docs:
	@RESULT=`cabal haddock $(LIBRARY) 2>/dev/null | tail -n 2`; \
	if [[ `echo $$RESULT | head -c 22` = "Documentation created:" ]]; then \    
		$(OPEN) `echo $$RESULT | tail -c +23`; \    
	fi    

watch:
	ghcid -p unlifted --color -c 'cabal repl unlifted --repl-options=-fno-code --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j'

repl:
	cabal repl unlifted --repl-options=-fobject-code --repl-options=-v1 --repl-options=-ferror-spans --repl-options=-j

lint:
	find . -name "*.hs" -print | xargs hlint

.PHONY: all build docs install watch repl lint

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	OPEN = open
else
	OPEN = echo
endif
