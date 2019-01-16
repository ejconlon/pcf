.PHONY: build
build:
	stack build

.PHONY: test
test:
	stack test

.PHONY: repl
repl:
	stack build && stack exec pcf-exe

.PHONY: deps
deps:
	stack install apply-refact hlint intero stylish-haskell

.PHONY: deps
format:
	find . -name '*.hs' | xargs -t stack exec stylish-haskell -- -i

.PHONY: lint
lint:
	stack exec hlint -- -i 'Parse error' -i 'Reduce duplication' src test

.PHONY: refactor
refactor:
	find . -path ./.stack-work -prune -o -name '*.hs' | xargs -t -L1 stack exec hlint -- --refactor --refactor-options -i
