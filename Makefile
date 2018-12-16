build:
	stack build

test:
	stack test

repl:
	stack build && stack exec pcf-exe

deps:
	stack install apply-refact hlint intero stylish-haskell

format:
	find . -name '*.hs' | xargs -t stack exec stylish-haskell -- -i

lint:
	stack exec hlint -- -i 'Parse error' -i 'Reduce duplication' src test

refactor:
	find . -name '*.hs' | xargs -t -L1 stack exec hlint -- --refactor --refactor-options -i
