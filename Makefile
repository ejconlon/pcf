deps:
	stack install apply-refact hlint intero stylish-haskell

format:
	find . -name '*.hs' | xargs -t stack exec stylish-haskell -- -i

lint:
	stack exec hlint -- src test

refactor:
	find . -name '*.hs' | xargs -t -L1 stack exec hlint -- --refactor --refactor-options -i
