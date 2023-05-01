build: hpack
	cabal build

test: hpack
	cabal test

hpack:
	hpack .

repl: hpack
	cabal repl

repl-test: hpack
	cabal repl test:spec

ghcid: hpack
	ghcid -c cabal repl

ghcid-test: hpack
	ghcid -c cabal repl test:spec

format:
	alejandra --quiet .
	fourmolu -i src/ test/

check-format:
	fourmolu -m check src/ test/ && alejandra --check .

hlint:
	hlint .

clean:
	cabal clean

haddock:
	cabal haddock

.PHONY: build test hpack repl repl-test ghcid ghcid-test format check-format hlint clean haddock
