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
	find src/ test/ -name "*.hs" -exec fourmolu -i {} --ghc-opt -XTypeApplications \;

hlint:
	hlint .

clean:
	cabal clean

haddock:
	cabal haddock

.PHONY: build test hpack repl repl-test ghcid ghcid-test format hlint clean haddock
