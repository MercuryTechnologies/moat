build: hpack
	cabal build

build: hpack
	cabal test

hpack:
	hpack .

repl: hpack
	cabal repl

repl-test: hpack
	cabal repl test

.PROXY: build hpack repl repl-test
