ghcid-lib:
	ghcid -c cabal repl

build:
	cabal new-build

clean:
	rm -rf dist-newstyle result result-*
