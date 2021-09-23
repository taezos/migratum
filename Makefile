build:
	cabal new-build

clean:
	rm -rf dist-newstyle result result-*

ghcid-lib:
	ghcid -c cabal repl

ghcid-test:
	ghcid -c="cabal repl test"
