all:
	cabal-dev install


lint:
	hlint src/*.hs
