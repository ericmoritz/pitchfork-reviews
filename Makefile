all:
	cabal install


lint:
	hlint src/*.hs
