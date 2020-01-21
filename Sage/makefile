
allclean : all clean

all : sage crypt judson tile dihsubs code

sage :
	ghc Sage.hs

crypt :
	ghc Data/Algabraic/Crypt/RSA.hs

code :
	ghc -o ~/.config/noise/bin/code Data/Algabraic/Code/code.hs

judson : 
	ghc Judson/Ch3.hs

tile :
	mkdir -p ~/bin && mkdir -p ~/.config/tile
	cp ~/bin/coat ~/.config/tile/coat
	ghc -O2 -o ~/.config/tile/move Data/Group/Tile/move.hs
	ghc -O2 -o ~/.config/tile/show Data/Group/Tile/show.hs
	cp Data/Group/Tile/tile.sh ~/bin/tile

dihsubs :
	ghc -threaded -rtsopts -O2 -o ~/bin/dihsubs Data/Group/dihsubs.hs

clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete
