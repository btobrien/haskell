
all : tile dihsubs clean

tile :
	mkdir -p ~/bin && mkdir -p ~/.config/tile
	cp ~/bin/coat ~/.config/tile/coat
	ghc -O2 -o ~/.config/tile/move Data/Group/Tile/move.hs
	ghc -O2 -o ~/.config/tile/show Data/Group/Tile/show.hs
	cp Data/Group/Tile/tile.sh ~/bin/tile

dihsubs :
	ghc -threaded -rtsopts -O2 -o ~/bin/dihsubs main/dihsubs.hs

clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete
