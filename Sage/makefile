
all : dihsubs movetile showtile tile

movetile:
	ghc -O2 -o ~/bin/movetile Data/Group/Tile/movetile.hs

showtile:
	ghc -O2 -o ~/bin/showtile Data/Group/Tile/showtile.hs

tile : movetile showtile
	cp Data/Group/Tile/tile.sh ~/bin/tile

dihsubs:
	ghc -threaded -rtsopts -O2 -o ~/bin/dihsubs main/dihsubs.hs

clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete
