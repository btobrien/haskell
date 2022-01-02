
all : chomp

config :
	mkdir -p ~/.config/chomp/bin
	cp import.sh ~/.config/chomp/import

chomp : config compass
	ghc -O2 -o ~/.config/chomp/bin/move move.hs
	ghc -o ~/.config/chomp/bin/show show.hs
	cp moveshow.sh ~/.config/chomp/bin/moveshow
	cp chomp.sh ~/bin/chomp
	cp compass.sh ~/.config/chomp/bin/compass

clean :
	find . -name '*.hi' -delete
	find . -name '*.o' -delete
