
all : chomp clean

config :
	mkdir -p ~/.config/chomp/bin
	cp import.sh ~/.config/chomp/import.sh

chomp : config compass
	ghc -O2 -o ~/.config/chomp/bin/move move.hs
	ghc -o ~/.config/chomp/bin/show show.hs
	cp chomp.sh ~/bin/chomp
	cp compass.sh ~/.config/chomp/bin/chomp
	cp compass.sh ~/bin/chomp

clean :
	find . -name '*.hi' -delete
	find . -name '*.o' -delete
