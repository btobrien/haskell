
all : go

config :
	mkdir -p ~/.config/go/bin
	cp import.sh ~/.config/go/import

go : config compass
	ghc -o ~/.config/go/bin/move move.hs
	ghc -o ~/.config/go/bin/show show.hs
	cp moveshow.sh ~/.config/go/bin/moveshow
	cp go.sh ~/bin/go
	cp compass.sh ~/.config/go/bin/compass

compass :
	cp compass.sh ~/bin/compass

clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete
