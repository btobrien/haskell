
all : hanoi playnoi textnoi shownoi coat clean

hanoi:
	cp hanoi.sh ~/bin/hanoi

playnoi:
	cp playnoi.sh ~/bin/playnoi

textnoi:
	ghc -o ~/bin/textnoi textnoi.hs

shownoi:
	ghc -o ~/bin/shownoi shownoi.hs

coat:
	cp coat.sh ~/bin/coat

clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete
