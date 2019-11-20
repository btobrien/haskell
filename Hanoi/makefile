
all : hanoi toi textnoi shownoi coat clean

hanoi:
	cp hanoi.sh ~/bin/hanoi

toi:
	cp toi.sh ~/bin/toi

textnoi:
	ghc -o ~/bin/textnoi textnoi.hs

shownoi:
	ghc -o ~/bin/shownoi shownoi.hs

coat:
	cp coat.sh ~/bin/coat

clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete
