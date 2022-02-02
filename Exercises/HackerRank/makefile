
all : sier clean

sier :
	mkdir -p ~/.config/sier
	ghc -o ~/.config/sier/sierpinski sierpinski.hs
	cp sierpinski.sh ~/bin/sier
	cp ~/bin/center ~/.config/sier/center

recursion :
	ghci Recursion.hs <<<""

clean :
	find . -name '*.hi' -delete
	find . -name '*.o' -delete
