
all : sierpinski clean

sierpinski :
	mkdir -p ~/.config/sierpinski
	ghc -o ~/.config/sierpinski/sierpinski sierpinski.hs
	cp sierpinski.sh ~/bin/sierpinski
	which center

recursion :
	ghci Recursion.hs <<<""

clean :
	find . -name '*.hi' -delete
	find . -name '*.o' -delete
