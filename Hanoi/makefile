
hanoi:
	mkdir -p ~/bin && mkdir -p ~/.config/hanoi
	cp ~/bin/coat ~/.config/hanoi/
	ghc -o ~/.config/hanoi/show show.hs
	ghc -o ~/.config/hanoi/move move.hs
	cp toi.sh ~/.config/hanoi/toi
	cp hanoi.sh ~/bin/hanoi

soi :
	ghc -o ~/bin/soi Solve.hs

centos : hanoi
	cp hanoi_centos.sh ~/bin/hanoi

clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete
