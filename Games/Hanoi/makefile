
hanoi: soi hang
	mkdir -p ~/bin && mkdir -p ~/.config/hanoi/bin
	cp ~/bin/coat ~/.config/hanoi/bin/
	ghc -o ~/.config/hanoi/bin/show show.hs
	ghc -o ~/.config/hanoi/bin/move move.hs
	cp hanoi.sh ~/bin/hanoi
	cp toi.sh ~/.config/hanoi/bin/toi
	cp throttle.sh ~/.config/hanoi/bin/throttle
	cp ~/bin/soi ~/.config/hanoi/bin/
	cp ~/bin/hang ~/.config/hanoi/bin/

soi :
	ghc -o ~/bin/soi Solve.hs

hang :
	ghc -o ~/bin/hang Hang.hs

centos : hanoi
	cp hanoi_centos.sh ~/bin/hanoi

clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete
