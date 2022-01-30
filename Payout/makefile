
payout : plot
	mkdir -p ~/bin && mkdir -p ~/.config/payout/bin
	ghc -o ~/.config/payout/bin/pay payout.hs
	cp payout.sh ~/bin/payout

plot :
	cp plot.sh ~/bin/plot

clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete

