
payout : plot
	mkdir -p ~/bin && mkdir -p ~/.config/payout/bin
	ghc -o ~/.config/payout/bin/pay payout.hs
	ghc -o ~/.config/payout/bin/distribute_missing distributeMissing.hs
	cp payout.sh ~/bin/payout

plot :
	cp plot.sh ~/bin/plot

clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete

