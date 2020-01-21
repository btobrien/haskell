
all : clone hanoi tile coat noise

clone :
	make clone -C Utils

tile : coat 
	make tile -C Sage

hanoi : coat
	make hanoi -C Hanoi

coat :
	make coat -C Utils

payout :
	make payout -C Payout

noise :
	make all -C Noise

clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete
