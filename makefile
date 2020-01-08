
all : clone hanoi tile coat

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

clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete
