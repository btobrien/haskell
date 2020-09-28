
all : clone hanoi tile coat noise sage

clone :
	make clone -C Utils

tile : coat 
	make tile -C Sage

hanoi : coat
	make hanoi -C Games/Hanoi

day :
	make day -C Games/Day

coat :
	make coat -C Utils

payout :
	make payout -C Payout

noise :
	make -C Noise

sage :
	make -C Sage

clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete
