
all : tile coat

clone :
	make clone -C Utils

tile : coat 
	make tile -C Sage

hanoi : coat
	make hanoi -C Hanoi

coat :
	make coat -C Utils

