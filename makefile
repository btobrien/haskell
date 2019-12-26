
all : tile coat

clone :
	make clone -C Utils
coat :
	make coat -C Utils

tile : coat 
	make tile -C Sage

hanoi :
	make hanoi -C Hanoi
