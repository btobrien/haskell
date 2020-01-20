
allclean : all clean

all : Z flipbit check

noise :
	mkdir -p ~/.config/noise/bin
	mkdir -p ~/.config/noise/tmp
	cp import.sh ~/.config/noise/import

check : noise
	cp check.sh ~/.config/noise/bin/check

Z : noise
	ghc -o ~/.config/noise/bin/Z Z.hs

flipbit : noise
	ghc -o ~/.config/noise/bin/flipbit flipbit.hs

clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete
