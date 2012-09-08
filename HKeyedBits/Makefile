example: example.hs
	ghc --make example.hs -o example -XDeriveDataTypeable
	rm *.o
	rm *.hi
	rm KeyedBits/*.hi KeyedBits/*.o

clean:
	if [ -f example ]; then rm example; fi
