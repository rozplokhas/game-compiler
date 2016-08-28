all:
	ghc -o translator main.hs
clean:
	rm -rf *.o *.hi translator
