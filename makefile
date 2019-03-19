main: Parse.hs Transform.hs Line.hs
	ghc -dynamic -O2 Parse.hs

clean:
	rm *.hi *.o Parse out.ppm

run:
	./Parse "script"
