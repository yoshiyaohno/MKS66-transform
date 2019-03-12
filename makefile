main: Main.hs Transform.hs Line.hs
	ghc -dynamic -O2 Main.hs

clean:
	rm *.hi *.o Main out.ppm

run: Main
	./Main
