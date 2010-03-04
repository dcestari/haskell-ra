all:
	ghc --make -hidir build -odir build PrecisionArbitraria

clean:
	rm -Rf build

