ghc := ghc
ghcflags := -Wall -optl -w
examples := Hello HiLo Infinity Func

all: $(examples)

%: %.hs
	$(ghc) $(ghcflags) --make -o $@ -main-is $(basename $<).main $<

%.run: %
	./$<

run:	$(examples:%=%.run)

clean:
	rm -f $(examples) *.o *.hi *.s *.bc Fib *.exe *.exe.manifest *.bc
