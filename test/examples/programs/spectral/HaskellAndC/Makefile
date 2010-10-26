EXE = rank0C rank1C rank0H rank1H
all: $(EXE)

rank0C: rank0.c
	mpicc -O2 -Wall rank0.c -o rank0C

rank1C: rank1.c
	mpicc -O2 -Wall rank1.c -o rank1C

rank0H: rank0.hs
	ghc --make -O2 rank0.hs -o rank0H

rank1H: rank1.hs
	ghc --make -O2 rank1.hs -o rank1H

clean:
	/bin/rm -f *.o *.hi

clobber: clean
	/bin/rm -f $(EXE)
