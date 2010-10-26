EXE = rank0C rank1C rank0H rank1H
all: $(EXE)

rank0C: Rank0.c
	mpicc -O2 -Wall Rank0.c -o rank0C

rank1C: Rank1.c
	mpicc -O2 -Wall Rank1.c -o rank1C

rank0H: Rank0.hs
	ghc --make -O2 Rank0.hs -o rank0H

rank1H: Rank1.hs
	ghc --make -O2 Rank1.hs -o rank1H

clean:
	/bin/rm -f *.o *.hi

clobber: clean
	/bin/rm -f $(EXE)
