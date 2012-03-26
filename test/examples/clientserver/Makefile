EXE = ServerC ClientC ServerH ClientH
all: $(EXE)

# Set C compiler to use -m32 if ghc is set to produce 32 bit executables
# as is usually (always?) the case on OS X and ghc 6.12

ServerC: Server.c
	mpicc -O2 -Wall Server.c -o ServerC
#	mpicc -m32 -O2 -Wall Server.c -o ServerC

ClientC: Client.c
	mpicc -O2 -Wall Client.c -o ClientC
#	mpicc -m32 -O2 -Wall Server.c -o ServerC

ServerH: Server.hs
	ghc --make -O2 Server.hs -o ServerH

ClientH: Client.hs
	ghc --make -O2 Client.hs -o ClientH

clean:
	/bin/rm -f *.o *.hi

clobber: clean
	/bin/rm -f $(EXE)
