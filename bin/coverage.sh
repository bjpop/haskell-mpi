#!/bin/bash
#
# Convenience script to help produce the code coverage report for
# testsuite. Should be run from the same dir where haskell-mpi.cabal
# is located and after "cabal install" or "cabal build" has been run

rm -f *.tix
mpirun -np 5 haskell-mpi-testsuite 2>receivers.log | tee sender.log
hpc combine --output=rank01.tix rank0.tix rank1.tix
hpc combine --output=rank23.tix rank2.tix rank3.tix
hpc combine --output=rank0123.tix rank01.tix rank23.tix
hpc combine --output=haskell-mpi-testsuite.tix rank0123.tix rank4.tix
hpc markup --destdir=./html haskell-mpi-testsuite.tix
hpc report haskell-mpi-testsuite.tix
