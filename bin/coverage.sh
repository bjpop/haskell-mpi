#!/bin/bash
#
# Convenience script to help produce the code coverage report for
# testsuite. Should be run from the same dir where bindings-mpi.cabal
# is located and after "cabal install" or "cabal build" has been run

rm -f *.tix
mpirun -np 5 bindings-mpi-testsuite 2>receivers.log | tee sender.log
hpc combine --output=bindings-mpi-testsuite.tix rank0.tix rank1.tix
hpc markup --destdir=./html bindings-mpi-testsuite.tix
hpc report bindings-mpi-testsuite.tix
