Bindings-MPI, Haskell bindings to the MPI library 
------------------------------------------------

How to Build
------------

Use "cabal install --extra-include-dirs=/usr/include/mpi" or something similar.
Make sure that you have libmpi.a and libmpi.so available.

How to Build Testsuite
----------------------

Add "-ftest" to configure flags. Binary "bindings-mpi-testsuite" will be built.
Run it with "mpirun" like this:

  run --output-filename out -np 2 bindings-mpi-testsuite --plain 

Output from each process rank will be found in "out.<rank>"

License and Copyright
---------------------

Bindings-MPI is distributed as open source software under the terms of the BSD 
License (see the file LICENSE in the top directory).

Author: Bernie Pope, copyright 2010.

Contact information
-------------------

Email Bernie Pope:

   florbitous <at> gmail <dot> com

