Bindings-MPI, Haskell bindings to the MPI library
------------------------------------------------

How to Build
------------

Use "cabal install --extra-include-dirs=/usr/include/mpi" or something similar.
Make sure that you have libmpi.a and libmpi.so available.

How to Build Testsuite
----------------------

Add "-ftest" to configure flags, like so:

   cabal -ftest configure

and executable "bindings-mpi-testsuite" will be built.

Run it with "mpirun" like this:

  mpirun -np 2 bindings-mpi-testsuite 1>sender.log 2>receiver.log

Process with rank 0 emits the output to stdout, and every other rank reports
to the stderr.

License and Copyright
---------------------

Bindings-MPI is distributed as open source software under the terms of the BSD 
License (see the file LICENSE in the top directory).

Author(s): Bernie Pope, Dmitry Astapov. Copyright 2010.

Contact information
-------------------

Email Bernie Pope:

   florbitous <at> gmail <dot> com

History
-------

Around the year 2000 Michael Weber released hMPI, a Haskell binding to MPI:

   http://www.foldr.org/~michaelw/hmpi/

Development on that code appears to have stopped in about the year 2001.
Hal Daumé III picked up the code and got it working with (at the time)
a more recent version of GHC:

   http://www.umiacs.umd.edu/~hal/software.html

In February 2010 both Michael and Hal reported that they had not worked on
the code for a long time, so it was open for new maintainers.

In early 2010 Bernie Pope downloaded the above mentioned versions of
hMPI and tried to get them working with a modern GHC.

A few things had changed in Haskell since hMPI was written, which suggested
that it might be worth starting the binding from scratch. In particular
the FFI had changed in a few ways, the C2HS tool had matured substantially,
and good quality serialization libraries had emerged. So while bindings-mpi
is highly inspired by hMPI (which was very good code),
it is almost entirely a rewrite.

bindings-mpi got its first main injection of effort during the inaugural
AusHac Australian Haskell Hackathon, hosted at UNSW from the 16th to the
18th of July 2010. The end result was a proof of concept.

The next major injection of effort happened when Dmitry Astapov started
contributing to the project in August 2010.
