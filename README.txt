Haskell-mpi, Haskell bindings to the MPI library
------------------------------------------------

How to build
------------

Use "cabal install --extra-include-dirs=/path/to/mpi/headers --extra-lib-dirs=/path/to/mpi/libs"
or something similar.  Make sure that you have libmpi.a and libmpi.so available.

When building against MPICH 1.4, pass extra flag "-fmpich14"

Testing
-------

Two types of tests are provided:

   1. Unit tests.
   2. Standalone tests.

The unit tests are designed to test the functions exported by the library on
an individual basis. The standalone tests are comprised of complete programs -
they act as simple integration tests, and may also include regression tests.

How to enable testing
---------------------

Add "-ftest" to cabal install:

   cabal -ftest install

How to run the unit tests
-------------------------

(Assuming you have built haskell-mpi  with -ftest, as described above):

Run the program "haskell-mpi-testsuite" using "mpirun" like so:

  mpirun -np 2 haskell-mpi-testsuite 1>sender.log 2>receiver.log

Process with rank 0 emits the output to stdout, and every other rank reports
to the stderr.

If you are using the PBS batch system to launch jobs, there is a sample
job script in test/pbs/ for submitting the test case to the jobs queue.

How to run standalone tests
---------------------------

Standalone test programs can be found in the test/examples directory.
You can test the execution of these programs using the shelltestrunner package:

   http://hackage.haskell.org/package/shelltestrunner

Make sure you install shelltestrunner first, for example:

   cabal install shelltestrunner

To run the tests, issue this command:

   shelltest --execdir test/examples/

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
and good quality serialization libraries had emerged. So while haskell-mpi
is highly inspired by hMPI (which was very good code),
it is almost entirely a rewrite.

Haskell-mpi got its first main injection of effort during the inaugural
AusHac Australian Haskell Hackathon, hosted at UNSW from the 16th to the
18th of July 2010. The end result was a proof of concept.

The next major injection of effort happened when Dmitry Astapov started
contributing to the project in August 2010.

Contributions have also been made by:

   - Abhishek Kulkarni: support for MPI-2 intercommunicator client/server
     functions 
   - Andres Löh: bug fixes
   - Ian Ross: updated the code to work with newer C2HS.
