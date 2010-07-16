#include <mpi.h>

/* Taken from HMPI */
#define MPI_CONST(ty, name, defn) inline ty name () { return ((ty)defn); }

MPI_CONST (MPI_Datatype, mpi_int, MPI_INT)
MPI_CONST (MPI_Datatype, mpi_byte, MPI_BYTE)
MPI_CONST (MPI_Comm, mpi_comm_world, MPI_COMM_WORLD)
