#include <mpi.h>

/* Taken from HMPI */
#define MPI_CONST(ty, name, defn) inline ty name () { return ((ty)defn); }

MPI_CONST (MPI_Datatype, mpi_int, MPI_INT)
MPI_CONST (MPI_Datatype, mpi_byte, MPI_BYTE)
MPI_CONST (MPI_Comm, mpi_comm_world, MPI_COMM_WORLD)
MPI_CONST (int, mpi_any_tag, MPI_ANY_TAG)
MPI_CONST (int, mpi_any_source, MPI_ANY_SOURCE)
MPI_CONST (int, mpi_root, MPI_ROOT)
MPI_CONST (int, mpi_proc_null, MPI_PROC_NULL)
