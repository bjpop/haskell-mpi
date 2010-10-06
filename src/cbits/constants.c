#include <mpi.h>

/* Taken from HMPI */
#define MPI_CONST(ty, name, defn) inline ty name () { return ((ty)defn); }

/* Datatypes */
MPI_CONST (MPI_Datatype, mpi_int, MPI_INT)
MPI_CONST (MPI_Datatype, mpi_byte, MPI_BYTE)
MPI_CONST (MPI_Datatype, mpi_double, MPI_DOUBLE)
MPI_CONST (MPI_Datatype, mpi_float, MPI_FLOAT)

/* Misc */
MPI_CONST (MPI_Comm, mpi_comm_world, MPI_COMM_WORLD)
MPI_CONST (int, mpi_any_tag, MPI_ANY_TAG)
MPI_CONST (int, mpi_any_source, MPI_ANY_SOURCE)
MPI_CONST (int, mpi_root, MPI_ROOT)
MPI_CONST (int, mpi_proc_null, MPI_PROC_NULL)
MPI_CONST (MPI_Group, mpi_group_empty, MPI_GROUP_EMPTY)

/* Operations */
MPI_CONST (MPI_Op, mpi_max    , MPI_MAX    )
MPI_CONST (MPI_Op, mpi_min    , MPI_MIN    )
MPI_CONST (MPI_Op, mpi_sum    , MPI_SUM    )
MPI_CONST (MPI_Op, mpi_prod   , MPI_PROD   )
MPI_CONST (MPI_Op, mpi_land   , MPI_LAND   )
MPI_CONST (MPI_Op, mpi_band   , MPI_BAND   )
MPI_CONST (MPI_Op, mpi_lor    , MPI_LOR    )
MPI_CONST (MPI_Op, mpi_bor    , MPI_BOR    )
MPI_CONST (MPI_Op, mpi_lxor   , MPI_LXOR   )
MPI_CONST (MPI_Op, mpi_bxor   , MPI_BXOR   )
MPI_CONST (MPI_Op, mpi_maxloc , MPI_MAXLOC )
MPI_CONST (MPI_Op, mpi_minloc , MPI_MINLOC )
MPI_CONST (MPI_Op, mpi_replace, MPI_REPLACE)
