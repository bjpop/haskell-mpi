#include <mpi.h>

/* Taken from HMPI */
#define MPI_CONST(ty, name, defn) inline ty name () { return ((ty)defn); }

/* Datatypes */
MPI_CONST (MPI_Datatype, mpi_char, MPI_CHAR)
MPI_CONST (MPI_Datatype, mpi_short, MPI_SHORT)
MPI_CONST (MPI_Datatype, mpi_int, MPI_INT)
MPI_CONST (MPI_Datatype, mpi_long, MPI_LONG)
MPI_CONST (MPI_Datatype, mpi_long_long, MPI_LONG_LONG)
MPI_CONST (MPI_Datatype, mpi_unsigned_char, MPI_UNSIGNED_CHAR)
MPI_CONST (MPI_Datatype, mpi_unsigned_short, MPI_UNSIGNED_SHORT)
MPI_CONST (MPI_Datatype, mpi_unsigned, MPI_UNSIGNED)
MPI_CONST (MPI_Datatype, mpi_unsigned_long, MPI_UNSIGNED_LONG)
MPI_CONST (MPI_Datatype, mpi_unsigned_long_long, MPI_UNSIGNED_LONG_LONG)
MPI_CONST (MPI_Datatype, mpi_float, MPI_FLOAT)
MPI_CONST (MPI_Datatype, mpi_double, MPI_DOUBLE)
MPI_CONST (MPI_Datatype, mpi_long_double, MPI_LONG_DOUBLE)
MPI_CONST (MPI_Datatype, mpi_byte, MPI_BYTE)
MPI_CONST (MPI_Datatype, mpi_packed, MPI_PACKED)

/* Misc */
MPI_CONST (int, mpi_any_source, MPI_ANY_SOURCE)
MPI_CONST (int, mpi_proc_null, MPI_PROC_NULL)
MPI_CONST (int, mpi_root, MPI_ROOT)
MPI_CONST (int, mpi_any_tag, MPI_ANY_TAG)
MPI_CONST (int, mpi_max_processor_name, MPI_MAX_PROCESSOR_NAME)
MPI_CONST (int, mpi_max_error_string, MPI_MAX_ERROR_STRING)
MPI_CONST (int, mpi_max_object_name, MPI_MAX_OBJECT_NAME)
MPI_CONST (int, mpi_undefined, MPI_UNDEFINED)
MPI_CONST (int, mpi_cart, MPI_CART)
MPI_CONST (int, mpi_graph, MPI_GRAPH)

/* MPI predefined handles */
MPI_CONST (MPI_Comm, mpi_comm_world, MPI_COMM_WORLD)
MPI_CONST (MPI_Comm, mpi_comm_self, MPI_COMM_SELF)
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
