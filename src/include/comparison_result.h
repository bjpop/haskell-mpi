#include <mpi.h>

/* The order of these is significant, at least for OpenMPI */
typedef enum ComparisonResult {
  Identical = MPI_IDENT,
  Congruent = MPI_CONGRUENT,
  Similar   = MPI_SIMILAR,
  Unequal   = MPI_UNEQUAL
};
