#include <mpi.h>

/* The order of these is significant */
typedef enum ComparisonResult {
  Ident = MPI_IDENT,
  Congruent = MPI_CONGRUENT,
  Similar = MPI_SIMILAR,
  Unequal = MPI_UNEQUAL
};
