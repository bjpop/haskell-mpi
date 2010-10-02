#include <mpi.h>

typedef enum CommCompare {
  Ident = MPI_IDENT,
  Congruent = MPI_CONGRUENT,
  Similar = MPI_SIMILAR,
  Unequal = MPI_UNEQUAL
};
