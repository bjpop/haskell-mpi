#include <mpi.h>

typedef enum Compare {
  Identical = MPI_IDENT,
  Congruent = MPI_CONGRUENT,
  Similar = MPI_SIMILAR,
  Unequal = MPI_UNEQUAL
};
