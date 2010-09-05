#include <mpi.h>

typedef enum ThreadSupport {
  Single = MPI_THREAD_SINGLE,
  Funneled = MPI_THREAD_FUNNELED,
  Serialized = MPI_THREAD_SERIALIZED,
  Multiple = MPI_THREAD_MULTIPLE
};
