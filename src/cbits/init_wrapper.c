#include <mpi.h>
#include "init_wrapper.h"

/* the following is taken from includes/Stg.h of the GHC distribution */

extern char **prog_argv;
extern int    prog_argc;

int init_wrapper (void) { return MPI_Init (&prog_argc, &prog_argv); }

int init_wrapper_thread (int required, int* provided) { return MPI_Init_thread (&prog_argc, &prog_argv, required, provided); }
