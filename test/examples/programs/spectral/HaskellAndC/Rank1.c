#include <mpi.h>
#include <stdio.h>

#define MAX_MSG 100

int main(int argc, char **argv)
{
   int rank, size, i;
   int msg[MAX_MSG];
   MPI_Status status;

   MPI_Init(NULL, NULL);
   MPI_Comm_rank(MPI_COMM_WORLD, &rank);
   MPI_Comm_size(MPI_COMM_WORLD, &size);

   printf("C process with rank %d world with size %d\n", rank, size);

   if (rank == 1)
   {
      MPI_Recv(msg, MAX_MSG, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);
      for (i = 0; i < MAX_MSG; i++)
      {
         msg[i] *= msg[i];
      }
      MPI_Send(msg, MAX_MSG, MPI_INT, 0, 0, MPI_COMM_WORLD);
   }
   else
   {
      printf ("This program must be rank 1\n");
   }
   MPI_Finalize();

   return 0;
}
