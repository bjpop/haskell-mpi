#include <stdlib.h>
#include <stdio.h>
#include "mpi.h"

int main(int argc, char **argv)
{
  MPI_Comm client;
  MPI_Status status;
  char port_name[MPI_MAX_PORT_NAME];
  int size, again, i;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  if (size != 1) {
    fprintf(stderr, "Server too big");
    exit(EXIT_FAILURE);
  }

  MPI_Open_port(MPI_INFO_NULL, port_name);
  printf("Server available at port: %s\n", port_name);
  while (1) {
    MPI_Comm_accept(port_name, MPI_INFO_NULL, 0, MPI_COMM_WORLD, &client);
    again = 1;
    while (again) {
      MPI_Recv(&i, 1, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG, client, &status);
      switch (status.MPI_TAG) {
      case 0:
	MPI_Comm_free(&client);
	MPI_Close_port(port_name);
	MPI_Finalize();
	return 0;
      case 1:
	MPI_Comm_disconnect(&client);
	again = 0;
	break;
      case 2: /* do something */
	printf("Received: %d\n", i);
	break;
      default:
	/* Unexpected message type */
	MPI_Abort(MPI_COMM_WORLD, 1);
      }
    }
  }
}
