#define BENCHMARK "OSU MPI Multi Latency Test"
/*
 * Copyright (C) 2002-2010 the Network-Based Computing Laboratory
 * (NBCL), The Ohio State University.
 *
 * Contact: Dr. D. K. Panda (panda@cse.ohio-state.edu)
 */

/*
This program is available under BSD licensing.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

(1) Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

(2) Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

(3) Neither the name of The Ohio State University nor the names of
their contributors may be used to endorse or promote products derived
from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/

#include "osu.h"
#include <string.h>

#define MAX_ALIGNMENT (16384)
#define MAX_MSG_SIZE (1<<22)
#define MAX_STEPS    (22+1)
#define MAXBUFSIZE (MAX_MSG_SIZE + MAX_ALIGNMENT)
#define LARGE_MSG_SIZE (8192)

char s_buf1[MAXBUFSIZE];
char r_buf1[MAXBUFSIZE];
char *s_buf, *r_buf;

int loop_small = 10000;
int skip_small = 100;
int loop_large = 1000;
int skip_large = 10;

static void multi_latency(int rank, int pairs);

int main(int argc, char* argv[])
{
    int align_size, rank, nprocs; 
    int pairs;

    MPI_Init(&argc, &argv);

    align_size = getpagesize();
    s_buf =
        (char *) (((unsigned long) s_buf1 + (align_size - 1)) /
                  align_size * align_size);
    r_buf =
        (char *) (((unsigned long) r_buf1 + (align_size - 1)) /
                  align_size * align_size);

    memset(s_buf, 0, MAX_MSG_SIZE);
    memset(r_buf, 0, MAX_MSG_SIZE);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);

    pairs = nprocs/2;

    if(rank == 0) {
        fprintf(stdout, "# %s v%s\n", BENCHMARK, PACKAGE_VERSION);
        fprintf(stdout, "%-*s%*s\n", 10, "# Size", FIELD_WIDTH, "Latency (us)");
        fflush(stdout);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    multi_latency(rank, pairs);
    
    MPI_Barrier(MPI_COMM_WORLD);

    MPI_Finalize();

    return EXIT_SUCCESS;
}

static void multi_latency(int rank, int pairs)
{
    int size, partner;
    int loop, i, skip;
    double t_start = 0.0, t_end = 0.0,
           latency = 0.0, total_lat = 0.0,
           avg_lat = 0.0;

    MPI_Status reqstat;


    for(size = 1; size <= MAX_MSG_SIZE; size *=2) {

        MPI_Barrier(MPI_COMM_WORLD);

        if(size > LARGE_MSG_SIZE) {
            loop = loop_large;
            skip = skip_large;
        } else {
            loop = loop_small;
            skip = skip_small;
        }

        if (rank < pairs) {
            partner = rank + pairs;

            for (i = 0; i < loop + skip; i++) {

                if (i == skip) {
                    t_start = MPI_Wtime();
                    MPI_Barrier(MPI_COMM_WORLD);
                }

                MPI_Send(s_buf, size, MPI_CHAR, partner, 1, MPI_COMM_WORLD);
                MPI_Recv(r_buf, size, MPI_CHAR, partner, 1, MPI_COMM_WORLD,
                         &reqstat);
            }

            t_end = MPI_Wtime();

        } else {
            partner = rank - pairs;

            for (i = 0; i < loop + skip; i++) {

                if (i == skip) {
                    t_start = MPI_Wtime();
                    MPI_Barrier(MPI_COMM_WORLD);
                }

                MPI_Recv(r_buf, size, MPI_CHAR, partner, 1, MPI_COMM_WORLD,
                         &reqstat);
                MPI_Send(s_buf, size, MPI_CHAR, partner, 1, MPI_COMM_WORLD);
            }

            t_end = MPI_Wtime();
        }

        latency = (t_end - t_start) * 1.0e6 / (2.0 * loop);

        MPI_Reduce(&latency, &total_lat, 1, MPI_DOUBLE, MPI_SUM, 0, 
                   MPI_COMM_WORLD);

        avg_lat = total_lat/(double) (pairs * 2);

        if(0 == rank) {
            fprintf(stdout, "%-*d%*.*f\n", 10, size, FIELD_WIDTH,
                    FLOAT_PRECISION, avg_lat);
            fflush(stdout);
        }
    }
}

/* vi: set sw=4 sts=4 tw=80: */
