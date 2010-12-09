#define BENCHMARK "OSU MPI All-to-All Personalized Exchange Latency Test"
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

#define MAX_MSG_SIZE (1 << 20)
#define SKIP 300
#define ITERATIONS 1000
#define SKIP_LARGE 10
#define ITERATIONS_LARGE 100
#define MAX_ALIGNMENT 16384

double ret_us(void);
int numprocs, large_message_size = 8192;

int main(int argc, char *argv[])
{
    int i = 0, j = 0, rank = 0, size, mpi_errno = MPI_SUCCESS;
    int  sendcnt, recvcnt, skip, iterations, align_size;
    double tmp1 = 0, tmp2 = 0, latency = 0, total = 0;
    char *sendbuf, *recvbuf, *s_buf1, *r_buf1;
    MPI_Status status;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);

    s_buf1 = r_buf1 = NULL;

    s_buf1 = (char *)malloc(sizeof(char) * MAX_MSG_SIZE * numprocs +
            MAX_ALIGNMENT);

    if(NULL == s_buf1) {
        fprintf(stderr, "malloc failed.\n");

        exit(1);
    }

    r_buf1 = (char *) malloc (sizeof(char) * MAX_MSG_SIZE * numprocs +
            MAX_ALIGNMENT);

    if(NULL == r_buf1) {
        fprintf(stderr, "malloc failed.\n");

        exit(1);
    }

    align_size = getpagesize();
    sendbuf = (char *)(((unsigned long) s_buf1 + (align_size - 1)) / align_size
            * align_size);
    recvbuf = (char *)(((unsigned long) r_buf1 + (align_size - 1)) / align_size
            * align_size);

    if(0 == rank) {
        fprintf(stdout, "# %s v%s\n", BENCHMARK, PACKAGE_VERSION);
        fprintf(stdout, "%-*s%*s\n", 10, "# Size", FIELD_WIDTH, "Latency (us)");
        fflush(stdout);
    }


    MPI_Barrier(MPI_COMM_WORLD);

    for(size = 1; size <= MAX_MSG_SIZE; size *= 2) {
        if(size > large_message_size) {
            skip = SKIP_LARGE;
            iterations = ITERATIONS_LARGE;
        }

        else {
            skip = SKIP;
            iterations = ITERATIONS;
        }

        for(i = 0; i < iterations + skip; i++) {
            if(i == skip) {
                tmp1 = ret_us();
            }

            MPI_Alltoall(sendbuf, size, MPI_CHAR, recvbuf, size, MPI_CHAR,
                    MPI_COMM_WORLD);
        }

        if(0 == rank) {
            tmp2 = ret_us();
            fprintf(stdout, "%-*d%*.*f\n", 10, size, FIELD_WIDTH,
                    FLOAT_PRECISION, (tmp2 - tmp1) / iterations);
            fflush(stdout);
        }
    }

    free(s_buf1);
    free(r_buf1);

    MPI_Finalize();

    return EXIT_SUCCESS;
}

double ret_us(void)
{
    struct timeval t;

    gettimeofday(&t, NULL);

    return t.tv_sec * 1e6 + t.tv_usec;
}

/* vi: set sw=4 sts=4 tw=80: */
