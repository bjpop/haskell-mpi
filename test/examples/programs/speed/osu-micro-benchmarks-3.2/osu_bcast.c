#define BENCHMARK "OSU Broadcast Latency Test"
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
#include <sys/time.h>
#include <math.h>

#define MAX_MSG_SIZE (1<<14)
#define SKIP 500
#define ITERATIONS 1000
#define SKIP_LARGE 10
#define ITERATIONS_LARGE 100
#define SKIP_TEST 500
#define ITERATIONS_TEST 1000
#define SKIP_LARGE_TEST 10
#define ITERATIONS_LARGE_TEST 50
int large_message_size = 8192;

#define ROOT 0

static inline double ret_us(void);
void get_ack_time(int,int);
int get_far_proc(int,int,int);

char x[MAX_MSG_SIZE];
char y[4] = {0,0,0,0};
double ack_time = 0.0;
int numprocs;
double t[ITERATIONS_TEST+1];

int main(int argc, char *argv[])
{
    int i = 0, rank, size, mpi_errno = MPI_SUCCESS;
    int far_proc = 0, skip, iterations;
    double latency = 0, total = 0, tmp1 = 0, tmp2 = 0;
    MPI_Status status;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);

    if(numprocs < 2) {
        if(rank == ROOT) {
            fprintf(stderr, "This test requires at least two processes\n");
        }

        MPI_Finalize();

        return EXIT_FAILURE;
    }

    if(rank == ROOT) {
        fprintf(stdout, "# %s v%s\n", BENCHMARK, PACKAGE_VERSION);
        fprintf(stdout, "%-*s%*s\n", 10, "# Size", FIELD_WIDTH, "Latency (us)");
        fflush(stdout);
    }

    for(i = 0; i < MAX_MSG_SIZE; i++) {
        x[i] = 'a';
    }

    for(size=1; size <= MAX_MSG_SIZE; size *= 2) {
        MPI_Barrier(MPI_COMM_WORLD);

        far_proc = get_far_proc(numprocs, rank, size);
        get_ack_time(far_proc, rank);

        if(size > large_message_size) {
            skip = SKIP_LARGE;
            iterations = ITERATIONS_LARGE;
        }

        else {
            skip = SKIP;
            iterations = ITERATIONS;
        }

        MPI_Barrier(MPI_COMM_WORLD);

        for(i=0; i < iterations + skip ; i++) {
            if(i == skip && rank == ROOT) {
                tmp1 = ret_us();
            }

            MPI_Bcast(&x, size, MPI_CHAR, 0, MPI_COMM_WORLD);         

            if(rank == ROOT) {
                mpi_errno = MPI_Recv(&y, 0, MPI_CHAR, far_proc, 1,
                        MPI_COMM_WORLD, &status);

                if(mpi_errno != MPI_SUCCESS) {
                    fprintf(stderr, "Receive failed\n");
                }
            }

            if(rank == far_proc) {
                mpi_errno = MPI_Send(&y, 0, MPI_CHAR, ROOT, 1, MPI_COMM_WORLD);

                if(mpi_errno != MPI_SUCCESS) {
                    fprintf(stderr, "Send failed\n");
                }
            }
        }

        if(rank == ROOT) {
            tmp2 = ret_us();
            total = tmp2 - tmp1;
            latency = (double)total/iterations;

            fprintf(stdout, "%-*d%*.*f\n", 10, size, FIELD_WIDTH,
                    FLOAT_PRECISION, latency - ack_time);
            fflush(stdout);
        }

        MPI_Barrier(MPI_COMM_WORLD);
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}

void get_ack_time(int far_proc, int myid) {
    int i;
    double t_start = 0.0, t_end = 0.0;
    MPI_Status reqstat;

    if(myid == ROOT) {
        for(i = 0; i < ITERATIONS + SKIP; i++) {
            if(i == SKIP) {
                t_start = ret_us();
            }

            MPI_Send(x, 0, MPI_CHAR, far_proc, 1, MPI_COMM_WORLD);
            MPI_Recv(y, 0, MPI_CHAR, far_proc, 1, MPI_COMM_WORLD, &reqstat);
        }

        t_end = ret_us();
        ack_time = (t_end - t_start) / (2.0 * ITERATIONS);
    }

    else if(myid == far_proc) {
        for(i = 0; i < ITERATIONS + SKIP; i++) {
            MPI_Recv(y, 0, MPI_CHAR, 0, 1, MPI_COMM_WORLD, &reqstat);
            MPI_Send(x, 0, MPI_CHAR, 0, 1, MPI_COMM_WORLD);
        }
    }
}

static inline double ret_us(void)
{
    struct timeval t;

    gettimeofday(&t, NULL);

    return t.tv_sec * 1e6 + t.tv_usec;
}

int get_far_proc(int numprocs, int rank, int size)
{
    int i = 0, j = 0, iter = 0, mpi_errno = 0;
    int far_proc = 0, skip, iterations, k;
    double max_latency = 0, mean, std_dev, temp;
    MPI_Status status;

    if(size < large_message_size) {
        skip = SKIP_TEST;
        iterations = ITERATIONS_TEST;
    }

    else {
        skip = SKIP_LARGE_TEST;
        iterations = ITERATIONS_LARGE_TEST;
    }

    MPI_Barrier(MPI_COMM_WORLD);

    for(i = 1; i < numprocs; i++) {
        for(iter = 0; iter < skip + iterations; iter++) {

            if(iter >= skip && rank == ROOT) {
                t[iter-skip] = ret_us();
            }

            MPI_Bcast(&x, size, MPI_CHAR, 0, MPI_COMM_WORLD);         

            if(rank == ROOT) {
                mpi_errno = MPI_Recv(&y, 0, MPI_CHAR, i, 1, MPI_COMM_WORLD,
                        &status);

                if(mpi_errno != MPI_SUCCESS) {
                   fprintf(stderr, "Receive failed\n");
                }
            }

            if(rank == i) {
                mpi_errno = MPI_Send(&y, 0, MPI_CHAR, ROOT, 1, MPI_COMM_WORLD);

                if (mpi_errno != MPI_SUCCESS) {
                    fprintf(stderr, "Send failed\n");
                }
            }
        }

        if(rank == ROOT) {
            t[iter-skip] = ret_us();

            for(j = 1, mean = 0; j <= iterations; j++) {
                mean += t[j]-t[j-1];
            }

            mean /= iterations;

            for(j = 1, std_dev = 0; j <= iterations; j++) {
                std_dev += (t[j] - t[j-1] - mean) * (t[j] - t[j-1] - mean);
            }

            std_dev /= iterations;
            std_dev = sqrt(std_dev);

            for(j = 1, k = temp = 0; j <= iterations; j++) {
                if((t[j]-t[j-1]>mean-1.5*std_dev) && 
                   (t[j]-t[j-1]<mean+1.5*std_dev)) {
                    temp+=t[j]-t[j-1];
                    k++;
                }
            }

            if(k != 0) {
                mean = (double)temp / k;
            }

            if(mean > max_latency) {
                far_proc = i;
                max_latency = mean;
            }
        }
    }

    MPI_Bcast(&far_proc, 1, MPI_INT, 0, MPI_COMM_WORLD);      

    return far_proc;
}

/* vi: set sw=4 sts=4 tw=80: */
