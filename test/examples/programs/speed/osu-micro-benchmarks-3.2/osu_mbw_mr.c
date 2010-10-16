#define BENCHMARK "OSU MPI Multiple Bandwidth / Message Rate Test"
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
#include <assert.h>

#define DEFAULT_WINDOW       (64)

#define ITERS_SMALL          (100)          
#define WARMUP_ITERS_SMALL   (10)
#define ITERS_LARGE          (20)
#define WARMUP_ITERS_LARGE   (2)
#define LARGE_THRESHOLD      (8192)

#define WINDOW_SIZES {8, 16, 32, 64, 128}
#define WINDOW_SIZES_COUNT   (5)

#define MAX_MSG_SIZE         (1<<22)
#define MAX_ALIGNMENT        (65536)
#define MY_BUF_SIZE (MAX_MSG_SIZE + MAX_ALIGNMENT)

char s_buf1[MY_BUF_SIZE];
char r_buf1[MY_BUF_SIZE];

MPI_Request * request;
MPI_Status * reqstat;

double calc_bw(int rank, int size, int num_pairs, int window_size, char *s_buf, char *r_buf);
void usage();

int main(int argc, char *argv[])
{
    char *s_buf, *r_buf;

    int numprocs, rank, align_size;
    int pairs, print_rate;
    int window_size, window_varied;
    int c, curr_size;

    MPI_Init(&argc, &argv);

    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    /* default values */
    pairs            = numprocs / 2;
    window_size      = DEFAULT_WINDOW;
    window_varied    = 0;
    print_rate       = 1;

    while((c = getopt(argc, argv, "p:w:r:vh")) != -1) {
        switch (c) {
            case 'p':
                pairs = atoi(optarg);

                if(pairs > (numprocs / 2)) {
                    if(0 == rank) {
                        usage();
                    }

                    goto error;
                }

                break;

            case 'w':
                window_size = atoi(optarg);
                break;

            case 'v':
                window_varied = 1;
                break;

            case 'r':
                print_rate = atoi(optarg);

                if(0 != print_rate && 1 != print_rate) {
                    if(0 == rank) {
                        usage();
                    }

                    goto error;
                }

                break;

            default:
                if(0 == rank) {
                    usage();
                }

                goto error;
        }
    }

    align_size = getpagesize();
    assert(align_size <= MAX_ALIGNMENT);

    s_buf =
        (char *) (((unsigned long) s_buf1 + (align_size - 1)) /
                  align_size * align_size);
    r_buf =
        (char *) (((unsigned long) r_buf1 + (align_size - 1)) /
                  align_size * align_size);

    if(numprocs < 2) {
        if(rank == 0) {
            fprintf(stderr, "This test requires at least two processes\n");
        }

        MPI_Finalize();

        return EXIT_FAILURE;
    }

    if(rank == 0) {
        fprintf(stdout, "# %s v%s\n", BENCHMARK, PACKAGE_VERSION);

        if(window_varied) {
            fprintf(stdout, "# [ pairs: %d ] [ window size: varied ]\n", pairs);
            fprintf(stdout, "\n# Uni-directional Bandwidth (MB/sec)\n");
        }

        else {
            fprintf(stdout, "# [ pairs: %d ] [ window size: %d ]\n", pairs,
                    window_size);

            if(print_rate) {
                fprintf(stdout, "%-*s%*s%*s\n", 10, "# Size", FIELD_WIDTH,
                        "MB/s", FIELD_WIDTH, "Messages/s");
            }

            else {
                fprintf(stdout, "%-*s%*s\n", 10, "# Size", FIELD_WIDTH, "MB/s");
            }
        }

        fflush(stdout);
    }

   /* More than one window size */

   if(window_varied) {
       int window_array[] = WINDOW_SIZES;
       double ** bandwidth_results;
       int log_val = 1, tmp_message_size = MAX_MSG_SIZE;
       int i, j;

       for(i = 0; i < WINDOW_SIZES_COUNT; i++) {
           if(window_array[i] > window_size) {
               window_size = window_array[i];
           }
       }

       request = (MPI_Request *) malloc(sizeof(MPI_Request) * window_size);
       reqstat = (MPI_Status *) malloc(sizeof(MPI_Status) * window_size);

       while(tmp_message_size >>= 1) {
           log_val++;
       }

       bandwidth_results = (double **) malloc(sizeof(double *) * log_val);

       for(i = 0; i < log_val; i++) {
           bandwidth_results[i] = (double *)malloc(sizeof(double) *
                   WINDOW_SIZES_COUNT);
       }

       if(rank == 0) {
           fprintf(stdout, "#      ");

           for(i = 0; i < WINDOW_SIZES_COUNT; i++) {
               fprintf(stdout, "  %10d", window_array[i]);
           }

           fprintf(stdout, "\n");
           fflush(stdout);
       }
    
       for(j = 0, curr_size = 1; curr_size <= MAX_MSG_SIZE; curr_size *= 2, j++) {
           if(rank == 0) {
               fprintf(stdout, "%-7d", curr_size);
           }

           for(i = 0; i < WINDOW_SIZES_COUNT; i++) {
               bandwidth_results[j][i] = calc_bw(rank, curr_size, pairs,
                       window_array[i], s_buf, r_buf);

               if(rank == 0) {
                   fprintf(stdout, "  %10.*f", FLOAT_PRECISION,
                           bandwidth_results[j][i]);
               }
           }

           if(rank == 0) {
               fprintf(stdout, "\n");
               fflush(stdout);
           }
       }

       if(rank == 0 && print_rate) {
            fprintf(stdout, "\n# Message Rate Profile\n");
            fprintf(stdout, "#      ");

            for(i = 0; i < WINDOW_SIZES_COUNT; i++) {
                fprintf(stdout, "  %10d", window_array[i]);
            }       

            fprintf(stdout, "\n");
            fflush(stdout);

            for(c = 0, curr_size = 1; curr_size <= MAX_MSG_SIZE; curr_size *= 2) { 
                fprintf(stdout, "%-7d", curr_size); 

                for(i = 0; i < WINDOW_SIZES_COUNT; i++) {
                    double rate = 1e6 * bandwidth_results[c][i] / curr_size;

                    fprintf(stdout, "  %10.2f", rate);
                }       

                fprintf(stdout, "\n");
                fflush(stdout);
                c++;    
            }
       }
   }

   else {
       /* Just one window size */
       request = (MPI_Request *)malloc(sizeof(MPI_Request) * window_size);
       reqstat = (MPI_Status *)malloc(sizeof(MPI_Status) * window_size);

       for(curr_size = 1; curr_size <= MAX_MSG_SIZE; curr_size *= 2) {
           double bw, rate;

           bw = calc_bw(rank, curr_size, pairs, window_size, s_buf, r_buf);

           if(rank == 0) {
               rate = 1e6 * bw / curr_size;

               if(print_rate) {
                   fprintf(stdout, "%-*d%*.*f%*.*f\n", 10, curr_size,
                           FIELD_WIDTH, FLOAT_PRECISION, bw, FIELD_WIDTH,
                           FLOAT_PRECISION, rate);
               }

               else {
                   fprintf(stdout, "%-*d%*.*f\n", 10, curr_size, FIELD_WIDTH,
                           FLOAT_PRECISION, bw);
               }
           } 
       }
   }

error:
   MPI_Finalize();

   return EXIT_SUCCESS;
}

void usage() {
    printf("Options:\n");
    printf("  -r=<0,1>         Print uni-directional message rate (default 1)\n");
    printf("  -p=<pairs>       Number of pairs involved (default np / 2)\n");
    printf("  -w=<window>      Number of messages sent before acknowledgement (64, 10)\n");
    printf("                   [cannot be used with -v]\n");
    printf("  -v               Vary the window size (default no)\n");
    printf("                   [cannot be used with -w]\n");
    printf("  -h               Print this help\n");
    printf("\n");
    printf("  Note: This benchmark relies on block ordering of the ranks.  Please see\n");
    printf("        the README for more information.\n");
    fflush(stdout);
}

double calc_bw(int rank, int size, int num_pairs, int window_size, char *s_buf,
        char *r_buf)
{
    double t_start = 0, t_end = 0, t = 0, maxtime = 0, bw = 0;
    int i, j, target;
    int loop, skip;
    int mult = (DEFAULT_WINDOW / window_size) > 0 ? (DEFAULT_WINDOW /
            window_size) : 1;

    for(i = 0; i < size; i++) {
        s_buf[i] = 'a';
        r_buf[i] = 'b';
    }

    if(size > LARGE_THRESHOLD) {
        loop = ITERS_LARGE * mult;
        skip = WARMUP_ITERS_LARGE * mult;
    }

    else {
        loop = ITERS_SMALL * mult;
        skip = WARMUP_ITERS_SMALL * mult;
    }

    MPI_Barrier(MPI_COMM_WORLD);

    if(rank < num_pairs) {
        target = rank + num_pairs;

        for(i = 0; i < loop + skip; i++) {
            if(i == skip) {
                MPI_Barrier(MPI_COMM_WORLD);
                t_start = MPI_Wtime();
            }

            for(j = 0; j < window_size; j++) {
                MPI_Isend(s_buf, size, MPI_CHAR, target, 100, MPI_COMM_WORLD,
                        request + j);
            }

            MPI_Waitall(window_size, request, reqstat);
            MPI_Recv(r_buf, 4, MPI_CHAR, target, 101, MPI_COMM_WORLD,
                    &reqstat[0]);
        }

        t_end = MPI_Wtime();
        t = t_end - t_start;
    }

    else if(rank < num_pairs * 2) {
        target = rank - num_pairs;

        for(i = 0; i < loop + skip; i++) {
            if(i == skip) {
                MPI_Barrier(MPI_COMM_WORLD);
            }

            for(j = 0; j < window_size; j++) {
                MPI_Irecv(r_buf, size, MPI_CHAR, target, 100, MPI_COMM_WORLD,
                        request + j);
            }

            MPI_Waitall(window_size, request, reqstat);
            MPI_Send(s_buf, 4, MPI_CHAR, target, 101, MPI_COMM_WORLD);
        }
    }

    else {
        MPI_Barrier(MPI_COMM_WORLD);
    }

    MPI_Reduce(&t, &maxtime, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);

    if(rank == 0) {
        double tmp = num_pairs * size / 1e6;

        tmp = tmp * loop * window_size;
        bw = tmp / maxtime;

        return bw;
    }

    return 0;
}

/* vi: set sw=4 sts=4 tw=80: */
