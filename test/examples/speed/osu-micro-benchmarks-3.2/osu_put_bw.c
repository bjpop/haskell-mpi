#define BENCHMARK "OSU One Sided MPI_Put Bandwidth Test"
/*
 * Copyright (C) 2003-2010 the Network-Based Computing Laboratory
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
#include <assert.h>

#define MAX_ALIGNMENT 65536
#define MYBUFSIZE (150000000)   /* ~= 100M Bytes */
#define MAX_REQ_NUM 100

/* Note we have a upper limit for buffer size, so be extremely careful
 * if you want to change the loop size or warm up size */
#define WARMUP  (10)
#define MAX_SIZE (1<<22)
#define LOOP (30)
#define WINDOW_SIZE     (32)

char        s_buf1[MAX_SIZE + MAX_ALIGNMENT];
char        r_buf1[MYBUFSIZE];
MPI_Request request[MAX_REQ_NUM];

int main (int argc, char *argv[])
{
    int         myid, numprocs, i, j;
    int         size, loop, page_size;
    char       *s_buf, *r_buf;
    double      t_start = 0.0, t_end = 0.0, t = 0.0;
    int         destrank;

    MPI_Group   comm_group, group;
    MPI_Win     win;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &myid);
    MPI_Comm_group(MPI_COMM_WORLD, &comm_group);

    if(numprocs != 2) {
        if(myid == 0) {
            fprintf(stderr, "This test requires exactly two processes\n");
        }

        MPI_Finalize();

        return EXIT_FAILURE;
    }

    loop = LOOP;
    page_size = getpagesize();
    assert(page_size <= MAX_ALIGNMENT);

    s_buf =
        (char *) (((unsigned long) s_buf1 + (page_size - 1)) / page_size *
                  page_size);
    r_buf =
        (char *) (((unsigned long) r_buf1 + (page_size - 1)) / page_size *
                  page_size);

    assert((s_buf != NULL) && (r_buf != NULL));
    assert(MAX_SIZE * WINDOW_SIZE < MYBUFSIZE);

    if(myid == 0) {
        fprintf(stdout, "# %s v%s\n", BENCHMARK, PACKAGE_VERSION);
        fprintf(stdout, "%-*s%*s\n", 10, "# Size", FIELD_WIDTH,
                "Bandwidth (MB/s)");
        fflush(stdout);
    }

    /* Bandwidth test */
    for(size = 1; size <= MAX_SIZE; size *= 2) {
        /* Window creation and warming-up */
        if(myid == 0) {
            MPI_Win_create(r_buf, size, 1, MPI_INFO_NULL, MPI_COMM_WORLD, &win);

            destrank = 1;

            MPI_Group_incl (comm_group, 1, &destrank, &group);

            for(i = 0; i < WARMUP; i++) {
                MPI_Win_start(group, 0, win);
                MPI_Put(s_buf, size, MPI_CHAR, 1, i * size, size, MPI_CHAR,
                        win);
                MPI_Win_complete(win);
            }
        }

        else {
            MPI_Win_create(r_buf, size * WINDOW_SIZE, 1, MPI_INFO_NULL,
                    MPI_COMM_WORLD, &win);

            destrank = 0;

            MPI_Group_incl(comm_group, 1, &destrank, &group);

            for(i = 0; i < WARMUP; i++) {
                MPI_Win_post(group, 0, win);
                MPI_Win_wait(win);
            }
        }

        MPI_Barrier(MPI_COMM_WORLD);

        if(myid == 0) {
            t_start = MPI_Wtime();

            for(i = 0; i < loop; i++) {
                MPI_Win_start(group, 0, win);

                for(j = 0; j < WINDOW_SIZE; j++) {
                    MPI_Put(s_buf, size, MPI_CHAR, 1, j * size, size, MPI_CHAR,
                            win);
                }

                MPI_Win_complete(win);
            }

            t_end = MPI_Wtime();
            MPI_Barrier(MPI_COMM_WORLD);

            t = t_end - t_start;
        }

        else {
            for(i = 0; i < loop; i++) {
                MPI_Win_post(group, 0, win);
                MPI_Win_wait(win);
            }

            MPI_Barrier (MPI_COMM_WORLD);
        }

        if(myid == 0) {
            double tmp = size / 1e6 * loop * WINDOW_SIZE;

            fprintf(stdout, "%-*d%*.*f\n", 10, size, FIELD_WIDTH,
                    FLOAT_PRECISION, tmp / t);
            fflush(stdout);
        }

        MPI_Group_free(&group);
        MPI_Win_free(&win);
    } 
    
    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Group_free(&comm_group);
    MPI_Finalize();

    return EXIT_SUCCESS;
}

/* vi: set sw=4 sts=4 tw=80: */
