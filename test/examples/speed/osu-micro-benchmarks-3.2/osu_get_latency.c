#define BENCHMARK "OSU One Sided MPI_Get latency Test"
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
#include <string.h>

#define MESSAGE_ALIGNMENT 64
#define MAX_SIZE (1<<22)
#define MYBUFSIZE (MAX_SIZE + MESSAGE_ALIGNMENT)

#define skip 100
#define INER_LOOP 1
#define LOOP 1000

char        A[MYBUFSIZE];
char        B[MYBUFSIZE];

int main (int argc, char *argv[])
{
    int         rank, destrank, nprocs, i;
    int         align_size;

    char       *s_buf, *r_buf;
    MPI_Group   comm_group, group;
    MPI_Win     win;
    int         loop;
    int         size;
    double      t_start, t_end;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if(nprocs != 2) {
        if(rank == 0) {
            fprintf(stderr, "This test requires exactly two processes\n");
        }

        MPI_Finalize();

        return EXIT_FAILURE;
    }

    align_size = MESSAGE_ALIGNMENT;
    loop = LOOP;
    s_buf =
        (char *) (((unsigned long) A + (align_size - 1)) /
                  align_size * align_size);
    r_buf =
        (char *) (((unsigned long) B + (align_size - 1)) /
                  align_size * align_size);

    memset(r_buf, 0, MAX_SIZE);
    memset(s_buf, 1, MAX_SIZE);

    if(rank == 0) {
        fprintf(stdout, "# %s v%s\n", BENCHMARK, PACKAGE_VERSION);
        fprintf(stdout, "%-*s%*s\n", 10, "# Size", FIELD_WIDTH, "Latency (us)"); 
        fflush(stdout);
    }

    MPI_Comm_group(MPI_COMM_WORLD, &comm_group);

    for(size = 0; size <= MAX_SIZE; size = (size ? size * 2 : size + 1)) {
        if(rank == 0) {
            MPI_Win_create(s_buf, size, 1, MPI_INFO_NULL, MPI_COMM_WORLD, &win);

            destrank = 1;

            MPI_Group_incl(comm_group, 1, &destrank, &group);
            MPI_Barrier(MPI_COMM_WORLD);

            for(i = 0; i < skip + loop; i++) {
                MPI_Win_start(group, 0, win);

                if (i == skip) {
                    t_start = MPI_Wtime ();
                }

                MPI_Get(r_buf, size, MPI_CHAR, 1, 0, size, MPI_CHAR, win);
                MPI_Win_complete(win);
                MPI_Win_post(group, 0, win);
                MPI_Win_wait(win);
            }

            t_end = MPI_Wtime ();
        }

        else {
            /* rank=1 */
            MPI_Win_create(s_buf, size, 1, MPI_INFO_NULL, MPI_COMM_WORLD, &win);

            destrank = 0;

            MPI_Group_incl(comm_group, 1, &destrank, &group);
            MPI_Barrier(MPI_COMM_WORLD);

            for(i = 0; i < skip + loop; i++) {
                MPI_Win_post(group, 0, win);
                MPI_Win_wait(win);
                MPI_Win_start(group, 0, win);
                MPI_Get(r_buf, size, MPI_CHAR, 0, 0, size, MPI_CHAR, win);
                MPI_Win_complete(win);
            }
        }

        if(rank == 0) {
            fprintf(stdout, "%-*d%*.*f\n", 10, size, FIELD_WIDTH,
                    FLOAT_PRECISION, (t_end - t_start) * 1.0e6 / loop / 2);
            fflush(stdout);
        }

        MPI_Barrier(MPI_COMM_WORLD);
        MPI_Group_free(&group);
        MPI_Win_free(&win);
    }

    MPI_Group_free(&comm_group);
    MPI_Finalize();

    return EXIT_SUCCESS;
}

/* vi: set sw=4 sts=4 tw=80: */
