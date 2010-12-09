#define BENCHMARK "OSU MPI One Sided MPI_Accumulate Latency Test"
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

#define SKIP 100
#define LOOP 1000

#define MESSAGE_ALIGNMENT 64
#define MAX_SIZE (1<<22)
#define MYBUFSIZE (MAX_SIZE + MESSAGE_ALIGNMENT)

char        s_buf_original[MYBUFSIZE];
char        r_buf_original[MYBUFSIZE];

int main (int argc, char *argv[])
{
    int         rank, destrank, nprocs, i;
    MPI_Group   comm_group, group;
    MPI_Win     win;

    int         loop;
    int         size;
    double      t_start, t_end;

    int         count, align_size;
    int        *s_buf;
    int        *r_buf;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_group(MPI_COMM_WORLD, &comm_group);

    if(nprocs != 2) {
        if(rank == 0) {
            fprintf(stderr, "This test requires exactly two processes\n");
        }

        MPI_Finalize();

        return EXIT_FAILURE;
    }

    loop = LOOP;
    align_size = MESSAGE_ALIGNMENT;

    s_buf =
        (int *) (((unsigned long) s_buf_original + (align_size - 1)) /
                 align_size * align_size);
    r_buf =
        (int *) (((unsigned long) r_buf_original + (align_size - 1)) /
                 align_size * align_size);

    for(i = 0; i < MAX_SIZE / sizeof(int); i++) {
        r_buf[i] = i;
        s_buf[i] = 2 * i;
    }

    if(rank == 0) {
        fprintf(stdout, "# %s v%s\n", BENCHMARK, PACKAGE_VERSION);
        fprintf(stdout, "%-*s%*s\n", 10, "# Size", FIELD_WIDTH, "Latency (us)");
        fflush(stdout);
    }

    for(count = 0; count <= MAX_SIZE / sizeof(int); (count <<= 1) || (count += 1)) {
        size = count * sizeof(int);

        if(rank == 0) {
            MPI_Win_create(r_buf, size, 1, MPI_INFO_NULL, MPI_COMM_WORLD, &win);

            destrank = 1;

            MPI_Group_incl(comm_group, 1, &destrank, &group);
            MPI_Barrier(MPI_COMM_WORLD);

            for(i = 0; i < SKIP + loop; i++) {
                MPI_Win_start (group, 0, win);

                if(i == SKIP) {
                    t_start = MPI_Wtime ();
                }

                MPI_Accumulate(s_buf, count, MPI_INT, 1, 0, count, MPI_INT,
                        MPI_SUM, win);
                MPI_Win_complete(win);
                MPI_Win_post(group, 0, win);
                MPI_Win_wait(win);
            }

            t_end = MPI_Wtime ();
        }

        else {
            MPI_Win_create(r_buf, size, 1, MPI_INFO_NULL, MPI_COMM_WORLD, &win);

            destrank = 0;

            MPI_Group_incl(comm_group, 1, &destrank, &group);
            MPI_Barrier(MPI_COMM_WORLD);

            for(i = 0; i < SKIP + loop; i++) {
                MPI_Win_post(group, 0, win);
                MPI_Win_wait(win);
                MPI_Win_start(group, 0, win);
                MPI_Accumulate(s_buf, count, MPI_INT, 0, 0, count, MPI_INT,
                        MPI_SUM, win);
                MPI_Win_complete (win);
            }
        }

        if(rank == 0) {
            fprintf(stdout, "%-*d%*.*f\n", 10, size, FIELD_WIDTH,
                    FLOAT_PRECISION, (t_end - t_start) * 1e6 / loop / 2);
            fflush(stdout);
        }

        MPI_Barrier(MPI_COMM_WORLD);
        MPI_Group_free(&group);
        MPI_Win_free(&win);
    }

    MPI_Group_free(&comm_group);
    MPI_Finalize ();

    return 0;
}

/* vi: set sw=4 sts=4 tw=80: */
