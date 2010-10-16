#define BENCHMARK "OSU MPI Multi-threaded Latency Test"
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
#include <pthread.h>

#define MESSAGE_ALIGNMENT 64
#define MAX_MSG_SIZE (1<<22)
#define MYBUFSIZE (MAX_MSG_SIZE + MESSAGE_ALIGNMENT)
#define THREADS 2

char        s_buf1[MYBUFSIZE];
char        r_buf1[MYBUFSIZE];

int         skip = 1000;
int         loop = 10000;
int         skip_large = 10;
int         loop_large = 100;
int         large_message_size = 8192;

pthread_mutex_t finished_size_mutex;
pthread_cond_t  finished_size_cond;

int finished_size;

MPI_Status  reqstat[THREADS];

typedef struct thread_tag {
        int id;
} thread_tag_t;

void * send_thread(void *arg);
void * recv_thread(void *arg);

int main(int argc, char *argv[])
{
    int numprocs, provided, myid, err;
    int i = 0;

    pthread_t sr_threads[THREADS];
    thread_tag_t tags[THREADS];

    pthread_mutex_init(&finished_size_mutex, NULL);
    pthread_cond_init(&finished_size_cond, NULL);

    err = MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);

    if(err != MPI_SUCCESS) {
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &myid);

    if(numprocs != 2) {
        if(myid == 0) {
            fprintf(stderr, "This test requires exactly two processes\n");
        }

        MPI_Finalize();

        return EXIT_FAILURE;
    }

    /* Check to make sure we actually have a thread-safe
     * implementation 
     */

    finished_size = 1;

    if(provided != MPI_THREAD_MULTIPLE) {
        if(myid == 0) {
            fprintf(stderr,
                "MPI_Init_thread must return MPI_THREAD_MULTIPLE!\n");
        }

        MPI_Finalize();

        return EXIT_FAILURE;
    }

    if(myid == 0) {
        fprintf(stdout, "# %s v%s\n", BENCHMARK, PACKAGE_VERSION);
        fprintf(stdout, "%-*s%*s\n", 10, "# Size", FIELD_WIDTH, "Latency (us)");
        fflush(stdout);

        tags[i].id = i;
        pthread_create(&sr_threads[i], NULL,
                send_thread, &tags[i]);
        pthread_join(sr_threads[i], NULL);

    }

    else {
        for(i = 0; i < THREADS; i++) {
            tags[i].id = i;
            pthread_create(&sr_threads[i], NULL, recv_thread, &tags[i]);
        }

        for(i = 0; i < THREADS; i++) {
            pthread_join(sr_threads[i], NULL);
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}

void * recv_thread(void *arg) {
    int size, i, j, val, align_size;
    int local_window_size, local_start;
    int start_send, send_size;
    int messages_recv = 0, iter;
    char *s_buf, *r_buf;
    double t_start = 0, t_end = 0, t = 0;
    thread_tag_t *thread_id;

    thread_id = (thread_tag_t *)arg;
    val = thread_id->id;

    align_size = MESSAGE_ALIGNMENT;

    s_buf =
        (char *) (((unsigned long) s_buf1 + (align_size - 1)) /
                  align_size * align_size);
    r_buf =
        (char *) (((unsigned long) r_buf1 + (align_size - 1)) /
                  align_size * align_size);


    for(size = 1, iter = 0; size <= MAX_MSG_SIZE; size *= 2) {
        pthread_mutex_lock(&finished_size_mutex);

        if(finished_size == THREADS) {
            MPI_Barrier(MPI_COMM_WORLD);

            finished_size = 1;

            pthread_mutex_unlock(&finished_size_mutex);
            pthread_cond_broadcast(&finished_size_cond);
        }

        else {
            finished_size++;

            pthread_cond_wait(&finished_size_cond, &finished_size_mutex);
            pthread_mutex_unlock(&finished_size_mutex);
        }

        if(size > large_message_size) {
            loop = loop_large;
            skip = skip_large;
        }  

        /* touch the data */
        for(i = 0; i < size; i++) {
            s_buf[i] = 'a';
            r_buf[i] = 'b';
        }

        for(i = val; i < (loop + skip); i += THREADS) {
            MPI_Recv (r_buf, size, MPI_CHAR, 0, 1, MPI_COMM_WORLD,
                    &reqstat[val]);
            MPI_Send (s_buf, size, MPI_CHAR, 0, 2, MPI_COMM_WORLD);
        }

        iter++;
    }

    sleep(1);

    return 0;
}


void * send_thread(void *arg) {
    int size, i, j, k, val, align_size, iter;
    int local_start, local_window_size;
    int start_send, send_size;
    char *s_buf, *r_buf;
    double t_start = 0, t_end = 0, t = 0, latency, t_sum;
    thread_tag_t *thread_id = (thread_tag_t *)arg;

    val = thread_id->id;
    align_size = MESSAGE_ALIGNMENT;

    s_buf =
        (char *) (((unsigned long) s_buf1 + (align_size - 1)) /
                  align_size * align_size);
    r_buf =
        (char *) (((unsigned long) r_buf1 + (align_size - 1)) /
                  align_size * align_size);

    for(size = 1, iter = 0; size <= MAX_MSG_SIZE; size *= 2) {
        MPI_Barrier(MPI_COMM_WORLD);

        if(size > large_message_size) {
            loop = loop_large;
            skip = skip_large;
        }  

        /* touch the data */
        for(i = 0; i < size; i++) {
            s_buf[i] = 'a';
            r_buf[i] = 'b';
        }

        for(i = 0; i < loop + skip; i++) {
            if(i == skip) {
                t_start = MPI_Wtime();
            }

            MPI_Send(s_buf, size, MPI_CHAR, 1, 1, MPI_COMM_WORLD);
            MPI_Recv(r_buf, size, MPI_CHAR, 1, 2, MPI_COMM_WORLD,
                    &reqstat[val]);
        }

        t_end = MPI_Wtime ();
        t = t_end - t_start;

        latency = (t) * 1.0e6 / (2.0 * loop);
        fprintf(stdout, "%-*d%*.*f\n", 10, size, FIELD_WIDTH, FLOAT_PRECISION,
                latency);
        fflush(stdout);
        iter++;
    }

    return 0;
}

/* vi: set sw=4 sts=4 tw=80: */
