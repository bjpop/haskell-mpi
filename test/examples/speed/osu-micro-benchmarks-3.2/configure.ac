#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.59])
AC_INIT([OSU-Micro-Benchmarks], [3.2], [mvapich-discuss@cse.ohio-state.edu])
AC_CONFIG_SRCDIR([osu_get_latency.c])
AC_CONFIG_HEADERS([osu.h])
AH_BOTTOM([#include <mpi.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>])

AM_INIT_AUTOMAKE([foreign])

# Checks for programs.
AC_PROG_CC([mpicc])

# Checks for libraries.
AC_SEARCH_LIBS([sqrt], [m])

# Checks for header files.
AC_CHECK_HEADERS([stdlib.h string.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_INLINE

# Checks for library functions.
AC_FUNC_MALLOC
AC_CHECK_FUNCS([getpagesize gettimeofday memset sqrt])
AC_CHECK_FUNC([MPI_Accumulate], [mpi2_library=true])

AM_CONDITIONAL([MPI2_LIBRARY], [test x$mpi2_library = xtrue])

AC_DEFINE([FIELD_WIDTH], [20], [Width of field used to report numbers])
AC_DEFINE([FLOAT_PRECISION], [2], [Precision of reported numbers])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
