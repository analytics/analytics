AC_PREREQ([2.68])
AC_INIT([analytics], [0.1], [ekmett@gmail.com])
AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_HEADERS([config.h])
AC_SUBST(ANALYTICS_COPYRIGHT,["Copyright 2013 Edward Kmett. All Rights Reserved."])
AM_INIT_AUTOMAKE([foreign])

# Checks for programs.
AC_CANONICAL_BUILD
AC_CANONICAL_HOST

case "${host_os}" in
  *mingw32*)
    AC_DEFINE(HOST_MINGW32, 1, [Define if you are running on Windows under 32-bit MinGW.])
    AC_DEFINE(HOST_WIN32, 1, [Define if you are running on 32-bit Windows.])
  ;;
  darwin*)
    AC_DEFINE(HOST_DARWIN, 1, [Define if you are running on Darwin/Mac OS X.])
  ;;
esac

AC_PROG_CC
AC_PROG_CXX
AC_LANG([C++])
AC_PROG_INSTALL

# Checks for libraries.

# Checks for header files.

AC_CHECK_HEADERS_ONCE([windows.h])
AC_CHECK_HEADERS_ONCE([unistd.h io.h stdint.h stddef.h fcntl.h byteswap.h])
AC_CHECK_HEADERS_ONCE([sys/mman.h sys/resource.h sys/byteswap.h sys/endian.h sys/time.h sys/fcntl.h sys/syscall.h sys/types.h])

# Checks for typedefs, structures, and compiler characteristics.

AX_C_HAVE_ATTRIBUTE
AX_C_HAVE_ATTRIBUTE_COLD
AC_C_BIGENDIAN(AC_SUBST([ENDIAN],[big]),AC_SUBST([ENDIAN],[little]))
AC_C_INLINE

AC_TYPE_OFF_T
AC_TYPE_SIZE_T

AC_TRY_COMPILE([#include <fcntl.h>],[fstore_t store = { F_ALLOCATECONTIG, F_PEOFPOSMODE, 0, 4096 };],[analytics_cv_have_fstore=yes],[analytics_cv_have_fstore=no])
if test x"$analytics_cv_have_fstore" = x"yes"; then
  AC_DEFINE(HAVE_FSTORE_T, 1, [Define to 1 if you can ask fstore_t for continuous storage with F_ALLOCATECONTIG])
fi

# Checks for library functions.

AC_FUNC_FSEEKO

AC_CHECK_DECLS(posix_fadvise, [], [], [
#define _XOPEN_SOURCE 600
#include <fcntl.h>])
AC_CHECK_FUNCS(ftruncate fallocate64 posix_fallocate posix_fadvise posix_memalign pread pwrite pread64 pwrite64 valloc lseek64)

AC_CACHE_CHECK([for fallocate],analytics_cv_have_fallocate,[
AC_TRY_LINK([#include <fcntl.h>
#include <sys/types.h>],[fallocate(0, 0, 0, 0);],analytics_have_fallocate=yes,analytics_cv_have_fallocate=no)])

if test x"$analytics_have_fallocate" = x"yes"; then
  AC_DEFINE(HAVE_FALLOCATE, 1, [Define to 1 if you have the fallocate function and it compiles and links without error])
fi

AC_CACHE_CHECK([for SYS_fallocate],analytics_cv_have_sys_fallocate,[
AC_TRY_COMPILE([#include <sys/syscall.h>
#include <sys/types.h>],[syscall(SYS_fallocate, 0, 0, (loff_t)0, (loff_t)0);],analytics_cv_have_sys_fallocate=yes,analytics_cv_have_sys_fallocate=no)])
if test x"$analytics_have_sys_fallocate" = x"yes"; then
  AC_DEFINE(HAVE_SYS_FALLOCATE, 1, [Define to 1 if you have the SYS_fallocate syscall number])
fi

if test x"$ac_cv_func_posix_fallocate" = x"yes"; then
  AC_MSG_CHECKING([whether posix_fallocate is efficient])
  case $host_os in
  *cygwin*)
    AC_MSG_RESULT(yes)
    AC_DEFINE(HAVE_EFFICIENT_POSIX_FALLOCATE, 1,
      [Define if posix_fallocate is efficient (Cygwin)])
    ;;
  *)
    AC_MSG_RESULT(no)
    ;;
  esac
fi

AC_CACHE_CHECK([for broken largefile support],analytics_cv_have_broken_largefile,[
AC_TRY_RUN([
#define _FILE_OFFSET_BITS 64
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>

int main(void) {
  struct flock lock;
  int status;
  char tpl[32] = "/tmp/locktest.XXXXXX";
  int fd = mkstemp(tpl);
  if (fd < 0) {
    strcpy(tpl, "conftest.dat");
    fd = open(tpl, O_CREAT|O_RDWR, 0600);
  }

  lock.l_type = F_WRLCK;
  lock.l_whence = SEEK_SET;
  lock.l_start = 0;
  lock.l_len = 1;
  lock.l_pid = 0;
  fcntl(fd,F_SETLK,&lock);
  if (fork() == 0) {
    lock.l_start = 1;
    _exit(fcntl(fd,F_SETLK,&lock) == 0);
  }
  wait(&status);
  unlink(tpl);
  exit(WEXITSTATUS(status));
}
],
analytics_cv_have_broken_largefile=yes,analytics_cv_have_broken_largefile=no,analytics_cv_have_broken_largefile=cross)])
if test x"$analytics_cv_have_broken_largefile" != x"yes"; then
   AC_SYS_LARGEFILE
fi

AH_VERBATIM([GCC_VERSION],
[/* Simplified version checking for GCC */
#ifdef __GNUC__
#define GCC_VERSION (__GNUC__ * 10000 + __GNUC_MINOR__ * 100 + __GNUC_PATCHLEVEL__)
#endif
])

# abuse GNU attributes. NB. the __attribute__ macro below could encompass all of these uses.
AH_BOTTOM(
[/* Encapsulate a number of optional GNU attributes */
#ifdef HAVE_ATTRIBUTE
# define NORETURN __attribute__ ((noreturn))
# define ALWAYS_INLINE __attribute__ ((always_inline)) inline
# define UNUSED __attribute__ ((unused))
# define ALIGNED(x) __attribute__ ((aligned(x))
#else
# define __attribute__(x)
# define NORETURN
# define ALWAYS_INLINE inline
# define UNUSED
# define ALIGNED
#endif

# ifdef HAVE_ATTRIBUTE_COLD
#  define NORETURN_COLD __attribute__ ((noreturn,cold))
#  define COLD __attribute__ ((cold))
# else 
#  define NORETURN_COLD NORETURN
#  define COLD
# endif
])

# Don't use AC_FUNC_MMAP, as it checks for mappings of already-mapped memory,
# which we don't need (and does not exist on Windows).
AC_CHECK_FUNC([mmap])

# See if we have __builtin_expect.
# TODO: Use AC_CACHE.
AC_MSG_CHECKING([if the compiler supports __builtin_expect])
AC_TRY_COMPILE(, [
    return __builtin_expect(1, 1) ? 1 : 0
], [
    analytics_have_builtin_expect=yes
    AC_MSG_RESULT([yes])
], [
    analytics_have_builtin_expect=no
    AC_MSG_RESULT([no])
])
if test x$analytics_have_builtin_expect = xyes ; then
    AC_DEFINE([HAVE_BUILTIN_EXPECT], [1], [Define to 1 if the compiler supports __builtin_expect.])
fi

# See if we have working count-trailing-zeros intrinsics.
# TODO: Use AC_CACHE.
AC_MSG_CHECKING([if the compiler supports __builtin_ctzll])
AC_TRY_COMPILE(, [
    return (__builtin_ctzll(0x100000000LL) == 32) ? 1 : 0
], [
    analytics_have_builtin_ctz=yes
    AC_MSG_RESULT([yes])
], [
    analytics_have_builtin_ctz=no
    AC_MSG_RESULT([no])
])
if test x$analytics_have_builtin_ctz = xyes ; then
    AC_DEFINE([HAVE_BUILTIN_CTZ], [1], [Define to 1 if the compiler supports __builtin_ctz and friends.])
fi

AC_CONFIG_FILES([Makefile analytics.buildinfo man/Makefile])
AC_CONFIG_SUBDIRS([man])
AC_OUTPUT
