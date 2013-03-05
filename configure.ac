AC_PREREQ([2.68])
AC_INIT([analytics], [0.1], [ekmett@gmail.com])
AC_CONFIG_HEADER(config.h)
AC_CONFIG_SRCDIR([cbits/fast.c])
AM_INIT_AUTOMAKE([foreign])

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL
AC_CANONICAL_HOST

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CHECK_FUNCS(ftruncate posix_fallocate)

AC_CACHE_CHECK([for fallocate],analytics_cv_have_fallocate,[
AC_TRY_LINK([#include <fcntl.h>
#include <sys/types.h>],
[fallocate(0, 0, 0, 0);],
analytics_have_fallocate=yes,analytics_cv_have_fallocate=no)])

if test x"$analytics_have_fallocate" = x"yes"; then
  AC_DEFINE(HAVE_FALLOCATE, 1, [Define to 1 if you have the fallocate function and it compiles and links without error])
fi

AC_CACHE_CHECK([for SYS_fallocate],analytics_cv_have_sys_fallocate,[
AC_TRY_COMPILE([#include <sys/syscall.h>
#include <sys/types.h>],
[syscall(SYS_fallocate, 0, 0, (loff_t)0, (loff_t)0);],
analytics_cv_have_sys_fallocate=yes,analytics_cv_have_sys_fallocate=no)])
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

AC_CACHE_CHECK([for broken largefile support],analytics_cv_HAVE_BROKEN_LARGEFILE,[
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
analytics_cv_HAVE_BROKEN_LARGEFILE=yes,analytics_cv_HAVE_BROKEN_LARGEFILE=no,analytics_cv_HAVE_BROKEN_LARGEFILE=cross)])
if test x"$analytics_cv_HAVE_BROKEN_LARGEFILE" != x"yes"; then
   AC_SYS_LARGEFILE
fi

AC_CONFIG_FILES([Makefile analytics.buildinfo man/Makefile])
AC_CONFIG_SUBDIRS([man])
AC_OUTPUT
