#define _GNU_SOURCE

#include "config.h"

#ifdef HAVE_IO_H
#include <io.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_SYS_FCNTL_H
#include <sys/fcntl.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

int c_fallocate(int fd, off_t len) {
#if HAVE_FALLOCATE64
  return fallocate64(fd, 0, 0, len);
#elif HAVE_FALLOCATE
  return fallocate(fd, 0, 0, len);
#elif HAVE_POSIX_FALLOCATE
  return posix_fallocate(fd, 0, len);
#elif HAVE_FSTORE_T && HAVE_FTRUNCATE /* Mac OS X */
  fstore_t store = { F_ALLOCATECONTIG, F_PEOFPOSMODE, 0, len };
  int result = fcntl(fd, F_PREALLOCATE, &store);
  if (result == -1) {
    store.fst_flags = F_ALLOCATEALL;
    result = fcntl(fd, F_PREALLOCATE, &store);
    if (result == -1) return 0;
  }
  return ftruncate(fd,len) == 0;
#else
  /* TODO: emulate fallocate */
#error Unable to fallocate on this platform
#endif
}

int c_prefetch(int fd UNUSED, off_t off UNUSED, size_t count UNUSED) {
#ifdef HAVE_POSIX_FADVISE
  return posix_fadvise(fd, off, count, POSIX_FADV_WILLNEED);
#elif HAVE_RADVISORY
  struct radvisory rad = { off, count };
  return fcntl(fd, F_RDADVISE, &rad);
#else
  return 0;
#endif
}

int c_sync(int fd) {
#ifdef HOST_WIN32
  return _commit(fd);
#else
  return fsync(fd);
#endif
}
