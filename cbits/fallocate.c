#define _GNU_SOURCE

#include "config.h"

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

int c_fallocate(int fd, off_t len) {
#if HAVE_FALLOCATE
  return fallocate(fd, 0, 0, len);
#elif HAVE_POSIX_FALLOCATE
  return posix_fallocate(fd, 0, len);
#elif HAVE_FTRUNCATE
  fstore_t store = { F_ALLOCATECONTIG, F_PEOFPOSMODE, 0, len };
  int result = fcntl(fd, F_PREALLOCATE, &store);
  if (result == -1) {
   store.fst_flags = F_ALLOCATEALL;
   result = fcntl(fd, F_PREALLOCATE, &store);
   if (result == -1) return 0;
  }
  return ftruncate(fd,len) == 0;
#else
#error "fallocate: Build issue"
#endif
}
