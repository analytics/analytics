#ifndef _hs_snappy_h
#define _hs_snappy_h

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

struct BS {
  const char *ptr;
  size_t off;
  size_t len;
};

size_t _hsnappy_MaxCompressedLength(size_t);

void _hsnappy_RawCompress(const char *input, size_t input_length,
			  char *compressed, size_t *compressed_length);

int _hsnappy_GetUncompressedLength(const char *compressed,
				   size_t compressed_length,
				   size_t *result);

int _hsnappy_RawUncompress(const char *compressed, size_t compressed_length,
			   char *uncompressed);

struct BS;

void _hsnappy_CompressChunks(struct BS *chunks, size_t count,
			     size_t length, char *compressed,
			     size_t *compressed_length);

struct BSSource;

struct BSSource *_hsnappy_NewSource(struct BS *chunks, size_t nchunks,
				    size_t length);

void _hsnappy_DeleteSource(struct BSSource *reader);

int _hsnappy_UncompressChunks(struct BSSource *reader, char *uncompressed);

int _hsnappy_GetUncompressedLengthChunks(struct BSSource *reader,
					 uint32_t *result);

#ifdef __cplusplus
}
#endif

#endif /* _hs_snappy_h */
