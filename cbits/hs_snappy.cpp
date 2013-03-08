#include "hs_snappy.h"
#include "snappy.h"
#include "snappy-sinksource.h"

using namespace snappy;

size_t _hsnappy_MaxCompressedLength(size_t n)
{
  return MaxCompressedLength(n);
}

void _hsnappy_RawCompress(const char *input, size_t input_length,
			  char *compressed, size_t *compressed_length)
{
  RawCompress(input, input_length, compressed, compressed_length);
}

int _hsnappy_GetUncompressedLength(const char *compressed,
				   size_t compressed_length,
				   size_t *result)
{
  return GetUncompressedLength(compressed, compressed_length, result);
}

int _hsnappy_RawUncompress(const char *compressed, size_t compressed_length,
			   char *uncompressed)
{
  return RawUncompress(compressed, compressed_length, uncompressed);
}

class BSSource : public Source
{
public:
  BSSource(BS *chunks, size_t nchunks, size_t size)
    : chunks_(chunks), nchunks_(nchunks), size_(size), cur_(chunks),
      left_(size) { }

  size_t Available() const { return left_; }

  const char *Peek(size_t *len) {
    if (left_ > 0) {
      *len = cur_->len - cur_->off;
      return cur_->ptr + cur_->off;
    } else {
      *len = 0;
      return NULL;
    }
  }

  void Skip(size_t n) {
    if (n > 0) {
      left_ -= n;
      cur_->off += n;
      if (cur_->off == cur_->len)
	cur_++;
    }
  }

  void Rewind() {
    left_ = size_;
    cur_ = chunks_;
    for (size_t i = 0; i < nchunks_ && chunks_[i].off > 0; i++)
      chunks_[i].off = 0;
  }

private:
  BS *chunks_;
  const size_t nchunks_;
  const size_t size_;
  BS *cur_;
  size_t left_;
};

void _hsnappy_CompressChunks(BS *chunks, size_t nchunks, size_t length,
			     char *compressed, size_t *compressed_length)
{
  BSSource reader(chunks, nchunks, length);
  UncheckedByteArraySink writer(compressed);

  Compress(&reader, &writer);

  *compressed_length = writer.CurrentDestination() - compressed;
}

BSSource *_hsnappy_NewSource(BS *chunks, size_t nchunks, size_t length)
{
  return new BSSource(chunks, nchunks, length);
}

void _hsnappy_DeleteSource(BSSource *src)
{
  delete src;
}

int _hsnappy_UncompressChunks(BSSource *reader, char *uncompressed)
{
  return RawUncompress(reader, uncompressed);
}

int _hsnappy_GetUncompressedLengthChunks(BSSource *reader, uint32_t *result)
{
  int n = GetUncompressedLength(reader, result);
  reader->Rewind();
  return n;
}
