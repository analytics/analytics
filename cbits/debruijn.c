// A 64 bit deBruijn multiplication table for calculating the the value of a single bit in a word64
const unsigned char debruijn_lsb64[] = {
 63,  0, 58,  1, 59, 47, 53,  2,
 60, 39, 48, 27, 54, 33, 42,  3,
 61, 51, 37, 40, 49, 18, 28, 20,
 55, 30, 34, 11, 43, 14, 22,  4,
 62, 57, 46, 52, 38, 26, 32, 41,
 50, 36, 17, 19, 29, 10, 13, 21,
 56, 45, 25, 31, 35, 16,  9, 12,
 44, 24, 15,  8, 23,  7,  6,  5
};

// A 32 bit deBruijn multiplication table for (n * 0x077CB531U) >> 27
const unsigned char debruijn_lsb32[] = {
   0,  1, 28,  2, 29, 14, 24, 3,
  30, 22, 20, 15, 25, 17,  4, 8,
  31, 27, 13, 23, 21, 19, 16, 7,
  26, 12, 18,  6, 11,  5, 10, 9
};

// Frigo's algorithm from http://stackoverflow.com/questions/7365562/de-bruijn-like-sequence-for-2n-1-how-is-it-constructed
const unsigned char debruijn_rank32[] = {
  32, -1,  2, -1,  3, -1, -1, -1,
  -1,  4, -1, 17, 13, -1, -1,  7,
   0, -1, -1,  5, -1, -1, 27, 18,
  29, 14, 24, -1, -1, 20,  8, -1,
  31,  1, -1, -1, -1, 16, 12,  6,
  -1, -1, -1, 26, 28, 23, 19, -1,
  30, -1, 15, 11, -1, 25, 22, -1,
  -1, 10, -1, 21,  9, -1, -1, -1
};

// A 32 bit deBruijn multiplication table for calculating log_2 based on first rounding down to 1 less than a power of 2
const unsigned char debruijn_log32[] = {
   0,  9,  1, 10, 13, 21,  2, 29,
  11, 14, 16, 18, 22, 25,  3, 30,
   8, 12, 20, 28, 15, 17, 24,  7,
  19, 27, 23,  6, 26,  5,  4, 31
};
