/*
 See http://martin.ankerl.com/2007/10/04/optimized-pow-approximation-for-java-and-c-c/

 All of these rely on being on a little endian machine, such as an Intel box.

 These can be _quite_ inaccurate. ~20% in many cases, but being much faster (~7x) may
 permit more loop iterations of tuning algorithms that only need approximate powers.

 --Edward Kmett

 TODO: Incorporate the changes suggested by Nic Schraudolph to Ankerl's inversion:

 "You can get a much better approximation (piecewise rational instead of linear) at the cost of a single floating-point division by using better_exp(x) = exp(x/2)/exp(-x/2), where exp() is my published approximation but you don't need the additive constant anymore, you can use c=0. On machines with hardware division this is very attractive." -- Nic Schraudolph

 TODO: Incorporate the techniques from https://code.google.com/p/fastapprox/ to enable us
to calculate more interesting approximate functions. They might need to be generalized to work on
Double values where appropriate I suppose.
*/

// Ankerl's version of Schraudolph's approximation.
double pow_fast(double a, double b) {
  union {
    double d;
    int x[2];
  } u = { a };
  u.x[1] = (int)(b * (u.x[1] - 1072632447) + 1072632447);
  u.x[0] = 0;
  return u.d;
}

/*
  "I have used the same trick for float, not double, with some slight modification to the constants to suite IEEE754 float format. The first constant for float is 1<<23/log(2) and the second is 127<<23 (for double they are 1<<20/log(2) and 1023<<20)." -- John


 double approximation: round(1<<20/log(2)) = 1512775, 1023<<20 = 1072693248
 float approximation: round(1<<23/log(2)) = 12102203, 127<<23 = 1065353216
*/

float powf_fast(float a, float b) {
  union {
    float d;
    int x;
  } u = { a };
  u.x = (int)(b * (u.x - 1065353216) + 1065353216);
  return u.d;
}

/*
  Now that 64 bit arithmetic is cheap we can (try to) improve on Ankerl's algorithm.

 double long long approximation: round 1<<52/log(2) 6497320848556798,
  mask = 0x3ff0000000000000LL = 4607182418800017408LL


>>> round (2**52 * log (3 / (8 * log 2) + 1/2) / log 2 - 1/2)
261140389990638
>>> 0x3ff0000000000000 - round (2**52 * log (3 / (8 * log 2) + 1/2) / log 2 - 1/2)
4606921278410026770

*/
double pow_fast_smooth(double a, double b) {
  union {
    double d;
    long long x;
  } u = { a };
  u.x = (long long)(b * (u.x - 0x3fef127e83d16f12LL) + 0x3fef127e83d16f12LL);
  return u.d;
}

/* should be much more precise with large b, still ~3.3x faster. */
double pow_fast_precise(double a, double b) {
  int flipped = 0;
  if (b < 0) {
    flipped = 1;
    b = -b;
  }

  /* calculate approximation with fraction of the exponent */
  int e = (int) b;
  union {
    double d;
    int x[2];
  } u = { a };
  u.x[1] = (int)((b - e) * (u.x[1] - 1072632447) + 1072632447);
  u.x[0] = 0;

  double r = 1.0;
  while (e) {
    if (e & 1) {
      r *= a;
    }
    a *= a;
    e >>= 1;
  }

  r *= u.d;
  return flipped ? 1.0/r : r;
}

/* should be much more precise with large b, still ~3.3x faster. */
double pow_fast_precise_smooth(double a, double b) {
  int flipped = 0;
  if (b < 0) {
    flipped = 1;
    b = -b;
  }

  /* calculate approximation with fraction of the exponent */
  int e = (int) b;
  union {
    double d;
    long long x;
  } u = { a };
  u.x = (long long)((b - e) * (u.x - 0x3fef127e83d16f12LL) + 0x3fef127e83d16f12LL);
  // u.x[1] = (int)((b - e) * (u.x[1] - 1072632447) + 1072632447);
  // u.x[0] = 0;

  double r = 1.0;
  while (e) {
    if (e & 1) {
      r *= a;
    }
    a *= a;
    e >>= 1;
  }

  r *= u.d;
  return flipped ? 1.0/r : r;
}


/* should be much more precise with large b */
float powf_fast_precise(float a, float b) {
  int flipped = 0;
  if (b < 0) {
    flipped = 1;
    b = -b;
  }

  /* calculate approximation with fraction of the exponent */
  int e = (int) b;
  union {
    float f;
    int x;
  } u = { a };
  u.x = (int)((b - e) * (u.x - 1065353216) + 1065353216);

  float r = 1.0f;
  while (e) {
    if (e & 1) {
      r *= a;
    }
    a *= a;
    e >>= 1;
  }

  r *= u.f;
  return flipped ? 1.0f/r : r;
}

/* Schraudolph's published algorithm */
double exp_fast(double a) {
  union {
    double d;
    int x[2];
  } u;
  u.x[1] = (int) (1512775 * a + 1072632447);
  u.x[0] = 0;
  return u.d;
}

/* Schraudolph's published algorithm 
 double long long approximation: round 1<<52/log(2) 6497320848556798,
  mask = 0x3ff0000000000000LL = 4607182418800017408LL
 double approximation: round(1<<20/log(2)) = 1512775, 1023<<20 = 1072693248
*/
double exp_fast_smooth(double a) {
  union {
    double d;
    long long x;
  } u;
  u.x = (long long) (6497320848556798 * a + 0x3ff0000000000000LL);
  return u.d;
}

/* Schraudolph's published algorithm with John's constants */
float expf_fast(float a) {
  union {
    float f;
    int x;
  } u;
  u.x = (int) (12102203 * a + 1065353216);
  return u.f;
}

/* Ankerl's inversion of Schraudolph's published algorithm */
double log_fast(double a) {
  union {
    double d;
    int x[2];
  } u = { a };
  return (u.x[1] - 1072632447) / 1512775;
}

/* Ankerl's adaptation of Schraudolph's published algorithm with John's constants */
float logf_fast(float a) {
  union {
    float f;
    int x;
  } u = { a };
  return (u.x - 1065353216) / 12102203;
}
