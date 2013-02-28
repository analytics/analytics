/*
 * See http://martin.ankerl.com/2007/10/04/optimized-pow-approximation-for-java-and-c-c/
 *
 * All of these rely on being on a little endian machine, such as an Intel box.
 *
 * These can be _quite_ inaccurate. ~20% in many cases, but being much faster (~7x) may
 * permit more loop iterations of tuning algorithms that only need approximate powers.
 *
 * This version of Ankerl's algorithm has been extended to provide optionally conservative (lower) bounds
 * and also to generate a full linear interpolation across the entire significand rather than 'stair-step'
 * at the expense of performing a 64 bit operation rather than a 32 bit one. This is cheap these days.
 *
 * --Edward Kmett
 *
 * TODO: Incorporate the changes suggested by Nic Schraudolph to Ankerl's inversion:
 *
 * "You can get a much better approximation (piecewise rational instead of linear) at
 * the cost of a single floating-point division by using better_exp(x) = exp(x/2)/exp(-x/2),
 * where exp() is my published approximation but you don't need the additive constant anymore,
 * you can use c=0. On machines with hardware division this is very attractive." -- Nic Schraudolph
 *
 * TODO: Incorporate the techniques from https://code.google.com/p/fastapprox/ to enable us
 * to calculate more interesting approximate functions. They might need to be generalized to work on
 * Double values where appropriate I suppose.
 *
 * Magic numbers:
 * float /int      : round(1<<23/log(2)) = 12102203,          127<<23 = 1065353216
 * double/int      : round(1<<20/log(2)) = 1512775,          1023<<20 = 1072693248
 * double/long long: round(1<<52/log(2)) = 6497320848556798, 1023<<52 = 4607182418800017408
 *
 * Fudge factors such that exp y <= exp_fast y
 * >>> ceiling (2^23 * (1 - (log (log 2) + 1)/log 2))
 * 722019
 * >>> ceiling (2^20 * (1 - (log (log 2) + 1)/log 2))
 * 90253
 * >>> ceiling (2^52 * (1 - (log (log 2) + 1)/log 2))
 * 387630818974388
 */

// Ankerl's version of Schraudolph's approximation.
double pow_fast(double a, double b) {
  union { double d; int x[2]; } u = { a };
  u.x[1] = (int)(b * (u.x[1] - 1072632447) + 1072632447);
  u.x[0] = 0;
  return u.d;
}

/*
  "I have used the same trick for float, not double, with some slight modification to the constants to suite IEEE754 float format. The first constant for float is 1<<23/log(2) and the second is 127<<23 (for double they are 1<<20/log(2) and 1023<<20)." -- John

*/

float powf_fast(float a, float b) {
  union { float d; int x; } u = { a };
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

double pow_fast_smooth_upper_bound(double a, double b) {
  union { double d; long long x; } u = { a };
  u.x = (long long)(b * (u.x - 4606794787981043020LL) + 4607182418800017409LL);
  return u.d;
}

double pow_fast_smooth(double a, double b) {
  union { double d; long long x; } u = { a };
  u.x = (long long)(b * (u.x - 4606921278410026770LL) + 4606921278410026770LL);
  return u.d;
}

double pow_fast_smooth_lower_bound(double a, double b) {
  union { double d; long long x; } u = { a };
  u.x = (long long)(b * (u.x - 4607182418800017409LL) + 4606794787981043020LL);
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
  union { double d; int x[2]; } u = { a };
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
  union { double d; long long x; } u = { a };
  u.x = (long long)((b - e) * (u.x - 0x3fef127e83d16f12LL) + 0x3fef127e83d16f12LL);

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
  union { float f; int x; } u = { a };
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

/* Schraudolph's published algorithm extended into the least significant bits to avoid the stair step.
 double long long approximation: round 1<<52/log(2) 6497320848556798,
  mask = 0x3ff0000000000000LL = 4607182418800017408LL
 double approximation: round(1<<20/log(2)) = 1512775, 1023<<20 = 1072693248
*/

/* 4607182418800017408 - 387630818974388 = 4606794787981043020

Exponent mask adapted to full 64 bit precision:
>>> 1023 * 2^52
4607182418800017408

The fudge factor for conservative lower bound adapted to full 64 bit precision:
>>> round (2^52 * (1 - (log (log 2) + 1)/log 2))
387630818974388

As a lower bound this is suitable for use when generating Mass and Precision estimates.
*/
double exp_fast_smooth_lower_bound(double a) {
  union { double d; long long x; } u;
  u.x = (long long)(6497320848556798LL * a + 4606794787981043020);
  return u.d;
}

/* 4607182418800017408 + 1 */
double exp_fast_smooth_upper_bound(double a) {
  union { double d; long long x; } u;
  u.x = (long long)(6497320848556798LL * a + 4607182418800017409);
  return u.d;
}

double exp_fast_smooth(double a) {
  union { double d; long long x; } u;
  u.x = (long long)(6497320848556798LL * a + 0x3fef127e83d16f12LL);
  return u.d;
}

/* Schraudolph's published algorithm */
double exp_fast(double a) {
  union { double d; int x[2]; } u;
  u.x[1] = (int) (1512775 * a + 1072632447);
  u.x[0] = 0;
  return u.d;
}


/* Schraudolph's published algorithm with John's constants */
float expf_fast(float a) {
  union { float f; int x; } u;
  u.x = (int) (12102203 * a + 1065353216);
  return u.f;
}


/* Ankerl's inversion of Schraudolph's published algorithm, converted to explicit multiplication */
double log_fast(double a) {
  union { double d; int x[2]; } u = { a };
  return (u.x[1] - 1072632447) * 6.610368362777016e-7; // 1 / 1512775.0;
}


double log_fast_smooth_upper_bound(double a) {
  union { double d; long long x; } u = { a };
  return (u.x - 4606794787981043020) * 1.539095918623324e-16; // 1 / 6497320848556798.0;
}
/* Ankerl's inversion of Schraudolph's published algorithm with my constants */
double log_fast_smooth(double a) {
  union { double d; long long x; } u = { a };
  return (u.x - 4606921278410026770) * 1.539095918623324e-16; // 1 / 6497320848556798.0;
}
double log_fast_smooth_lower_bound(double a) {
  union { double d; long long x; } u = { a };
  return (u.x - 4607182418800017409) * 1.539095918623324e-16; // 1 / 6497320848556798.0;
}

/* Ankerl's adaptation of Schraudolph's published algorithm with John's constants */
float logf_fast(float a) {
  union {
    float f;
    int x;
  } u = { a };
  return (u.x - 1065353216) * 8.262958405176314e-8f; // / 12102203.0;
}

/*  722019 */
