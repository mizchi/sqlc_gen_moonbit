#ifdef __cplusplus
extern "C" {
#endif

#include "moonbit.h"

#ifdef _MSC_VER
#define _Noreturn __declspec(noreturn)
#endif

#if defined(__clang__)
#pragma clang diagnostic ignored "-Wshift-op-parentheses"
#pragma clang diagnostic ignored "-Wtautological-compare"
#endif

MOONBIT_EXPORT _Noreturn void moonbit_panic(void);
MOONBIT_EXPORT void *moonbit_malloc_array(enum moonbit_block_kind kind,
                                          int elem_size_shift, int32_t len);
MOONBIT_EXPORT int moonbit_val_array_equal(const void *lhs, const void *rhs);
MOONBIT_EXPORT moonbit_string_t moonbit_add_string(moonbit_string_t s1,
                                                   moonbit_string_t s2);
MOONBIT_EXPORT void moonbit_unsafe_bytes_blit(moonbit_bytes_t dst,
                                              int32_t dst_start,
                                              moonbit_bytes_t src,
                                              int32_t src_offset, int32_t len);
MOONBIT_EXPORT moonbit_string_t moonbit_unsafe_bytes_sub_string(
    moonbit_bytes_t bytes, int32_t start, int32_t len);
MOONBIT_EXPORT void moonbit_println(moonbit_string_t str);
MOONBIT_EXPORT moonbit_bytes_t *moonbit_get_cli_args(void);
MOONBIT_EXPORT void moonbit_runtime_init(int argc, char **argv);
MOONBIT_EXPORT void moonbit_drop_object(void *);

#define Moonbit_make_regular_object_header(ptr_field_offset, ptr_field_count,  \
                                           tag)                                \
  (((uint32_t)moonbit_BLOCK_KIND_REGULAR << 30) |                              \
   (((uint32_t)(ptr_field_offset) & (((uint32_t)1 << 11) - 1)) << 19) |        \
   (((uint32_t)(ptr_field_count) & (((uint32_t)1 << 11) - 1)) << 8) |          \
   ((tag) & 0xFF))

// header manipulation macros
#define Moonbit_object_ptr_field_offset(obj)                                   \
  ((Moonbit_object_header(obj)->meta >> 19) & (((uint32_t)1 << 11) - 1))

#define Moonbit_object_ptr_field_count(obj)                                    \
  ((Moonbit_object_header(obj)->meta >> 8) & (((uint32_t)1 << 11) - 1))

#if !defined(_WIN64) && !defined(_WIN32)
void *malloc(size_t size);
void free(void *ptr);
#define libc_malloc malloc
#define libc_free free
#endif

// several important runtime functions are inlined
static void *moonbit_malloc_inlined(size_t size) {
  struct moonbit_object *ptr = (struct moonbit_object *)libc_malloc(
      sizeof(struct moonbit_object) + size);
  ptr->rc = 1;
  return ptr + 1;
}

#define moonbit_malloc(obj) moonbit_malloc_inlined(obj)
#define moonbit_free(obj) libc_free(Moonbit_object_header(obj))

static void moonbit_incref_inlined(void *ptr) {
  struct moonbit_object *header = Moonbit_object_header(ptr);
  int32_t const count = header->rc;
  if (count > 0) {
    header->rc = count + 1;
  }
}

#define moonbit_incref moonbit_incref_inlined

static void moonbit_decref_inlined(void *ptr) {
  struct moonbit_object *header = Moonbit_object_header(ptr);
  int32_t const count = header->rc;
  if (count > 1) {
    header->rc = count - 1;
  } else if (count == 1) {
    moonbit_drop_object(ptr);
  }
}

#define moonbit_decref moonbit_decref_inlined

#define moonbit_unsafe_make_string moonbit_make_string

// detect whether compiler builtins exist for advanced bitwise operations
#ifdef __has_builtin

#if __has_builtin(__builtin_clz)
#define HAS_BUILTIN_CLZ
#endif

#if __has_builtin(__builtin_ctz)
#define HAS_BUILTIN_CTZ
#endif

#if __has_builtin(__builtin_popcount)
#define HAS_BUILTIN_POPCNT
#endif

#if __has_builtin(__builtin_sqrt)
#define HAS_BUILTIN_SQRT
#endif

#if __has_builtin(__builtin_sqrtf)
#define HAS_BUILTIN_SQRTF
#endif

#if __has_builtin(__builtin_fabs)
#define HAS_BUILTIN_FABS
#endif

#if __has_builtin(__builtin_fabsf)
#define HAS_BUILTIN_FABSF
#endif

#endif

// if there is no builtin operators, use software implementation
#ifdef HAS_BUILTIN_CLZ
static inline int32_t moonbit_clz32(int32_t x) {
  return x == 0 ? 32 : __builtin_clz(x);
}

static inline int32_t moonbit_clz64(int64_t x) {
  return x == 0 ? 64 : __builtin_clzll(x);
}

#undef HAS_BUILTIN_CLZ
#else
// table for [clz] value of 4bit integer.
static const uint8_t moonbit_clz4[] = {4, 3, 2, 2, 1, 1, 1, 1,
                                       0, 0, 0, 0, 0, 0, 0, 0};

int32_t moonbit_clz32(uint32_t x) {
  /* The ideas is to:

     1. narrow down the 4bit block where the most signficant "1" bit lies,
        using binary search
     2. find the number of leading zeros in that 4bit block via table lookup

     Different time/space tradeoff can be made here by enlarging the table
     and do less binary search.
     One benefit of the 4bit lookup table is that it can fit into a single cache
     line.
  */
  int32_t result = 0;
  if (x > 0xffff) {
    x >>= 16;
  } else {
    result += 16;
  }
  if (x > 0xff) {
    x >>= 8;
  } else {
    result += 8;
  }
  if (x > 0xf) {
    x >>= 4;
  } else {
    result += 4;
  }
  return result + moonbit_clz4[x];
}

int32_t moonbit_clz64(uint64_t x) {
  int32_t result = 0;
  if (x > 0xffffffff) {
    x >>= 32;
  } else {
    result += 32;
  }
  return result + moonbit_clz32((uint32_t)x);
}
#endif

#ifdef HAS_BUILTIN_CTZ
static inline int32_t moonbit_ctz32(int32_t x) {
  return x == 0 ? 32 : __builtin_ctz(x);
}

static inline int32_t moonbit_ctz64(int64_t x) {
  return x == 0 ? 64 : __builtin_ctzll(x);
}

#undef HAS_BUILTIN_CTZ
#else
int32_t moonbit_ctz32(int32_t x) {
  /* The algorithm comes from:

       Leiserson, Charles E. et al. “Using de Bruijn Sequences to Index a 1 in a
     Computer Word.” (1998).

     The ideas is:

     1. leave only the least significant "1" bit in the input,
        set all other bits to "0". This is achieved via [x & -x]
     2. now we have [x * n == n << ctz(x)], if [n] is a de bruijn sequence
        (every 5bit pattern occurn exactly once when you cycle through the bit
     string), we can find [ctz(x)] from the most significant 5 bits of [x * n]
 */
  static const uint32_t de_bruijn_32 = 0x077CB531;
  static const uint8_t index32[] = {0,  1,  28, 2,  29, 14, 24, 3,  30, 22, 20,
                                    15, 25, 17, 4,  8,  31, 27, 13, 23, 21, 19,
                                    16, 7,  26, 12, 18, 6,  11, 5,  10, 9};
  return (x == 0) * 32 + index32[(de_bruijn_32 * (x & -x)) >> 27];
}

int32_t moonbit_ctz64(int64_t x) {
  static const uint64_t de_bruijn_64 = 0x0218A392CD3D5DBF;
  static const uint8_t index64[] = {
      0,  1,  2,  7,  3,  13, 8,  19, 4,  25, 14, 28, 9,  34, 20, 40,
      5,  17, 26, 38, 15, 46, 29, 48, 10, 31, 35, 54, 21, 50, 41, 57,
      63, 6,  12, 18, 24, 27, 33, 39, 16, 37, 45, 47, 30, 53, 49, 56,
      62, 11, 23, 32, 36, 44, 52, 55, 61, 22, 43, 51, 60, 42, 59, 58};
  return (x == 0) * 64 + index64[(de_bruijn_64 * (x & -x)) >> 58];
}
#endif

#ifdef HAS_BUILTIN_POPCNT

#define moonbit_popcnt32 __builtin_popcount
#define moonbit_popcnt64 __builtin_popcountll
#undef HAS_BUILTIN_POPCNT

#else
int32_t moonbit_popcnt32(uint32_t x) {
  /* The classic SIMD Within A Register algorithm.
     ref: [https://nimrod.blog/posts/algorithms-behind-popcount/]
 */
  x = x - ((x >> 1) & 0x55555555);
  x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
  x = (x + (x >> 4)) & 0x0F0F0F0F;
  return (x * 0x01010101) >> 24;
}

int32_t moonbit_popcnt64(uint64_t x) {
  x = x - ((x >> 1) & 0x5555555555555555);
  x = (x & 0x3333333333333333) + ((x >> 2) & 0x3333333333333333);
  x = (x + (x >> 4)) & 0x0F0F0F0F0F0F0F0F;
  return (x * 0x0101010101010101) >> 56;
}
#endif

/* The following sqrt implementation comes from
   [musl](https://git.musl-libc.org/cgit/musl),
   with some helpers inlined to make it zero dependency.
 */
#ifdef MOONBIT_NATIVE_NO_SYS_HEADER
const uint16_t __rsqrt_tab[128] = {
    0xb451, 0xb2f0, 0xb196, 0xb044, 0xaef9, 0xadb6, 0xac79, 0xab43, 0xaa14,
    0xa8eb, 0xa7c8, 0xa6aa, 0xa592, 0xa480, 0xa373, 0xa26b, 0xa168, 0xa06a,
    0x9f70, 0x9e7b, 0x9d8a, 0x9c9d, 0x9bb5, 0x9ad1, 0x99f0, 0x9913, 0x983a,
    0x9765, 0x9693, 0x95c4, 0x94f8, 0x9430, 0x936b, 0x92a9, 0x91ea, 0x912e,
    0x9075, 0x8fbe, 0x8f0a, 0x8e59, 0x8daa, 0x8cfe, 0x8c54, 0x8bac, 0x8b07,
    0x8a64, 0x89c4, 0x8925, 0x8889, 0x87ee, 0x8756, 0x86c0, 0x862b, 0x8599,
    0x8508, 0x8479, 0x83ec, 0x8361, 0x82d8, 0x8250, 0x81c9, 0x8145, 0x80c2,
    0x8040, 0xff02, 0xfd0e, 0xfb25, 0xf947, 0xf773, 0xf5aa, 0xf3ea, 0xf234,
    0xf087, 0xeee3, 0xed47, 0xebb3, 0xea27, 0xe8a3, 0xe727, 0xe5b2, 0xe443,
    0xe2dc, 0xe17a, 0xe020, 0xdecb, 0xdd7d, 0xdc34, 0xdaf1, 0xd9b3, 0xd87b,
    0xd748, 0xd61a, 0xd4f1, 0xd3cd, 0xd2ad, 0xd192, 0xd07b, 0xcf69, 0xce5b,
    0xcd51, 0xcc4a, 0xcb48, 0xca4a, 0xc94f, 0xc858, 0xc764, 0xc674, 0xc587,
    0xc49d, 0xc3b7, 0xc2d4, 0xc1f4, 0xc116, 0xc03c, 0xbf65, 0xbe90, 0xbdbe,
    0xbcef, 0xbc23, 0xbb59, 0xba91, 0xb9cc, 0xb90a, 0xb84a, 0xb78c, 0xb6d0,
    0xb617, 0xb560,
};

/* returns a*b*2^-32 - e, with error 0 <= e < 1.  */
static inline uint32_t mul32(uint32_t a, uint32_t b) {
  return (uint64_t)a * b >> 32;
}
#endif

#ifdef MOONBIT_NATIVE_NO_SYS_HEADER
float sqrtf(float x) {
  uint32_t ix, m, m1, m0, even, ey;

  ix = *(uint32_t *)&x;
  if (ix - 0x00800000 >= 0x7f800000 - 0x00800000) {
    /* x < 0x1p-126 or inf or nan.  */
    if (ix * 2 == 0)
      return x;
    if (ix == 0x7f800000)
      return x;
    if (ix > 0x7f800000)
      return (x - x) / (x - x);
    /* x is subnormal, normalize it.  */
    x *= 0x1p23f;
    ix = *(uint32_t *)&x;
    ix -= 23 << 23;
  }

  /* x = 4^e m; with int e and m in [1, 4).  */
  even = ix & 0x00800000;
  m1 = (ix << 8) | 0x80000000;
  m0 = (ix << 7) & 0x7fffffff;
  m = even ? m0 : m1;

  /* 2^e is the exponent part of the return value.  */
  ey = ix >> 1;
  ey += 0x3f800000 >> 1;
  ey &= 0x7f800000;

  /* compute r ~ 1/sqrt(m), s ~ sqrt(m) with 2 goldschmidt iterations.  */
  static const uint32_t three = 0xc0000000;
  uint32_t r, s, d, u, i;
  i = (ix >> 17) % 128;
  r = (uint32_t)__rsqrt_tab[i] << 16;
  /* |r*sqrt(m) - 1| < 0x1p-8 */
  s = mul32(m, r);
  /* |s/sqrt(m) - 1| < 0x1p-8 */
  d = mul32(s, r);
  u = three - d;
  r = mul32(r, u) << 1;
  /* |r*sqrt(m) - 1| < 0x1.7bp-16 */
  s = mul32(s, u) << 1;
  /* |s/sqrt(m) - 1| < 0x1.7bp-16 */
  d = mul32(s, r);
  u = three - d;
  s = mul32(s, u);
  /* -0x1.03p-28 < s/sqrt(m) - 1 < 0x1.fp-31 */
  s = (s - 1) >> 6;
  /* s < sqrt(m) < s + 0x1.08p-23 */

  /* compute nearest rounded result.  */
  uint32_t d0, d1, d2;
  float y, t;
  d0 = (m << 16) - s * s;
  d1 = s - d0;
  d2 = d1 + s + 1;
  s += d1 >> 31;
  s &= 0x007fffff;
  s |= ey;
  y = *(float *)&s;
  /* handle rounding and inexact exception. */
  uint32_t tiny = d2 == 0 ? 0 : 0x01000000;
  tiny |= (d1 ^ d2) & 0x80000000;
  t = *(float *)&tiny;
  y = y + t;
  return y;
}
#endif

#ifdef MOONBIT_NATIVE_NO_SYS_HEADER
/* returns a*b*2^-64 - e, with error 0 <= e < 3.  */
static inline uint64_t mul64(uint64_t a, uint64_t b) {
  uint64_t ahi = a >> 32;
  uint64_t alo = a & 0xffffffff;
  uint64_t bhi = b >> 32;
  uint64_t blo = b & 0xffffffff;
  return ahi * bhi + (ahi * blo >> 32) + (alo * bhi >> 32);
}

double sqrt(double x) {
  uint64_t ix, top, m;

  /* special case handling.  */
  ix = *(uint64_t *)&x;
  top = ix >> 52;
  if (top - 0x001 >= 0x7ff - 0x001) {
    /* x < 0x1p-1022 or inf or nan.  */
    if (ix * 2 == 0)
      return x;
    if (ix == 0x7ff0000000000000)
      return x;
    if (ix > 0x7ff0000000000000)
      return (x - x) / (x - x);
    /* x is subnormal, normalize it.  */
    x *= 0x1p52;
    ix = *(uint64_t *)&x;
    top = ix >> 52;
    top -= 52;
  }

  /* argument reduction:
     x = 4^e m; with integer e, and m in [1, 4)
     m: fixed point representation [2.62]
     2^e is the exponent part of the result.  */
  int even = top & 1;
  m = (ix << 11) | 0x8000000000000000;
  if (even)
    m >>= 1;
  top = (top + 0x3ff) >> 1;

  /* approximate r ~ 1/sqrt(m) and s ~ sqrt(m) when m in [1,4)

     initial estimate:
     7bit table lookup (1bit exponent and 6bit significand).

     iterative approximation:
     using 2 goldschmidt iterations with 32bit int arithmetics
     and a final iteration with 64bit int arithmetics.

     details:

     the relative error (e = r0 sqrt(m)-1) of a linear estimate
     (r0 = a m + b) is |e| < 0.085955 ~ 0x1.6p-4 at best,
     a table lookup is faster and needs one less iteration
     6 bit lookup table (128b) gives |e| < 0x1.f9p-8
     7 bit lookup table (256b) gives |e| < 0x1.fdp-9
     for single and double prec 6bit is enough but for quad
     prec 7bit is needed (or modified iterations). to avoid
     one more iteration >=13bit table would be needed (16k).

     a newton-raphson iteration for r is
       w = r*r
       u = 3 - m*w
       r = r*u/2
     can use a goldschmidt iteration for s at the end or
       s = m*r

     first goldschmidt iteration is
       s = m*r
       u = 3 - s*r
       r = r*u/2
       s = s*u/2
     next goldschmidt iteration is
       u = 3 - s*r
       r = r*u/2
       s = s*u/2
     and at the end r is not computed only s.

     they use the same amount of operations and converge at the
     same quadratic rate, i.e. if
       r1 sqrt(m) - 1 = e, then
       r2 sqrt(m) - 1 = -3/2 e^2 - 1/2 e^3
     the advantage of goldschmidt is that the mul for s and r
     are independent (computed in parallel), however it is not
     "self synchronizing": it only uses the input m in the
     first iteration so rounding errors accumulate. at the end
     or when switching to larger precision arithmetics rounding
     errors dominate so the first iteration should be used.

     the fixed point representations are
       m: 2.30 r: 0.32, s: 2.30, d: 2.30, u: 2.30, three: 2.30
     and after switching to 64 bit
       m: 2.62 r: 0.64, s: 2.62, d: 2.62, u: 2.62, three: 2.62  */

  static const uint64_t three = 0xc0000000;
  uint64_t r, s, d, u, i;

  i = (ix >> 46) % 128;
  r = (uint32_t)__rsqrt_tab[i] << 16;
  /* |r sqrt(m) - 1| < 0x1.fdp-9 */
  s = mul32(m >> 32, r);
  /* |s/sqrt(m) - 1| < 0x1.fdp-9 */
  d = mul32(s, r);
  u = three - d;
  r = mul32(r, u) << 1;
  /* |r sqrt(m) - 1| < 0x1.7bp-16 */
  s = mul32(s, u) << 1;
  /* |s/sqrt(m) - 1| < 0x1.7bp-16 */
  d = mul32(s, r);
  u = three - d;
  r = mul32(r, u) << 1;
  /* |r sqrt(m) - 1| < 0x1.3704p-29 (measured worst-case) */
  r = r << 32;
  s = mul64(m, r);
  d = mul64(s, r);
  u = (three << 32) - d;
  s = mul64(s, u); /* repr: 3.61 */
  /* -0x1p-57 < s - sqrt(m) < 0x1.8001p-61 */
  s = (s - 2) >> 9; /* repr: 12.52 */
  /* -0x1.09p-52 < s - sqrt(m) < -0x1.fffcp-63 */

  /* s < sqrt(m) < s + 0x1.09p-52,
     compute nearest rounded result:
     the nearest result to 52 bits is either s or s+0x1p-52,
     we can decide by comparing (2^52 s + 0.5)^2 to 2^104 m.  */
  uint64_t d0, d1, d2;
  double y, t;
  d0 = (m << 42) - s * s;
  d1 = s - d0;
  d2 = d1 + s + 1;
  s += d1 >> 63;
  s &= 0x000fffffffffffff;
  s |= top << 52;
  y = *(double *)&s;
  return y;
}
#endif

#ifdef MOONBIT_NATIVE_NO_SYS_HEADER
double fabs(double x) {
  union {
    double f;
    uint64_t i;
  } u = {x};
  u.i &= 0x7fffffffffffffffULL;
  return u.f;
}
#endif

#ifdef MOONBIT_NATIVE_NO_SYS_HEADER
float fabsf(float x) {
  union {
    float f;
    uint32_t i;
  } u = {x};
  u.i &= 0x7fffffff;
  return u.f;
}
#endif

#ifdef _MSC_VER
/* MSVC treats syntactic division by zero as fatal error,
   even for float point numbers,
   so we have to use a constant variable to work around this */
static const int MOONBIT_ZERO = 0;
#else
#define MOONBIT_ZERO 0
#endif

#ifdef __cplusplus
}
#endif
struct $$moonbitlang$core$builtin$Array$3c$String$3e$;

struct $Ref$3c$Int$3e$;

struct $$3c$Bytes$3e$$3d$$3e$String;

struct $StringView;

struct $Result$3c$Byte$2a$$moonbitlang$core$builtin$NoError$3e$$Err;

struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$;

struct $Result$3c$FixedArray$3c$String$3e$$2a$$moonbitlang$core$builtin$NoError$3e$$Err;

struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$;

struct $Result$3c$FixedArray$3c$String$3e$$2a$$moonbitlang$core$builtin$NoError$3e$$Ok;

struct $Error$moonbitlang$x$fs$IOError$IOError;

struct $$moonbitlang$core$builtin$Logger;

struct $$moonbitlang$core$builtin$Array$3c$QueryDef$3e$;

struct $Result$3c$Bytes$2a$$moonbitlang$core$builtin$NoError$3e$$Err;

struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$;

struct $$moonbitlang$core$builtin$Array$3c$Byte$3e$;

struct $String$$iter$$2a$p$fn$3$2d$cap;

struct $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger;

struct $$3c$String$2a$String$3e$;

struct $Bytes$$from_array$fn$5$2d$cap;

struct $$3c$Int$2a$Char$3e$;

struct $Result$3c$$moonbitlang$core$builtin$Array$3c$Char$3e$$2a$$moonbitlang$core$builtin$NoError$3e$$Err;

struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$Char$3e$;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Err;

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$;

struct $$3c$$moonbitlang$core$builtin$Array$3c$Char$3e$$2a$Char$3e$$3d$$3e$$moonbitlang$core$builtin$Array$3c$Char$3e$;

struct $Result$3c$$moonbitlang$core$builtin$Array$3c$Char$3e$$2a$$moonbitlang$core$builtin$NoError$3e$$Ok;

struct $Result$3c$String$2a$$moonbitlang$core$builtin$NoError$3e$$Err;

struct $$moonbitlang$core$builtin$Array$3c$Char$3e$;

struct $Result$3c$String$2a$$moonbitlang$core$builtin$NoError$3e$$Ok;

struct $StringView$$iter2$fn$2$2d$cap;

struct $QueryDef;

struct $$3c$Char$3e$$3d$$3e$Bool;

struct $Result$3c$String$2a$$moonbitlang$x$fs$IOError$3e$$Err;

struct $Result$3c$Bytes$2a$$moonbitlang$x$fs$IOError$3e$$Err;

struct $StringView$$iter$$2a$p$fn$1$2d$cap;

struct $Result$3c$Bytes$2a$$moonbitlang$x$fs$IOError$3e$$Ok;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Ok;

struct $$moonbitlang$core$builtin$SourceLocRepr;

struct $Result$3c$Bytes$2a$$moonbitlang$core$builtin$NoError$3e$$Ok;

struct $Result$3c$String$2a$$moonbitlang$x$fs$IOError$3e$$Ok;

struct $Option$3c$StringView$3e$$Some;

struct $Result$3c$Byte$2a$$moonbitlang$core$builtin$NoError$3e$$Ok;

struct $$moonbitlang$core$builtin$Logger$static_method_table;

struct $$moonbitlang$core$builtin$StringBuilder;

struct $$3c$String$2a$$moonbitlang$core$builtin$Logger$3e$;

struct $$moonbitlang$core$builtin$Array$3c$String$3e$ {
  int32_t $1;
  moonbit_string_t* $0;
  
};

struct $Ref$3c$Int$3e$ {
  int32_t $0;
  
};

struct $$3c$Bytes$3e$$3d$$3e$String {
  moonbit_string_t(* code)(
    struct $$3c$Bytes$3e$$3d$$3e$String*,
    moonbit_bytes_t
  );
  
};

struct $StringView {
  int32_t $1;
  int32_t $2;
  moonbit_string_t $0;
  
};

struct $Result$3c$Byte$2a$$moonbitlang$core$builtin$NoError$3e$$Err {
  int32_t $0;
  
};

struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ {
  int32_t $1;
  int32_t $2;
  moonbit_bytes_t $0;
  
};

struct $Result$3c$FixedArray$3c$String$3e$$2a$$moonbitlang$core$builtin$NoError$3e$$Err {
  int32_t $0;
  
};

struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$ {
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t $0_0;
  void* $1;
  
};

struct $Result$3c$FixedArray$3c$String$3e$$2a$$moonbitlang$core$builtin$NoError$3e$$Ok {
  moonbit_string_t* $0;
  
};

struct $Error$moonbitlang$x$fs$IOError$IOError {
  moonbit_string_t $0;
  
};

struct $$moonbitlang$core$builtin$Logger {
  struct $$moonbitlang$core$builtin$Logger$static_method_table* $0;
  void* $1;
  
};

struct $$moonbitlang$core$builtin$Array$3c$QueryDef$3e$ {
  int32_t $1;
  struct $QueryDef** $0;
  
};

struct $Result$3c$Bytes$2a$$moonbitlang$core$builtin$NoError$3e$$Err {
  int32_t $0;
  
};

struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$ {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$*);
  
};

struct $$moonbitlang$core$builtin$Array$3c$Byte$3e$ {
  int32_t $1;
  moonbit_bytes_t $0;
  
};

struct $String$$iter$$2a$p$fn$3$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$*);
  int32_t $2;
  struct $Ref$3c$Int$3e$* $0;
  moonbit_string_t $1;
  
};

struct $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger {
  struct $$moonbitlang$core$builtin$Logger$static_method_table* $0;
  void* $1;
  
};

struct $$3c$String$2a$String$3e$ {
  moonbit_string_t $0;
  moonbit_string_t $1;
  
};

struct $Bytes$$from_array$fn$5$2d$cap {
  int32_t(* code)(struct $$3c$Char$3e$$3d$$3e$Bool*, int32_t);
  int32_t $0_1;
  int32_t $0_2;
  moonbit_bytes_t $0_0;
  
};

struct $$3c$Int$2a$Char$3e$ {
  int32_t $0;
  int32_t $1;
  
};

struct $Result$3c$$moonbitlang$core$builtin$Array$3c$Char$3e$$2a$$moonbitlang$core$builtin$NoError$3e$$Err {
  int32_t $0;
  
};

struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$ {
  struct $$3c$Int$2a$Char$3e$*(* code)(
    struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$*
  );
  
};

struct $$moonbitlang$core$builtin$ArrayView$3c$Char$3e$ {
  int32_t $1;
  int32_t $2;
  int32_t* $0;
  
};

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Err {
  void* $0;
  
};

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$ {
  int32_t $1;
  struct $$3c$String$2a$String$3e$** $0;
  
};

struct $$3c$$moonbitlang$core$builtin$Array$3c$Char$3e$$2a$Char$3e$$3d$$3e$$moonbitlang$core$builtin$Array$3c$Char$3e$ {
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$*(* code)(
    struct $$3c$$moonbitlang$core$builtin$Array$3c$Char$3e$$2a$Char$3e$$3d$$3e$$moonbitlang$core$builtin$Array$3c$Char$3e$*,
    struct $$moonbitlang$core$builtin$Array$3c$Char$3e$*,
    int32_t
  );
  
};

struct $Result$3c$$moonbitlang$core$builtin$Array$3c$Char$3e$$2a$$moonbitlang$core$builtin$NoError$3e$$Ok {
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* $0;
  
};

struct $Result$3c$String$2a$$moonbitlang$core$builtin$NoError$3e$$Err {
  int32_t $0;
  
};

struct $$moonbitlang$core$builtin$Array$3c$Char$3e$ {
  int32_t $1;
  int32_t* $0;
  
};

struct $Result$3c$String$2a$$moonbitlang$core$builtin$NoError$3e$$Ok {
  moonbit_string_t $0;
  
};

struct $StringView$$iter2$fn$2$2d$cap {
  struct $$3c$Int$2a$Char$3e$*(* code)(
    struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$*
  );
  int32_t $2;
  int32_t $3_1;
  int32_t $3_2;
  struct $Ref$3c$Int$3e$* $0;
  struct $Ref$3c$Int$3e$* $1;
  moonbit_string_t $3_0;
  
};

struct $QueryDef {
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* $2;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* $3;
  moonbit_string_t $4;
  
};

struct $$3c$Char$3e$$3d$$3e$Bool {
  int32_t(* code)(struct $$3c$Char$3e$$3d$$3e$Bool*, int32_t);
  
};

struct $Result$3c$String$2a$$moonbitlang$x$fs$IOError$3e$$Err {
  void* $0;
  
};

struct $Result$3c$Bytes$2a$$moonbitlang$x$fs$IOError$3e$$Err {
  void* $0;
  
};

struct $StringView$$iter$$2a$p$fn$1$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$*);
  int32_t $1;
  int32_t $2_1;
  int32_t $2_2;
  struct $Ref$3c$Int$3e$* $0;
  moonbit_string_t $2_0;
  
};

struct $Result$3c$Bytes$2a$$moonbitlang$x$fs$IOError$3e$$Ok {
  moonbit_bytes_t $0;
  
};

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Ok {
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t $0_0;
  
};

struct $$moonbitlang$core$builtin$SourceLocRepr {
  int32_t $0_1;
  int32_t $0_2;
  int32_t $1_1;
  int32_t $1_2;
  int32_t $2_1;
  int32_t $2_2;
  int32_t $3_1;
  int32_t $3_2;
  int32_t $4_1;
  int32_t $4_2;
  int32_t $5_1;
  int32_t $5_2;
  moonbit_string_t $0_0;
  moonbit_string_t $1_0;
  moonbit_string_t $2_0;
  moonbit_string_t $3_0;
  moonbit_string_t $4_0;
  moonbit_string_t $5_0;
  
};

struct $Result$3c$Bytes$2a$$moonbitlang$core$builtin$NoError$3e$$Ok {
  moonbit_bytes_t $0;
  
};

struct $Result$3c$String$2a$$moonbitlang$x$fs$IOError$3e$$Ok {
  moonbit_string_t $0;
  
};

struct $Option$3c$StringView$3e$$Some {
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t $0_0;
  
};

struct $Result$3c$Byte$2a$$moonbitlang$core$builtin$NoError$3e$$Ok {
  int32_t $0;
  
};

struct $$moonbitlang$core$builtin$Logger$static_method_table {
  int32_t(* $method_0)(void*, moonbit_string_t);
  int32_t(* $method_1)(void*, moonbit_string_t, int32_t, int32_t);
  int32_t(* $method_2)(void*, struct $StringView);
  int32_t(* $method_3)(void*, int32_t);
  
};

struct $$moonbitlang$core$builtin$StringBuilder {
  int32_t $1;
  moonbit_bytes_t $0;
  
};

struct $$3c$String$2a$$moonbitlang$core$builtin$Logger$3e$ {
  moonbit_string_t $0;
  struct $$moonbitlang$core$builtin$Logger$static_method_table* $1_0;
  void* $1_1;
  
};

struct moonbit_result_1 {
  int tag;
  union { moonbit_bytes_t ok; void* err;  } data;
  
};

struct moonbit_result_2 {
  int tag;
  union { struct $StringView ok; void* err;  } data;
  
};

struct moonbit_result_0 {
  int tag;
  union { moonbit_string_t ok; void* err;  } data;
  
};

struct $$moonbitlang$core$builtin$Array$3c$QueryDef$3e$* $sqlc_gen_moonbit$tools$codegen$parse_queries(
  moonbit_string_t content$870
);

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$substr(
  moonbit_string_t s$864,
  int32_t start$867
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $sqlc_gen_moonbit$tools$codegen$split_whitespace(
  moonbit_string_t s$857
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $sqlc_gen_moonbit$tools$codegen$split_lines(
  moonbit_string_t s$849
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $sqlc_gen_moonbit$tools$codegen$split_by_comma(
  moonbit_string_t s$841
);

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$join_lines(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* lines$833,
  moonbit_string_t sep$836
);

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$generate_code(
  struct $$moonbitlang$core$builtin$Array$3c$QueryDef$3e$* queries$827
);

int32_t $sqlc_gen_moonbit$tools$codegen$generate_query(
  struct $$moonbitlang$core$builtin$StringBuilder* buf$783,
  struct $QueryDef* query$780
);

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$to_snake_case(
  moonbit_string_t s$774
);

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$sql_type_to_moonbit(
  moonbit_string_t sql_type$770
);

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$param_to_bind_call(
  moonbit_string_t sql_type$768,
  moonbit_string_t idx$760,
  moonbit_string_t expr$761
);

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$escape_string(
  moonbit_string_t s$754
);

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$column_to_moonbit(
  moonbit_string_t sql_type$751,
  int32_t idx$744
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $moonbitlang$x$sys$get_cli_args(
  
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $moonbitlang$x$sys$internal$ffi$get_cli_args(
  
);

moonbit_string_t $$moonbitlang$x$sys$internal$ffi$get_cli_args$fn$6(
  struct $$3c$Bytes$3e$$3d$$3e$String* _env$2266,
  moonbit_bytes_t arg$742
);

#define $moonbitlang$x$sys$internal$ffi$internal_get_cli_args moonbit_get_cli_args

struct moonbit_result_0 $moonbitlang$x$fs$read_file_to_string$inner(
  moonbit_string_t path$740,
  moonbit_string_t encoding$741
);

struct moonbit_result_0 $moonbitlang$x$fs$read_file_to_string_internal$inner(
  moonbit_string_t path$739,
  moonbit_string_t encoding$738
);

struct moonbit_result_1 $moonbitlang$x$fs$read_file_to_bytes_internal(
  moonbit_string_t path$734
);

moonbit_string_t $moonbitlang$x$fs$get_error_message();

#define $moonbitlang$x$fs$fclose_ffi moonbitlang_x_fs_fclose_ffi

#define $moonbitlang$x$fs$ftell_ffi moonbitlang_x_fs_ftell_ffi

#define $moonbitlang$x$fs$fseek_ffi moonbitlang_x_fs_fseek_ffi

#define $moonbitlang$x$fs$get_error_message_ffi moonbitlang_x_fs_get_error_message

#define $moonbitlang$x$fs$fread_ffi moonbitlang_x_fs_fread_ffi

#define $moonbitlang$x$fs$is_null moonbitlang_x_fs_is_null

#define $moonbitlang$x$fs$fopen_ffi moonbitlang_x_fs_fopen_ffi

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$x$fs$IOError$$output(
  void* _x_67$729,
  struct $$moonbitlang$core$builtin$Logger _x_68$732
);

moonbit_string_t $moonbitlang$x$internal$ffi$utf8_bytes_to_mbt_string(
  moonbit_bytes_t bytes$725
);

moonbit_bytes_t $moonbitlang$x$internal$ffi$mbt_string_to_utf8_bytes(
  moonbit_string_t str$717,
  int32_t is_filename$722
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$from_fixed_array$0(
  moonbit_string_t* arr$713
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$710,
  struct $$moonbitlang$core$builtin$Logger logger$711
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$673,
  struct $$moonbitlang$core$builtin$Logger logger$709
);

moonbit_bytes_t $Bytes$$from_array(
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ arr$670
);

int32_t $Bytes$$from_array$fn$5(
  struct $$3c$Char$3e$$3d$$3e$Bool* _env$2045,
  int32_t i$671
);

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$669);

moonbit_bytes_t $Bytes$$makei$0(
  int32_t length$664,
  struct $$3c$Char$3e$$3d$$3e$Bool* value$666
);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit_fixed$0(
  moonbit_string_t* dst$658,
  int32_t dst_offset$659,
  moonbit_string_t* src$660,
  int32_t src_offset$661,
  int32_t len$663
);

moonbit_bytes_t $Bytes$$make(int32_t len$655, int32_t init$656);

struct $$3c$Int$2a$Char$3e$* $$moonbitlang$core$builtin$Iter2$$next$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$* self$654
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$make_uninit$0(
  int32_t len$653
);

int32_t $$moonbitlang$core$builtin$ArrayView$$at$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ self$652,
  int32_t index$651
);

moonbit_string_t* $FixedArray$$map$0(
  moonbit_bytes_t* self$645,
  struct $$3c$Bytes$3e$$3d$$3e$String* f$647
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$644
);

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$643,
  struct $$moonbitlang$core$builtin$Logger logger$642
);

struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* $String$$to_array(
  moonbit_string_t self$639
);

moonbit_string_t $String$$to_upper(moonbit_string_t self$628);

int32_t $String$$to_upper$fn$4(
  struct $$3c$Char$3e$$3d$$3e$Bool* _env$2010,
  int32_t _hole3791$629
);

int32_t $Char$$is_ascii_lowercase(int32_t self$626);

struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* $String$$iter(
  moonbit_string_t self$621
);

int32_t $String$$iter$$2a$p$fn$3(
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* _env$1985
);

struct $StringView $String$$trim(
  moonbit_string_t self$619,
  void* chars$opt$617
);

struct $StringView $String$$trim$inner(
  moonbit_string_t self$614,
  struct $StringView chars$615
);

struct $StringView $StringView$$trim$inner(
  struct $StringView self$612,
  struct $StringView chars$613
);

struct $StringView $StringView$$trim_end$inner(
  struct $StringView self$611,
  struct $StringView chars$609
);

struct $StringView $StringView$$trim_start$inner(
  struct $StringView self$605,
  struct $StringView chars$603
);

int32_t $StringView$$contains_char(
  struct $StringView self$589,
  int32_t c$591
);

struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* $$moonbitlang$core$builtin$Iter$$fold$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* self$583,
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* init$581,
  struct $$3c$$moonbitlang$core$builtin$Array$3c$Char$3e$$2a$Char$3e$$3d$$3e$$moonbitlang$core$builtin$Array$3c$Char$3e$* f$586
);

struct $$3c$Int$2a$Char$3e$* $$moonbitlang$core$builtin$Iter$$next$1(
  struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$* self$579
);

int32_t $$moonbitlang$core$builtin$Iter$$next$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* self$577
);

int32_t $$moonbitlang$core$builtin$Array$$push$4(
  struct $$moonbitlang$core$builtin$Array$3c$Byte$3e$* self$573,
  int32_t value$575
);

int32_t $$moonbitlang$core$builtin$Array$$push$3(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$570,
  int32_t value$572
);

int32_t $$moonbitlang$core$builtin$Array$$push$2(
  struct $$moonbitlang$core$builtin$Array$3c$QueryDef$3e$* self$567,
  struct $QueryDef* value$569
);

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$564,
  moonbit_string_t value$566
);

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* self$561,
  struct $$3c$String$2a$String$3e$* value$563
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$4(
  struct $$moonbitlang$core$builtin$Array$3c$Byte$3e$* self$559
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$3(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$556
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$2(
  struct $$moonbitlang$core$builtin$Array$3c$QueryDef$3e$* self$553
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$550
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* self$547
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$4(
  struct $$moonbitlang$core$builtin$Array$3c$Byte$3e$* self$543,
  int32_t new_capacity$541
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$3(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$537,
  int32_t new_capacity$535
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$QueryDef$3e$* self$531,
  int32_t new_capacity$529
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$525,
  int32_t new_capacity$523
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* self$519,
  int32_t new_capacity$517
);

struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$515
);

int32_t $String$$has_prefix(
  moonbit_string_t self$513,
  struct $StringView str$514
);

int32_t $StringView$$has_prefix(
  struct $StringView self$509,
  struct $StringView str$510
);

int32_t $String$$has_suffix(
  moonbit_string_t self$506,
  struct $StringView str$507
);

int32_t $StringView$$has_suffix(
  struct $StringView self$502,
  struct $StringView str$503
);

int64_t $StringView$$rev_find(
  struct $StringView self$500,
  struct $StringView str$499
);

int64_t $moonbitlang$core$builtin$brute_force_rev_find(
  struct $StringView haystack$490,
  struct $StringView needle$492
);

int64_t $String$$find_by(
  moonbit_string_t self$487,
  struct $$3c$Char$3e$$3d$$3e$Bool* pred$488
);

int64_t $StringView$$find_by(
  struct $StringView self$479,
  struct $$3c$Char$3e$$3d$$3e$Bool* pred$485
);

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
  struct $StringView haystack$468,
  struct $StringView needle$470
);

int64_t $StringView$$find(
  struct $StringView self$466,
  struct $StringView str$465
);

int64_t $moonbitlang$core$builtin$brute_force_find(
  struct $StringView haystack$455,
  struct $StringView needle$457
);

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find(
  struct $StringView haystack$441,
  struct $StringView needle$443
);

int32_t $$moonbitlang$core$builtin$StringBuilder$$reset(
  struct $$moonbitlang$core$builtin$StringBuilder* self$438
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$436,
  struct $StringView str$437
);

int64_t $String$$offset_of_nth_char$inner(
  moonbit_string_t self$433,
  int32_t i$434,
  int32_t start_offset$435,
  int64_t end_offset$431
);

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$428,
  int32_t n$426,
  int32_t start_offset$422,
  int32_t end_offset$423
);

int64_t $String$$offset_of_nth_char_backward(
  moonbit_string_t self$420,
  int32_t n$418,
  int32_t start_offset$417,
  int32_t end_offset$416
);

int32_t $String$$char_length_eq$inner(
  moonbit_string_t self$406,
  int32_t len$409,
  int32_t start_offset$413,
  int64_t end_offset$404
);

moonbit_string_t $String$$from_array(
  struct $$moonbitlang$core$builtin$ArrayView$3c$Char$3e$ chars$398
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ self$396
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$Char$3e$ self$395
);

struct $StringView $String$$view$inner(
  moonbit_string_t self$393,
  int32_t start_offset$394,
  int64_t end_offset$391
);

struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$* $StringView$$iter2(
  struct $StringView self$382
);

struct $$3c$Int$2a$Char$3e$* $StringView$$iter2$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$* _env$1680
);

struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$* $$moonbitlang$core$builtin$Iter2$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$* f$380
);

struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* $StringView$$iter(
  struct $StringView self$375
);

int32_t $StringView$$iter$$2a$p$fn$1(
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* _env$1662
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$StringView$$to_string(
  struct $StringView self$373
);

int32_t $$moonbitlang$core$builtin$Show$$String$$output(
  moonbit_string_t self$365,
  struct $$moonbitlang$core$builtin$Logger logger$363
);

int32_t $moonbitlang$core$builtin$output$flush_segment$7c$3594(
  struct $$3c$String$2a$$moonbitlang$core$builtin$Logger$3e$* _env$359,
  int32_t seg$362,
  int32_t i$361
);

moonbit_string_t $Byte$$to_hex(int32_t b$357);

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3604(int32_t i$355);

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$353,
  int32_t that$354
);

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$351,
  int32_t that$352
);

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$349,
  int32_t that$350
);

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$347,
  int32_t that$348
);

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$344,
  int32_t start$342,
  int32_t end$343
);

int32_t $StringView$$unsafe_charcode_at(
  struct $StringView self$340,
  int32_t index$341
);

struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$* $$moonbitlang$core$builtin$Iter$$new$1(
  struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$* f$339
);

struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* $$moonbitlang$core$builtin$Iter$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* f$338
);

moonbit_string_t $Int$$to_string$inner(int32_t self$322, int32_t radix$321);

int32_t $moonbitlang$core$builtin$radix_count32(
  uint32_t value$315,
  int32_t radix$318
);

int32_t $moonbitlang$core$builtin$hex_count32(uint32_t value$313);

int32_t $moonbitlang$core$builtin$dec_count32(uint32_t value$312);

int32_t $moonbitlang$core$builtin$int_to_string_dec(
  uint16_t* buffer$302,
  uint32_t num$290,
  int32_t digit_start$293,
  int32_t total_len$292
);

int32_t $moonbitlang$core$builtin$int_to_string_generic(
  uint16_t* buffer$284,
  uint32_t num$278,
  int32_t digit_start$276,
  int32_t total_len$275,
  int32_t radix$280
);

int32_t $moonbitlang$core$builtin$int_to_string_hex(
  uint16_t* buffer$271,
  uint32_t num$267,
  int32_t digit_start$265,
  int32_t total_len$264
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  int32_t self$262
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$260
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  void* self$258
);

int32_t $StringView$$start_offset(struct $StringView self$256);

int32_t $StringView$$length(struct $StringView self$255);

moonbit_string_t $StringView$$data(struct $StringView self$254);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$248,
  moonbit_string_t value$251,
  int32_t start$252,
  int32_t len$253
);

struct moonbit_result_2 $String$$sub(
  moonbit_string_t self$246,
  int64_t start$opt$244,
  int64_t end$247
);

struct moonbit_result_2 $String$$sub$inner(
  moonbit_string_t self$236,
  int32_t start$242,
  int64_t end$238
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$233,
  moonbit_string_t str$234
);

int32_t $FixedArray$$blit_from_string(
  moonbit_bytes_t self$225,
  int32_t bytes_offset$220,
  moonbit_string_t str$227,
  int32_t str_offset$223,
  int32_t length$221
);

struct $$moonbitlang$core$builtin$SourceLocRepr* $$moonbitlang$core$builtin$SourceLocRepr$$parse(
  moonbit_string_t repr$142
);

moonbit_bytes_t $$moonbitlang$core$builtin$Array$$buffer$4(
  struct $$moonbitlang$core$builtin$Array$3c$Byte$3e$* self$140
);

int32_t* $$moonbitlang$core$builtin$Array$$buffer$3(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$139
);

struct $QueryDef** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$QueryDef$3e$* self$138
);

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$137
);

struct $$3c$String$2a$String$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* self$136
);

int32_t $String$$unsafe_char_at(moonbit_string_t self$133, int32_t index$134);

int32_t $moonbitlang$core$builtin$code_point_of_surrogate_pair(
  int32_t leading$130,
  int32_t trailing$131
);

int32_t $String$$unsafe_charcode_at(
  moonbit_string_t self$128,
  int32_t idx$129
);

int32_t $Int$$is_trailing_surrogate(int32_t self$127);

int32_t $Int$$is_leading_surrogate(int32_t self$126);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
  struct $$moonbitlang$core$builtin$StringBuilder* self$123,
  int32_t ch$125
);

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$118,
  int32_t required$119
);

int32_t $$moonbitlang$core$builtin$Default$$Byte$$default();

int32_t $FixedArray$$set_utf16le_char(
  moonbit_bytes_t self$112,
  int32_t offset$113,
  int32_t value$111
);

int32_t $UInt$$to_byte(uint32_t self$109);

uint32_t $Char$$to_uint(int32_t self$108);

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$107
);

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$102,
  int32_t offset$106,
  int64_t length$104
);

#define $moonbitlang$core$builtin$unsafe_sub_string moonbit_unsafe_bytes_sub_string

struct $$moonbitlang$core$builtin$StringBuilder* $$moonbitlang$core$builtin$StringBuilder$$new$inner(
  int32_t size_hint$99
);

int32_t $Byte$$to_char(int32_t self$97);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$4(
  moonbit_bytes_t dst$92,
  int32_t dst_offset$93,
  moonbit_bytes_t src$94,
  int32_t src_offset$95,
  int32_t len$96
);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$3(
  int32_t* dst$87,
  int32_t dst_offset$88,
  int32_t* src$89,
  int32_t src_offset$90,
  int32_t len$91
);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
  struct $QueryDef** dst$82,
  int32_t dst_offset$83,
  struct $QueryDef** src$84,
  int32_t src_offset$85,
  int32_t len$86
);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
  moonbit_string_t* dst$77,
  int32_t dst_offset$78,
  moonbit_string_t* src$79,
  int32_t src_offset$80,
  int32_t len$81
);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
  struct $$3c$String$2a$String$3e$** dst$72,
  int32_t dst_offset$73,
  struct $$3c$String$2a$String$3e$** src$74,
  int32_t src_offset$75,
  int32_t len$76
);

int32_t $FixedArray$$unsafe_blit$5(
  moonbit_bytes_t dst$63,
  int32_t dst_offset$65,
  moonbit_bytes_t src$64,
  int32_t src_offset$66,
  int32_t len$68
);

int32_t $FixedArray$$unsafe_blit$4(
  int32_t* dst$54,
  int32_t dst_offset$56,
  int32_t* src$55,
  int32_t src_offset$57,
  int32_t len$59
);

int32_t $FixedArray$$unsafe_blit$3(
  struct $QueryDef** dst$45,
  int32_t dst_offset$47,
  struct $QueryDef** src$46,
  int32_t src_offset$48,
  int32_t len$50
);

int32_t $FixedArray$$unsafe_blit$2(
  moonbit_string_t* dst$36,
  int32_t dst_offset$38,
  moonbit_string_t* src$37,
  int32_t src_offset$39,
  int32_t len$41
);

int32_t $FixedArray$$unsafe_blit$1(
  struct $$3c$String$2a$String$3e$** dst$27,
  int32_t dst_offset$29,
  struct $$3c$String$2a$String$3e$** src$28,
  int32_t src_offset$30,
  int32_t len$32
);

int32_t $FixedArray$$unsafe_blit$0(
  moonbit_bytes_t dst$18,
  int32_t dst_offset$20,
  moonbit_bytes_t src$19,
  int32_t src_offset$21,
  int32_t len$23
);

int64_t $moonbitlang$core$builtin$abort$4(
  moonbit_string_t string$16,
  moonbit_string_t loc$17
);

int32_t $moonbitlang$core$builtin$abort$3(
  moonbit_string_t string$14,
  moonbit_string_t loc$15
);

struct $StringView $moonbitlang$core$builtin$abort$2(
  moonbit_string_t string$12,
  moonbit_string_t loc$13
);

int32_t $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$10,
  moonbit_string_t loc$11
);

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$8,
  moonbit_string_t loc$9
);

int32_t $$moonbitlang$core$builtin$Logger$$write_object$0(
  struct $$moonbitlang$core$builtin$Logger self$7,
  moonbit_string_t obj$6
);

int64_t $moonbitlang$core$abort$abort$4(moonbit_string_t msg$5);

int32_t $moonbitlang$core$abort$abort$3(moonbit_string_t msg$4);

struct $StringView $moonbitlang$core$abort$abort$2(moonbit_string_t msg$3);

int32_t $moonbitlang$core$abort$abort$1(moonbit_string_t msg$2);

int32_t $moonbitlang$core$abort$abort$0(moonbit_string_t msg$1);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1248,
  int32_t _param$1247
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1245,
  struct $StringView _param$1244
);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1242,
  moonbit_string_t _param$1239,
  int32_t _param$1240,
  int32_t _param$1241
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1237,
  moonbit_string_t _param$1236
);

moonbit_bytes_t moonbitlang_x_fs_get_error_message();

void* moonbitlang_x_fs_fopen_ffi(moonbit_bytes_t $0, moonbit_bytes_t $1);

int32_t moonbitlang_x_fs_fread_ffi(
  moonbit_bytes_t $0,
  int32_t $1,
  int32_t $2,
  void* $3
);

int32_t moonbitlang_x_fs_ftell_ffi(void* $0);

int32_t moonbitlang_x_fs_fseek_ffi(void* $0, int32_t $1, int32_t $2);

int32_t moonbitlang_x_fs_fclose_ffi(void* $0);

int32_t moonbitlang_x_fs_is_null(void* $0);

struct { int32_t rc; uint32_t meta; uint16_t const data[52]; 
} const moonbit_string_literal_114 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 51), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 97, 114, 
    114, 97, 121, 118, 105, 101, 119, 46, 109, 98, 116, 58, 49, 50, 52, 
    58, 53, 45, 49, 50, 54, 58, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[1]; 
} const moonbit_string_literal_111 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 0), 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[50]; 
} const moonbit_string_literal_101 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 49), 
    98, 121, 116, 101, 115, 95, 116, 111, 95, 115, 116, 114, 105, 110, 
    103, 40, 64, 115, 113, 108, 105, 116, 101, 46, 115, 113, 108, 105, 
    116, 101, 95, 99, 111, 108, 117, 109, 110, 95, 116, 101, 120, 116, 
    40, 115, 116, 109, 116, 44, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_41 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    44, 32, 112, 97, 114, 97, 109, 115, 32, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_54 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[36]; 
} const moonbit_string_literal_103 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 35), 
    64, 115, 113, 108, 105, 116, 101, 46, 115, 113, 108, 105, 116, 101, 
    95, 99, 111, 108, 117, 109, 110, 95, 100, 111, 117, 98, 108, 101, 
    40, 115, 116, 109, 116, 44, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_121 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[11]; 
} const moonbit_string_literal_2 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 10), 
    45, 45, 32, 64, 112, 97, 114, 97, 109, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_128 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 48, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_39 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    10, 112, 117, 98, 32, 102, 110, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_16 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    58, 111, 110, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_45 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    32, 124, 62, 32, 105, 103, 110, 111, 114, 101, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[48]; 
} const moonbit_string_literal_7 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 47), 
    47, 47, 32, 71, 101, 110, 101, 114, 97, 116, 101, 100, 32, 98, 121, 
    32, 115, 113, 108, 99, 45, 103, 101, 110, 45, 109, 111, 111, 110, 
    98, 105, 116, 32, 40, 115, 116, 97, 110, 100, 97, 108, 111, 110, 
    101, 41, 10, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_44 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    112, 97, 114, 97, 109, 115, 46, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_76 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    68, 111, 117, 98, 108, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[50]; 
} const moonbit_string_literal_118 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 49), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 115, 116, 
    114, 105, 110, 103, 46, 109, 98, 116, 58, 52, 50, 54, 58, 57, 45, 
    52, 50, 54, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_63 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    32, 32, 125, 32, 101, 108, 115, 101, 32, 123, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_15 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    80, 97, 114, 97, 109, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_14 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    82, 111, 119, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[21]; 
} const moonbit_string_literal_23 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 20), 
    125, 32, 100, 101, 114, 105, 118, 101, 40, 83, 104, 111, 119, 44, 
    32, 69, 113, 41, 10, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_10 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    125, 10, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_127 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 116, 111, 
    95, 115, 116, 114, 105, 110, 103, 46, 109, 98, 116, 58, 50, 51, 57, 
    58, 53, 45, 50, 51, 57, 58, 52, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_94 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 41, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[48]; 
} const moonbit_string_literal_137 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 47), 
    32, 32, 45, 45, 32, 64, 114, 101, 116, 117, 114, 110, 115, 32, 105, 
    100, 32, 73, 78, 84, 69, 71, 69, 82, 44, 32, 110, 97, 109, 101, 32, 
    84, 69, 88, 84, 44, 32, 101, 109, 97, 105, 108, 32, 84, 69, 88, 84, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[15]; 
} const moonbit_string_literal_51 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 14), 
    32, 32, 32, 32, 108, 101, 116, 32, 114, 111, 119, 32, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[39]; 
} const moonbit_string_literal_46 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 38), 
    32, 32, 64, 115, 113, 108, 105, 116, 101, 46, 115, 113, 108, 105, 
    116, 101, 95, 115, 116, 101, 112, 40, 115, 116, 109, 116, 41, 32, 
    124, 62, 32, 105, 103, 110, 111, 114, 101, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_81 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    83, 77, 65, 76, 76, 73, 78, 84, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_6 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 32, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_78 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    68, 79, 85, 66, 76, 69, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[40]; 
} const moonbit_string_literal_8 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 39), 
    47, 47, 47, 124, 10, 102, 110, 32, 99, 115, 116, 114, 105, 110, 103, 
    40, 115, 32, 58, 32, 83, 116, 114, 105, 110, 103, 41, 32, 45, 62, 
    32, 66, 121, 116, 101, 115, 32, 123, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_80 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    82, 69, 65, 76, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_99 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 34, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[34]; 
} const moonbit_string_literal_104 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 33), 
    64, 115, 113, 108, 105, 116, 101, 46, 115, 113, 108, 105, 116, 101, 
    95, 99, 111, 108, 117, 109, 110, 95, 98, 108, 111, 98, 40, 115, 116, 
    109, 116, 44, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[11]; 
} const moonbit_string_literal_92 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 10), 
    46, 116, 111, 95, 105, 110, 116, 40, 41, 41, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_55 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    44, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_124 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_122 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_110 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    73, 79, 69, 114, 114, 111, 114, 40, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_72 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    67, 72, 65, 82, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_70 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    66, 121, 116, 101, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[14]; 
} const moonbit_string_literal_31 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 13), 
    47, 47, 47, 124, 10, 112, 117, 98, 32, 108, 101, 116, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_75 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    68, 69, 67, 73, 77, 65, 76, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_87 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    73, 78, 84, 69, 71, 69, 82, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_73 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    86, 65, 82, 67, 72, 65, 82, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[35]; 
} const moonbit_string_literal_61 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 34), 
    32, 32, 32, 32, 64, 115, 113, 108, 105, 116, 101, 46, 115, 113, 108, 
    105, 116, 101, 95, 102, 105, 110, 97, 108, 105, 122, 101, 40, 115, 
    116, 109, 116, 41, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_34 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    85, 110, 105, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_108 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    85, 110, 115, 117, 112, 112, 111, 114, 116, 101, 100, 32, 101, 110, 
    99, 111, 100, 105, 110, 103, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[19]; 
} const moonbit_string_literal_98 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 18), 
    32, 123, 32, 49, 32, 125, 32, 101, 108, 115, 101, 32, 123, 32, 48, 
    32, 125, 41, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_56 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    32, 32, 32, 32, 125, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_17 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    58, 109, 97, 110, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[11]; 
} const moonbit_string_literal_59 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 10), 
    32, 32, 114, 101, 115, 117, 108, 116, 115, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[25]; 
} const moonbit_string_literal_135 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 24), 
    32, 32, 45, 45, 32, 64, 113, 117, 101, 114, 121, 32, 71, 101, 116, 
    85, 115, 101, 114, 32, 58, 111, 110, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[20]; 
} const moonbit_string_literal_115 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 19), 
    73, 110, 118, 97, 108, 105, 100, 32, 115, 116, 97, 114, 116, 32, 
    105, 110, 100, 101, 120, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[35]; 
} const moonbit_string_literal_102 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 34), 
    64, 115, 113, 108, 105, 116, 101, 46, 115, 113, 108, 105, 116, 101, 
    95, 99, 111, 108, 117, 109, 110, 95, 105, 110, 116, 54, 52, 40, 115, 
    116, 109, 116, 44, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_38 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    47, 47, 47, 124, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[11]; 
} const moonbit_string_literal_1 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 10), 
    45, 45, 32, 64, 113, 117, 101, 114, 121, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_43 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    41, 41, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_25 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    58, 58, 110, 101, 119, 40, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_134 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    83, 81, 76, 32, 102, 105, 108, 101, 32, 102, 111, 114, 109, 97, 116, 
    58, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[33]; 
} const moonbit_string_literal_105 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 32), 
    64, 115, 113, 108, 105, 116, 101, 46, 115, 113, 108, 105, 116, 101, 
    95, 99, 111, 108, 117, 109, 110, 95, 105, 110, 116, 40, 115, 116, 
    109, 116, 44, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_53 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    32, 32, 32, 32, 32, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[54]; 
} const moonbit_string_literal_120 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 53), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 115, 116, 
    114, 105, 110, 103, 118, 105, 101, 119, 46, 109, 98, 116, 58, 51, 
    56, 50, 58, 53, 45, 51, 56, 50, 58, 51, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_74 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    84, 69, 88, 84, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_28 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    32, 123, 10, 32, 32, 123, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_85 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    73, 78, 84, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_30 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    95, 115, 113, 108, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_19 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    32, 123, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_5 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 59, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[32]; 
} const moonbit_string_literal_88 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 31), 
    64, 115, 113, 108, 105, 116, 101, 46, 115, 113, 108, 105, 116, 101, 
    95, 98, 105, 110, 100, 95, 116, 101, 120, 116, 40, 115, 116, 109, 
    116, 44, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_82 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    73, 110, 116, 54, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_26 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    44, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[48]; 
} const moonbit_string_literal_11 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 47), 
    47, 47, 47, 124, 10, 102, 110, 32, 98, 121, 116, 101, 115, 95, 116, 
    111, 95, 115, 116, 114, 105, 110, 103, 40, 98, 32, 58, 32, 66, 121, 
    116, 101, 115, 41, 32, 45, 62, 32, 83, 116, 114, 105, 110, 103, 32, 
    123, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[56]; 
} const moonbit_string_literal_60 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 55), 
    32, 32, 105, 102, 32, 64, 115, 113, 108, 105, 116, 101, 46, 115, 
    113, 108, 105, 116, 101, 95, 115, 116, 101, 112, 40, 115, 116, 109, 
    116, 41, 32, 61, 61, 32, 64, 115, 113, 108, 105, 116, 101, 46, 83, 
    81, 76, 73, 84, 69, 95, 82, 79, 87, 32, 123, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_117 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    105, 110, 118, 97, 108, 105, 100, 32, 115, 117, 114, 114, 111, 103, 
    97, 116, 101, 32, 112, 97, 105, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_132 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    10, 32, 32, 97, 116, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[31]; 
} const moonbit_string_literal_126 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 30), 
    114, 97, 100, 105, 120, 32, 109, 117, 115, 116, 32, 98, 101, 32, 
    98, 101, 116, 119, 101, 101, 110, 32, 50, 32, 97, 110, 100, 32, 51, 
    54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_57 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    32, 32, 32, 32, 114, 101, 115, 117, 108, 116, 115, 46, 112, 117, 
    115, 104, 40, 114, 111, 119, 41, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[36]; 
} const moonbit_string_literal_138 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 35), 
    32, 32, 83, 69, 76, 69, 67, 84, 32, 42, 32, 70, 82, 79, 77, 32, 117, 
    115, 101, 114, 115, 32, 87, 72, 69, 82, 69, 32, 105, 100, 32, 61, 
    32, 63, 59, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[31]; 
} const moonbit_string_literal_91 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 30), 
    64, 115, 113, 108, 105, 116, 101, 46, 115, 113, 108, 105, 116, 101, 
    95, 98, 105, 110, 100, 95, 105, 110, 116, 40, 115, 116, 109, 116, 
    44, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_84 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    73, 78, 84, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[34]; 
} const moonbit_string_literal_95 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 33), 
    64, 115, 113, 108, 105, 116, 101, 46, 115, 113, 108, 105, 116, 101, 
    95, 98, 105, 110, 100, 95, 100, 111, 117, 98, 108, 101, 40, 115, 
    116, 109, 116, 44, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[59]; 
} const moonbit_string_literal_50 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 58), 
    32, 32, 119, 104, 105, 108, 101, 32, 64, 115, 113, 108, 105, 116, 
    101, 46, 115, 113, 108, 105, 116, 101, 95, 115, 116, 101, 112, 40, 
    115, 116, 109, 116, 41, 32, 61, 61, 32, 64, 115, 113, 108, 105, 116, 
    101, 46, 83, 81, 76, 73, 84, 69, 95, 82, 79, 87, 32, 123, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_21 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    32, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[49]; 
} const moonbit_string_literal_42 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 48), 
    32, 32, 108, 101, 116, 32, 115, 116, 109, 116, 32, 61, 32, 64, 115, 
    113, 108, 105, 116, 101, 46, 115, 113, 108, 105, 116, 101, 95, 112, 
    114, 101, 112, 97, 114, 101, 40, 100, 98, 44, 32, 99, 115, 116, 114, 
    105, 110, 103, 40, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[41]; 
} const moonbit_string_literal_13 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 40), 
    32, 32, 100, 101, 99, 111, 100, 101, 114, 46, 100, 101, 99, 111, 
    100, 101, 95, 108, 111, 115, 115, 121, 40, 98, 91, 48, 58, 98, 46, 
    108, 101, 110, 103, 116, 104, 40, 41, 93, 41, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[32]; 
} const moonbit_string_literal_96 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 31), 
    64, 115, 113, 108, 105, 116, 101, 46, 115, 113, 108, 105, 116, 101, 
    95, 98, 105, 110, 100, 95, 98, 108, 111, 98, 40, 115, 116, 109, 116, 
    44, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[11]; 
} const moonbit_string_literal_89 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 10), 
    44, 32, 99, 115, 116, 114, 105, 110, 103, 40, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_0 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    9, 10, 13, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_107 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    117, 116, 102, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[49]; 
} const moonbit_string_literal_131 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 48), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 98, 121, 
    116, 101, 115, 46, 109, 98, 116, 58, 50, 57, 56, 58, 53, 45, 50, 
    57, 56, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[37]; 
} const moonbit_string_literal_129 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 36), 
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 97, 98, 99, 100, 101, 102, 
    103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 
    116, 117, 118, 119, 120, 121, 122, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_136 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    32, 32, 45, 45, 32, 64, 112, 97, 114, 97, 109, 32, 105, 100, 32, 
    73, 78, 84, 69, 71, 69, 82, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[43]; 
} const moonbit_string_literal_112 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 42), 
    105, 110, 100, 101, 120, 32, 111, 117, 116, 32, 111, 102, 32, 98, 
    111, 117, 110, 100, 115, 58, 32, 116, 104, 101, 32, 108, 101, 110, 
    32, 105, 115, 32, 102, 114, 111, 109, 32, 48, 32, 116, 111, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[29]; 
} const moonbit_string_literal_133 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 28), 
    85, 115, 97, 103, 101, 58, 32, 99, 111, 100, 101, 103, 101, 110, 
    32, 60, 113, 117, 101, 114, 105, 101, 115, 46, 115, 113, 108, 62, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_37 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 63, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_22 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 10, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_83 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    66, 73, 71, 73, 78, 84, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_33 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    34, 10, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_20 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    32, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[33]; 
} const moonbit_string_literal_93 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 32), 
    64, 115, 113, 108, 105, 116, 101, 46, 115, 113, 108, 105, 116, 101, 
    95, 98, 105, 110, 100, 95, 105, 110, 116, 54, 52, 40, 115, 116, 109, 
    116, 44, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_49 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    93, 32, 61, 32, 91, 93, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[21]; 
} const moonbit_string_literal_139 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 20), 
    69, 114, 114, 111, 114, 32, 114, 101, 97, 100, 105, 110, 103, 32, 
    102, 105, 108, 101, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[19]; 
} const moonbit_string_literal_113 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 18), 
    32, 98, 117, 116, 32, 116, 104, 101, 32, 105, 110, 100, 101, 120, 
    32, 105, 115, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[33]; 
} const moonbit_string_literal_47 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 32), 
    32, 32, 64, 115, 113, 108, 105, 116, 101, 46, 115, 113, 108, 105, 
    116, 101, 95, 102, 105, 110, 97, 108, 105, 122, 101, 40, 115, 116, 
    109, 116, 41, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_27 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    41, 32, 45, 62, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_69 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    66, 89, 84, 69, 65, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_86 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    73, 78, 84, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[33]; 
} const moonbit_string_literal_109 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 32), 
    44, 32, 111, 110, 108, 121, 32, 117, 116, 102, 56, 32, 105, 115, 
    32, 115, 117, 112, 112, 111, 114, 116, 101, 100, 32, 102, 111, 114, 
    32, 110, 111, 119, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_71 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    66, 76, 79, 66, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_66 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    66, 79, 79, 76, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[50]; 
} const moonbit_string_literal_116 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 49), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 115, 116, 
    114, 105, 110, 103, 46, 109, 98, 116, 58, 51, 50, 57, 58, 53, 45, 
    51, 50, 57, 58, 51, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_119 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    73, 110, 118, 97, 108, 105, 100, 32, 105, 110, 100, 101, 120, 32, 
    102, 111, 114, 32, 86, 105, 101, 119, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_65 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    83, 116, 114, 105, 110, 103, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[18]; 
} const moonbit_string_literal_130 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 17), 
    67, 104, 97, 114, 32, 111, 117, 116, 32, 111, 102, 32, 114, 97, 110, 
    103, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_52 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    32, 61, 32, 123, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[39]; 
} const moonbit_string_literal_9 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 38), 
    32, 32, 64, 101, 110, 99, 111, 100, 105, 110, 103, 46, 101, 110, 
    99, 111, 100, 101, 40, 64, 101, 110, 99, 111, 100, 105, 110, 103, 
    46, 85, 84, 70, 56, 44, 32, 115, 41, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_58 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    32, 32, 125, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_125 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    92, 117, 123, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_48 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    32, 32, 108, 101, 116, 32, 114, 101, 115, 117, 108, 116, 115, 32, 
    58, 32, 65, 114, 114, 97, 121, 91, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_4 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    45, 45, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_90 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    41, 41, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_36 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 93, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_3 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    45, 45, 32, 64, 114, 101, 116, 117, 114, 110, 115, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_106 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    41, 32, 33, 61, 32, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_67 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    66, 111, 111, 108, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_79 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    70, 76, 79, 65, 84, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_123 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 98, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_35 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    65, 114, 114, 97, 121, 91, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[15]; 
} const moonbit_string_literal_62 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 14), 
    32, 32, 32, 32, 83, 111, 109, 101, 40, 114, 111, 119, 41, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_24 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    47, 47, 47, 124, 10, 112, 117, 98, 32, 102, 110, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_100 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 92, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[14]; 
} const moonbit_string_literal_32 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 13), 
    32, 58, 32, 83, 116, 114, 105, 110, 103, 32, 61, 32, 34, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_97 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    44, 32, 105, 102, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_68 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    66, 79, 79, 76, 69, 65, 78, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_29 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    32, 125, 10, 125, 10, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_18 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    47, 47, 47, 124, 10, 112, 117, 98, 32, 115, 116, 114, 117, 99, 116, 
    32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[22]; 
} const moonbit_string_literal_40 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 21), 
    40, 100, 98, 32, 58, 32, 64, 115, 113, 108, 105, 116, 101, 46, 83, 
    113, 108, 105, 116, 101, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[51]; 
} const moonbit_string_literal_12 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 50), 
    32, 32, 108, 101, 116, 32, 100, 101, 99, 111, 100, 101, 114, 32, 
    61, 32, 64, 101, 110, 99, 111, 100, 105, 110, 103, 46, 100, 101, 
    99, 111, 100, 101, 114, 40, 64, 101, 110, 99, 111, 100, 105, 110, 
    103, 46, 85, 84, 70, 56, 41, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[10]; 
} const moonbit_string_literal_64 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 9), 
    32, 32, 32, 32, 78, 111, 110, 101, 10, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_77 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    78, 85, 77, 69, 82, 73, 67, 0
  };

struct { int32_t rc; uint32_t meta; uint8_t const data[1]; 
} const moonbit_bytes_literal_1 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 0, 0), 0};

struct { int32_t rc; uint32_t meta; uint8_t const data[4]; 
} const moonbit_bytes_literal_0 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 0, 3), 
    114, 98, 0, 0
  };

struct moonbit_object const moonbit_constant_constructor_0 =
  { -1, Moonbit_make_regular_object_header(2, 0, 0)};

struct moonbit_object const moonbit_constant_constructor_1 =
  { -1, Moonbit_make_regular_object_header(2, 0, 1)};

struct { int32_t rc; uint32_t meta; struct $$3c$Char$3e$$3d$$3e$Bool data; 
} const $String$$to_upper$fn$4$closure =
  { -1, Moonbit_make_regular_object_header(2, 0, 0), $String$$to_upper$fn$4};

struct { int32_t rc; uint32_t meta; struct $$3c$Bytes$3e$$3d$$3e$String data; 
} const $$moonbitlang$x$sys$internal$ffi$get_cli_args$fn$6$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $$moonbitlang$x$sys$internal$ffi$get_cli_args$fn$6
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$moonbitlang$core$builtin$Logger$static_method_table data;
  
} $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id$object =
  {
    -1,
    Moonbit_make_regular_object_header(
      sizeof(
        struct $$moonbitlang$core$builtin$Logger$static_method_table
      )
      >> 2,
        0,
        0
    ),
    {
      .$method_0 = $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger,
        .$method_1 = $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0,
        .$method_2 = $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger,
        .$method_3 = $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger
    }
  };

struct $$moonbitlang$core$builtin$Logger$static_method_table* $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id =
  &$$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id$object.data;

moonbit_string_t $moonbitlang$core$builtin$trim$$2a$bind$7c$6141 =
  (moonbit_string_t)moonbit_string_literal_0.data;

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$439;

int64_t $moonbitlang$core$builtin$brute_force_find$constr$453;

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$208 =
  (moonbit_string_t)moonbit_string_literal_1.data;

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$210 =
  (moonbit_string_t)moonbit_string_literal_2.data;

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$213 =
  (moonbit_string_t)moonbit_string_literal_3.data;

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$218 =
  (moonbit_string_t)moonbit_string_literal_1.data;

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$219 =
  (moonbit_string_t)moonbit_string_literal_4.data;

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$220 =
  (moonbit_string_t)moonbit_string_literal_5.data;

struct $$moonbitlang$core$builtin$Array$3c$QueryDef$3e$* $sqlc_gen_moonbit$tools$codegen$parse_queries(
  moonbit_string_t content$870
) {
  struct $QueryDef** _tmp$2442 = (struct $QueryDef**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$QueryDef$3e$* queries$868 =
    (struct $$moonbitlang$core$builtin$Array$3c$QueryDef$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$QueryDef$3e$)
    );
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* lines$869;
  int32_t i$871;
  Moonbit_object_header(queries$868)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$QueryDef$3e$, $0
    )
    >> 2,
      1,
      0
  );
  queries$868->$0 = _tmp$2442;
  queries$868->$1 = 0;
  lines$869 = $sqlc_gen_moonbit$tools$codegen$split_lines(content$870);
  i$871 = 0;
  while (1) {
    int32_t _tmp$2379 = i$871;
    int32_t len$2380 = lines$869->$1;
    if (_tmp$2379 < len$2380) {
      int32_t _bind$873 = i$871;
      int32_t _if_result$2778;
      moonbit_string_t* _field$2460;
      moonbit_string_t* buf$2441;
      moonbit_string_t _tmp$2459;
      moonbit_string_t _tmp$2438;
      void* None$2439;
      struct $StringView _tmp$2437;
      moonbit_string_t line$872;
      int32_t _tmp$2382;
      struct $StringView _tmp$2381;
      int32_t _tmp$2436;
      if (_bind$873 < 0) {
        _if_result$2778 = 1;
      } else {
        int32_t len$2440 = lines$869->$1;
        _if_result$2778 = _bind$873 >= len$2440;
      }
      if (_if_result$2778) {
        moonbit_panic();
      }
      _field$2460 = lines$869->$0;
      buf$2441 = _field$2460;
      _tmp$2459 = (moonbit_string_t)buf$2441[_bind$873];
      _tmp$2438 = _tmp$2459;
      None$2439 = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      moonbit_incref(_tmp$2438);
      _tmp$2437 = $String$$trim(_tmp$2438, None$2439);
      line$872
      = $$moonbitlang$core$builtin$Show$$StringView$$to_string(
        _tmp$2437
      );
      _tmp$2382
      = Moonbit_array_length(
        $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$208
      );
      moonbit_incref(
        $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$208
      );
      _tmp$2381
      = (struct $StringView){
        0,
          _tmp$2382,
          $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$208
      };
      moonbit_incref(line$872);
      if ($String$$has_prefix(line$872, _tmp$2381)) {
        moonbit_string_t _tmp$2435 =
          $sqlc_gen_moonbit$tools$codegen$substr(line$872, 10);
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$* parts$874 =
          $sqlc_gen_moonbit$tools$codegen$split_whitespace(_tmp$2435);
        int32_t len$2383 = parts$874->$1;
        if (len$2383 >= 2) {
          int32_t len$2433 = parts$874->$1;
          moonbit_string_t* _field$2458;
          moonbit_string_t* buf$2434;
          moonbit_string_t _tmp$2457;
          moonbit_string_t name$875;
          int32_t len$2431;
          moonbit_string_t* _field$2456;
          int32_t _cnt$2697;
          moonbit_string_t* buf$2432;
          moonbit_string_t _tmp$2455;
          moonbit_string_t cmd$876;
          struct $$3c$String$2a$String$3e$** _tmp$2430;
          struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* params$877;
          struct $$3c$String$2a$String$3e$** _tmp$2429;
          struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* returns$878;
          moonbit_string_t* _tmp$2428;
          struct $$moonbitlang$core$builtin$Array$3c$String$3e$* sql_lines$879;
          int32_t _tmp$2384;
          moonbit_string_t sql$892;
          struct $QueryDef* _tmp$2427;
          if (0 >= len$2433) {
            moonbit_panic();
          }
          _field$2458 = parts$874->$0;
          buf$2434 = _field$2458;
          _tmp$2457 = (moonbit_string_t)buf$2434[0];
          name$875 = _tmp$2457;
          len$2431 = parts$874->$1;
          if (1 >= len$2431) {
            moonbit_panic();
          }
          _field$2456 = parts$874->$0;
          moonbit_incref(name$875);
          _cnt$2697 = Moonbit_object_header(parts$874)->rc;
          if (_cnt$2697 > 1) {
            int32_t _new_cnt$2698 = _cnt$2697 - 1;
            Moonbit_object_header(parts$874)->rc = _new_cnt$2698;
            moonbit_incref(_field$2456);
          } else if (_cnt$2697 == 1) {
            moonbit_free(parts$874);
          }
          buf$2432 = _field$2456;
          _tmp$2455 = (moonbit_string_t)buf$2432[1];
          moonbit_incref(_tmp$2455);
          moonbit_decref(buf$2432);
          cmd$876 = _tmp$2455;
          _tmp$2430
          = (struct $$3c$String$2a$String$3e$**)moonbit_empty_ref_array;
          params$877
          = (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$*)moonbit_malloc(
              sizeof(
                struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$
              )
            );
          Moonbit_object_header(params$877)->meta
          = Moonbit_make_regular_object_header(
            offsetof(
              struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$,
                $0
            )
            >> 2,
              1,
              0
          );
          params$877->$0 = _tmp$2430;
          params$877->$1 = 0;
          _tmp$2429
          = (struct $$3c$String$2a$String$3e$**)moonbit_empty_ref_array;
          returns$878
          = (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$*)moonbit_malloc(
              sizeof(
                struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$
              )
            );
          Moonbit_object_header(returns$878)->meta
          = Moonbit_make_regular_object_header(
            offsetof(
              struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$,
                $0
            )
            >> 2,
              1,
              0
          );
          returns$878->$0 = _tmp$2429;
          returns$878->$1 = 0;
          _tmp$2428 = (moonbit_string_t*)moonbit_empty_ref_array;
          sql_lines$879
          = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
              sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
            );
          Moonbit_object_header(sql_lines$879)->meta
          = Moonbit_make_regular_object_header(
            offsetof(
              struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
            )
            >> 2,
              1,
              0
          );
          sql_lines$879->$0 = _tmp$2428;
          sql_lines$879->$1 = 0;
          _tmp$2384 = i$871;
          i$871 = _tmp$2384 + 1;
          while (1) {
            int32_t _tmp$2385 = i$871;
            int32_t len$2386 = lines$869->$1;
            if (_tmp$2385 < len$2386) {
              int32_t _bind$881 = i$871;
              int32_t _if_result$2780;
              moonbit_string_t* _field$2454;
              moonbit_string_t* buf$2426;
              moonbit_string_t _tmp$2453;
              moonbit_string_t _tmp$2423;
              void* None$2424;
              struct $StringView _tmp$2422;
              moonbit_string_t next_line$880;
              int32_t _tmp$2388;
              struct $StringView _tmp$2387;
              int32_t _tmp$2421;
              if (_bind$881 < 0) {
                _if_result$2780 = 1;
              } else {
                int32_t len$2425 = lines$869->$1;
                _if_result$2780 = _bind$881 >= len$2425;
              }
              if (_if_result$2780) {
                moonbit_panic();
              }
              _field$2454 = lines$869->$0;
              buf$2426 = _field$2454;
              _tmp$2453 = (moonbit_string_t)buf$2426[_bind$881];
              _tmp$2423 = _tmp$2453;
              None$2424
              = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
              moonbit_incref(_tmp$2423);
              _tmp$2422 = $String$$trim(_tmp$2423, None$2424);
              next_line$880
              = $$moonbitlang$core$builtin$Show$$StringView$$to_string(
                _tmp$2422
              );
              _tmp$2388
              = Moonbit_array_length(
                $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$210
              );
              moonbit_incref(
                $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$210
              );
              _tmp$2387
              = (struct $StringView){
                0,
                  _tmp$2388,
                  $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$210
              };
              moonbit_incref(next_line$880);
              if ($String$$has_prefix(next_line$880, _tmp$2387)) {
                moonbit_string_t param_str$882 =
                  $sqlc_gen_moonbit$tools$codegen$substr(next_line$880, 10);
                struct $$moonbitlang$core$builtin$Array$3c$String$3e$* param_parts$883 =
                  $sqlc_gen_moonbit$tools$codegen$split_whitespace(
                    param_str$882
                  );
                int32_t len$2389 = param_parts$883->$1;
                if (len$2389 >= 2) {
                  int32_t len$2395 = param_parts$883->$1;
                  moonbit_string_t* _field$2446;
                  moonbit_string_t* buf$2396;
                  moonbit_string_t _tmp$2445;
                  moonbit_string_t _tmp$2391;
                  int32_t len$2393;
                  moonbit_string_t* _field$2444;
                  int32_t _cnt$2699;
                  moonbit_string_t* buf$2394;
                  moonbit_string_t _tmp$2443;
                  moonbit_string_t _tmp$2392;
                  struct $$3c$String$2a$String$3e$* _tuple$2390;
                  if (0 >= len$2395) {
                    moonbit_panic();
                  }
                  _field$2446 = param_parts$883->$0;
                  buf$2396 = _field$2446;
                  _tmp$2445 = (moonbit_string_t)buf$2396[0];
                  _tmp$2391 = _tmp$2445;
                  len$2393 = param_parts$883->$1;
                  if (1 >= len$2393) {
                    moonbit_panic();
                  }
                  _field$2444 = param_parts$883->$0;
                  moonbit_incref(_tmp$2391);
                  _cnt$2699 = Moonbit_object_header(param_parts$883)->rc;
                  if (_cnt$2699 > 1) {
                    int32_t _new_cnt$2700 = _cnt$2699 - 1;
                    Moonbit_object_header(param_parts$883)->rc
                    = _new_cnt$2700;
                    moonbit_incref(_field$2444);
                  } else if (_cnt$2699 == 1) {
                    moonbit_free(param_parts$883);
                  }
                  buf$2394 = _field$2444;
                  _tmp$2443 = (moonbit_string_t)buf$2394[1];
                  moonbit_incref(_tmp$2443);
                  moonbit_decref(buf$2394);
                  _tmp$2392 = _tmp$2443;
                  _tuple$2390
                  = (struct $$3c$String$2a$String$3e$*)moonbit_malloc(
                      sizeof(struct $$3c$String$2a$String$3e$)
                    );
                  Moonbit_object_header(_tuple$2390)->meta
                  = Moonbit_make_regular_object_header(
                    offsetof(struct $$3c$String$2a$String$3e$, $0) >> 2, 2, 0
                  );
                  _tuple$2390->$0 = _tmp$2391;
                  _tuple$2390->$1 = _tmp$2392;
                  moonbit_incref(params$877);
                  $$moonbitlang$core$builtin$Array$$push$0(
                    params$877, _tuple$2390
                  );
                } else {
                  moonbit_decref(param_parts$883);
                }
              } else {
                int32_t _tmp$2398 =
                  Moonbit_array_length(
                    $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$213
                  );
                struct $StringView _tmp$2397;
                moonbit_incref(
                  $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$213
                );
                _tmp$2397
                = (struct $StringView){
                  0,
                    _tmp$2398,
                    $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$213
                };
                moonbit_incref(next_line$880);
                if ($String$$has_prefix(next_line$880, _tmp$2397)) {
                  moonbit_string_t ret_str$884 =
                    $sqlc_gen_moonbit$tools$codegen$substr(next_line$880, 12);
                  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* cols$885 =
                    $sqlc_gen_moonbit$tools$codegen$split_by_comma(
                      ret_str$884
                    );
                  int32_t _len$886 = cols$885->$1;
                  int32_t _i$887 = 0;
                  while (1) {
                    if (_i$887 < _len$886) {
                      moonbit_string_t* _field$2452 = cols$885->$0;
                      moonbit_string_t* buf$2410 = _field$2452;
                      moonbit_string_t _tmp$2451 =
                        (moonbit_string_t)buf$2410[_i$887];
                      moonbit_string_t col$888 = _tmp$2451;
                      void* None$2409 =
                        (struct moonbit_object*)&moonbit_constant_constructor_0
                        + 1;
                      struct $StringView _tmp$2408;
                      moonbit_string_t _tmp$2407;
                      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* col_parts$889;
                      int32_t len$2399;
                      int32_t _tmp$2411;
                      moonbit_incref(col$888);
                      _tmp$2408 = $String$$trim(col$888, None$2409);
                      _tmp$2407
                      = $$moonbitlang$core$builtin$Show$$StringView$$to_string(
                        _tmp$2408
                      );
                      col_parts$889
                      = $sqlc_gen_moonbit$tools$codegen$split_whitespace(
                        _tmp$2407
                      );
                      len$2399 = col_parts$889->$1;
                      if (len$2399 >= 2) {
                        int32_t len$2405 = col_parts$889->$1;
                        moonbit_string_t* _field$2450;
                        moonbit_string_t* buf$2406;
                        moonbit_string_t _tmp$2449;
                        moonbit_string_t _tmp$2401;
                        int32_t len$2403;
                        moonbit_string_t* _field$2448;
                        int32_t _cnt$2701;
                        moonbit_string_t* buf$2404;
                        moonbit_string_t _tmp$2447;
                        moonbit_string_t _tmp$2402;
                        struct $$3c$String$2a$String$3e$* _tuple$2400;
                        if (0 >= len$2405) {
                          moonbit_panic();
                        }
                        _field$2450 = col_parts$889->$0;
                        buf$2406 = _field$2450;
                        _tmp$2449 = (moonbit_string_t)buf$2406[0];
                        _tmp$2401 = _tmp$2449;
                        len$2403 = col_parts$889->$1;
                        if (1 >= len$2403) {
                          moonbit_panic();
                        }
                        _field$2448 = col_parts$889->$0;
                        moonbit_incref(_tmp$2401);
                        _cnt$2701 = Moonbit_object_header(col_parts$889)->rc;
                        if (_cnt$2701 > 1) {
                          int32_t _new_cnt$2702 = _cnt$2701 - 1;
                          Moonbit_object_header(col_parts$889)->rc
                          = _new_cnt$2702;
                          moonbit_incref(_field$2448);
                        } else if (_cnt$2701 == 1) {
                          moonbit_free(col_parts$889);
                        }
                        buf$2404 = _field$2448;
                        _tmp$2447 = (moonbit_string_t)buf$2404[1];
                        moonbit_incref(_tmp$2447);
                        moonbit_decref(buf$2404);
                        _tmp$2402 = _tmp$2447;
                        _tuple$2400
                        = (struct $$3c$String$2a$String$3e$*)moonbit_malloc(
                            sizeof(struct $$3c$String$2a$String$3e$)
                          );
                        Moonbit_object_header(_tuple$2400)->meta
                        = Moonbit_make_regular_object_header(
                          offsetof(struct $$3c$String$2a$String$3e$, $0) >> 2,
                            2,
                            0
                        );
                        _tuple$2400->$0 = _tmp$2401;
                        _tuple$2400->$1 = _tmp$2402;
                        moonbit_incref(returns$878);
                        $$moonbitlang$core$builtin$Array$$push$0(
                          returns$878, _tuple$2400
                        );
                      } else {
                        moonbit_decref(col_parts$889);
                      }
                      _tmp$2411 = _i$887 + 1;
                      _i$887 = _tmp$2411;
                      continue;
                    } else {
                      moonbit_decref(cols$885);
                    }
                    break;
                  }
                } else {
                  int32_t _tmp$2413 =
                    Moonbit_array_length(
                      $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$218
                    );
                  struct $StringView _tmp$2412;
                  moonbit_incref(
                    $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$218
                  );
                  _tmp$2412
                  = (struct $StringView){
                    0,
                      _tmp$2413,
                      $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$218
                  };
                  moonbit_incref(next_line$880);
                  if ($String$$has_prefix(next_line$880, _tmp$2412)) {
                    moonbit_decref(next_line$880);
                    break;
                  } else {
                    int32_t _tmp$2417 = Moonbit_array_length(next_line$880);
                    int32_t _if_result$2782;
                    if (_tmp$2417 > 0) {
                      int32_t _tmp$2416 =
                        Moonbit_array_length(
                          $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$219
                        );
                      struct $StringView _tmp$2415;
                      int32_t _tmp$2414;
                      moonbit_incref(
                        $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$219
                      );
                      _tmp$2415
                      = (struct $StringView){
                        0,
                          _tmp$2416,
                          $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$219
                      };
                      moonbit_incref(next_line$880);
                      _tmp$2414
                      = $String$$has_prefix(
                        next_line$880, _tmp$2415
                      );
                      _if_result$2782 = !_tmp$2414;
                    } else {
                      _if_result$2782 = 0;
                    }
                    if (_if_result$2782) {
                      int32_t _tmp$2419;
                      struct $StringView _tmp$2418;
                      moonbit_incref(next_line$880);
                      moonbit_incref(sql_lines$879);
                      $$moonbitlang$core$builtin$Array$$push$1(
                        sql_lines$879, next_line$880
                      );
                      _tmp$2419
                      = Moonbit_array_length(
                        $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$220
                      );
                      moonbit_incref(
                        $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$220
                      );
                      _tmp$2418
                      = (struct $StringView){
                        0,
                          _tmp$2419,
                          $sqlc_gen_moonbit$tools$codegen$parse_queries$$2a$bind$7c$220
                      };
                      if ($String$$has_suffix(next_line$880, _tmp$2418)) {
                        int32_t _tmp$2420 = i$871;
                        i$871 = _tmp$2420 + 1;
                        break;
                      }
                    } else {
                      moonbit_decref(next_line$880);
                    }
                  }
                }
              }
              _tmp$2421 = i$871;
              i$871 = _tmp$2421 + 1;
              continue;
            }
            break;
          }
          sql$892
          = $sqlc_gen_moonbit$tools$codegen$join_lines(
            sql_lines$879, (moonbit_string_t)moonbit_string_literal_6.data
          );
          _tmp$2427
          = (struct $QueryDef*)moonbit_malloc(sizeof(struct $QueryDef));
          Moonbit_object_header(_tmp$2427)->meta
          = Moonbit_make_regular_object_header(
            offsetof(struct $QueryDef, $0) >> 2, 5, 0
          );
          _tmp$2427->$0 = name$875;
          _tmp$2427->$1 = cmd$876;
          _tmp$2427->$2 = params$877;
          _tmp$2427->$3 = returns$878;
          _tmp$2427->$4 = sql$892;
          moonbit_incref(queries$868);
          $$moonbitlang$core$builtin$Array$$push$2(queries$868, _tmp$2427);
          continue;
        } else {
          moonbit_decref(parts$874);
        }
      } else {
        moonbit_decref(line$872);
      }
      _tmp$2436 = i$871;
      i$871 = _tmp$2436 + 1;
      continue;
    } else {
      moonbit_decref(lines$869);
    }
    break;
  }
  return queries$868;
}

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$substr(
  moonbit_string_t s$864,
  int32_t start$867
) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$862 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* chars$863 =
    $String$$to_array(s$864);
  int32_t i$865 = start$867;
  while (1) {
    int32_t len$2374 = chars$863->$1;
    if (i$865 < len$2374) {
      int32_t _if_result$2784;
      int32_t* _field$2462;
      int32_t* buf$2377;
      int32_t _tmp$2461;
      int32_t _tmp$2375;
      int32_t _tmp$2378;
      if (i$865 < 0) {
        _if_result$2784 = 1;
      } else {
        int32_t len$2376 = chars$863->$1;
        _if_result$2784 = i$865 >= len$2376;
      }
      if (_if_result$2784) {
        moonbit_panic();
      }
      _field$2462 = chars$863->$0;
      buf$2377 = _field$2462;
      _tmp$2461 = (int32_t)buf$2377[i$865];
      _tmp$2375 = _tmp$2461;
      moonbit_incref(buf$862);
      $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
        buf$862, _tmp$2375
      );
      _tmp$2378 = i$865 + 1;
      i$865 = _tmp$2378;
      continue;
    } else {
      moonbit_decref(chars$863);
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$862);
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $sqlc_gen_moonbit$tools$codegen$split_whitespace(
  moonbit_string_t s$857
) {
  moonbit_string_t* _tmp$2373 = (moonbit_string_t*)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* parts$854 =
    (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  struct $$moonbitlang$core$builtin$StringBuilder* buf$855;
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* _it$856;
  moonbit_string_t _tmp$2371;
  int32_t _tmp$2463;
  int32_t _tmp$2370;
  Moonbit_object_header(parts$854)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  parts$854->$0 = _tmp$2373;
  parts$854->$1 = 0;
  buf$855 = $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  _it$856 = $String$$iter(s$857);
  while (1) {
    int32_t _bind$858;
    moonbit_incref(_it$856);
    _bind$858 = $$moonbitlang$core$builtin$Iter$$next$0(_it$856);
    if (_bind$858 == -1) {
      moonbit_decref(_it$856);
    } else {
      int32_t _Some$859 = _bind$858;
      int32_t _c$860 = _Some$859;
      if (_c$860 == 32 || _c$860 == 9) {
        moonbit_string_t _tmp$2368;
        int32_t _tmp$2464;
        int32_t _tmp$2367;
        moonbit_incref(buf$855);
        _tmp$2368
        = $$moonbitlang$core$builtin$StringBuilder$$to_string(
          buf$855
        );
        _tmp$2464 = Moonbit_array_length(_tmp$2368);
        moonbit_decref(_tmp$2368);
        _tmp$2367 = _tmp$2464;
        if (_tmp$2367 > 0) {
          moonbit_string_t _tmp$2369;
          moonbit_incref(buf$855);
          _tmp$2369
          = $$moonbitlang$core$builtin$StringBuilder$$to_string(
            buf$855
          );
          moonbit_incref(parts$854);
          $$moonbitlang$core$builtin$Array$$push$1(parts$854, _tmp$2369);
          moonbit_incref(buf$855);
          $$moonbitlang$core$builtin$StringBuilder$$reset(buf$855);
        }
      } else {
        moonbit_incref(buf$855);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
          buf$855, _c$860
        );
      }
      continue;
    }
    break;
  }
  moonbit_incref(buf$855);
  _tmp$2371 = $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$855);
  _tmp$2463 = Moonbit_array_length(_tmp$2371);
  moonbit_decref(_tmp$2371);
  _tmp$2370 = _tmp$2463;
  if (_tmp$2370 > 0) {
    moonbit_string_t _tmp$2372 =
      $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$855);
    moonbit_incref(parts$854);
    $$moonbitlang$core$builtin$Array$$push$1(parts$854, _tmp$2372);
  } else {
    moonbit_decref(buf$855);
  }
  return parts$854;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $sqlc_gen_moonbit$tools$codegen$split_lines(
  moonbit_string_t s$849
) {
  moonbit_string_t* _tmp$2366 = (moonbit_string_t*)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* lines$846 =
    (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  struct $$moonbitlang$core$builtin$StringBuilder* buf$847;
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* _it$848;
  moonbit_string_t _tmp$2364;
  int32_t _tmp$2465;
  int32_t _tmp$2363;
  Moonbit_object_header(lines$846)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  lines$846->$0 = _tmp$2366;
  lines$846->$1 = 0;
  buf$847 = $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  _it$848 = $String$$iter(s$849);
  while (1) {
    int32_t _bind$850;
    moonbit_incref(_it$848);
    _bind$850 = $$moonbitlang$core$builtin$Iter$$next$0(_it$848);
    if (_bind$850 == -1) {
      moonbit_decref(_it$848);
    } else {
      int32_t _Some$851 = _bind$850;
      int32_t _c$852 = _Some$851;
      if (_c$852 == 10) {
        moonbit_string_t _tmp$2362;
        moonbit_incref(buf$847);
        _tmp$2362
        = $$moonbitlang$core$builtin$StringBuilder$$to_string(
          buf$847
        );
        moonbit_incref(lines$846);
        $$moonbitlang$core$builtin$Array$$push$1(lines$846, _tmp$2362);
        moonbit_incref(buf$847);
        $$moonbitlang$core$builtin$StringBuilder$$reset(buf$847);
      } else if (_c$852 != 13) {
        moonbit_incref(buf$847);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
          buf$847, _c$852
        );
      }
      continue;
    }
    break;
  }
  moonbit_incref(buf$847);
  _tmp$2364 = $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$847);
  _tmp$2465 = Moonbit_array_length(_tmp$2364);
  moonbit_decref(_tmp$2364);
  _tmp$2363 = _tmp$2465;
  if (_tmp$2363 > 0) {
    moonbit_string_t _tmp$2365 =
      $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$847);
    moonbit_incref(lines$846);
    $$moonbitlang$core$builtin$Array$$push$1(lines$846, _tmp$2365);
  } else {
    moonbit_decref(buf$847);
  }
  return lines$846;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $sqlc_gen_moonbit$tools$codegen$split_by_comma(
  moonbit_string_t s$841
) {
  moonbit_string_t* _tmp$2361 = (moonbit_string_t*)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* parts$838 =
    (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  struct $$moonbitlang$core$builtin$StringBuilder* buf$839;
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* _it$840;
  moonbit_string_t _tmp$2359;
  int32_t _tmp$2466;
  int32_t _tmp$2358;
  Moonbit_object_header(parts$838)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  parts$838->$0 = _tmp$2361;
  parts$838->$1 = 0;
  buf$839 = $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  _it$840 = $String$$iter(s$841);
  while (1) {
    int32_t _bind$842;
    moonbit_incref(_it$840);
    _bind$842 = $$moonbitlang$core$builtin$Iter$$next$0(_it$840);
    if (_bind$842 == -1) {
      moonbit_decref(_it$840);
    } else {
      int32_t _Some$843 = _bind$842;
      int32_t _c$844 = _Some$843;
      if (_c$844 == 44) {
        moonbit_string_t _tmp$2357;
        moonbit_incref(buf$839);
        _tmp$2357
        = $$moonbitlang$core$builtin$StringBuilder$$to_string(
          buf$839
        );
        moonbit_incref(parts$838);
        $$moonbitlang$core$builtin$Array$$push$1(parts$838, _tmp$2357);
        moonbit_incref(buf$839);
        $$moonbitlang$core$builtin$StringBuilder$$reset(buf$839);
      } else {
        moonbit_incref(buf$839);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
          buf$839, _c$844
        );
      }
      continue;
    }
    break;
  }
  moonbit_incref(buf$839);
  _tmp$2359 = $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$839);
  _tmp$2466 = Moonbit_array_length(_tmp$2359);
  moonbit_decref(_tmp$2359);
  _tmp$2358 = _tmp$2466;
  if (_tmp$2358 > 0) {
    moonbit_string_t _tmp$2360 =
      $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$839);
    moonbit_incref(parts$838);
    $$moonbitlang$core$builtin$Array$$push$1(parts$838, _tmp$2360);
  } else {
    moonbit_decref(buf$839);
  }
  return parts$838;
}

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$join_lines(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* lines$833,
  moonbit_string_t sep$836
) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$831 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t _len$832 = lines$833->$1;
  int32_t _i$834 = 0;
  while (1) {
    if (_i$834 < _len$832) {
      moonbit_string_t* _field$2468 = lines$833->$0;
      moonbit_string_t* buf$2355 = _field$2468;
      moonbit_string_t _tmp$2467 = (moonbit_string_t)buf$2355[_i$834];
      moonbit_string_t line$835 = _tmp$2467;
      int32_t _tmp$2356;
      if (_i$834 > 0) {
        moonbit_incref(sep$836);
        moonbit_incref(line$835);
        moonbit_incref(buf$831);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$831, sep$836
        );
      } else {
        moonbit_incref(line$835);
      }
      moonbit_incref(buf$831);
      $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
        buf$831, line$835
      );
      _tmp$2356 = _i$834 + 1;
      _i$834 = _tmp$2356;
      continue;
    } else {
      moonbit_decref(sep$836);
      moonbit_decref(lines$833);
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$831);
}

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$generate_code(
  struct $$moonbitlang$core$builtin$Array$3c$QueryDef$3e$* queries$827
) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$825 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t _len$826;
  int32_t _i$828;
  moonbit_incref(buf$825);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$825, (moonbit_string_t)moonbit_string_literal_7.data
  );
  moonbit_incref(buf$825);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$825, (moonbit_string_t)moonbit_string_literal_8.data
  );
  moonbit_incref(buf$825);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$825, (moonbit_string_t)moonbit_string_literal_9.data
  );
  moonbit_incref(buf$825);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$825, (moonbit_string_t)moonbit_string_literal_10.data
  );
  moonbit_incref(buf$825);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$825, (moonbit_string_t)moonbit_string_literal_11.data
  );
  moonbit_incref(buf$825);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$825, (moonbit_string_t)moonbit_string_literal_12.data
  );
  moonbit_incref(buf$825);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$825, (moonbit_string_t)moonbit_string_literal_13.data
  );
  moonbit_incref(buf$825);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$825, (moonbit_string_t)moonbit_string_literal_10.data
  );
  _len$826 = queries$827->$1;
  _i$828 = 0;
  while (1) {
    if (_i$828 < _len$826) {
      struct $QueryDef** _field$2470 = queries$827->$0;
      struct $QueryDef** buf$2353 = _field$2470;
      struct $QueryDef* _tmp$2469 = (struct $QueryDef*)buf$2353[_i$828];
      struct $QueryDef* query$829 = _tmp$2469;
      int32_t _tmp$2354;
      moonbit_incref(query$829);
      moonbit_incref(buf$825);
      $sqlc_gen_moonbit$tools$codegen$generate_query(buf$825, query$829);
      _tmp$2354 = _i$828 + 1;
      _i$828 = _tmp$2354;
      continue;
    } else {
      moonbit_decref(queries$827);
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$825);
}

int32_t $sqlc_gen_moonbit$tools$codegen$generate_query(
  struct $$moonbitlang$core$builtin$StringBuilder* buf$783,
  struct $QueryDef* query$780
) {
  moonbit_string_t _field$2521 = query$780->$0;
  moonbit_string_t name$2352 = _field$2521;
  moonbit_string_t func_name$779;
  moonbit_string_t _field$2520;
  moonbit_string_t name$2351;
  moonbit_string_t row_type$781;
  moonbit_string_t _field$2519;
  moonbit_string_t name$2350;
  moonbit_string_t params_type$782;
  moonbit_string_t _field$2518;
  moonbit_string_t cmd$2300;
  int32_t _tmp$2517;
  int32_t _if_result$2790;
  int32_t _if_result$2791;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$2507;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* params$2308;
  int32_t _field$2506;
  int32_t len$2307;
  moonbit_string_t sql_const$804;
  moonbit_string_t _field$2491;
  moonbit_string_t sql$2326;
  moonbit_string_t _tmp$2325;
  moonbit_string_t _field$2490;
  moonbit_string_t _bind$805;
  moonbit_string_t return_type$806;
  moonbit_string_t _field$2489;
  moonbit_string_t name$2327;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$2488;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* params$2329;
  int32_t _field$2487;
  int32_t len$2328;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$2486;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _arr$807;
  int32_t _len$808;
  int32_t _i$809;
  moonbit_string_t _field$2481;
  moonbit_string_t _bind$814;
  moonbit_incref(name$2352);
  func_name$779 = $sqlc_gen_moonbit$tools$codegen$to_snake_case(name$2352);
  _field$2520 = query$780->$0;
  name$2351 = _field$2520;
  moonbit_incref(name$2351);
  row_type$781
  = moonbit_add_string(
    name$2351, (moonbit_string_t)moonbit_string_literal_14.data
  );
  _field$2519 = query$780->$0;
  name$2350 = _field$2519;
  moonbit_incref(name$2350);
  params_type$782
  = moonbit_add_string(
    name$2350, (moonbit_string_t)moonbit_string_literal_15.data
  );
  _field$2518 = query$780->$1;
  cmd$2300 = _field$2518;
  _tmp$2517
  = moonbit_val_array_equal(
    cmd$2300, (moonbit_string_t)moonbit_string_literal_16.data
  );
  if (_tmp$2517) {
    _if_result$2790 = 1;
  } else {
    moonbit_string_t _field$2516 = query$780->$1;
    moonbit_string_t cmd$2299 = _field$2516;
    int32_t _tmp$2515 =
      moonbit_val_array_equal(
        cmd$2299, (moonbit_string_t)moonbit_string_literal_17.data
      );
    _if_result$2790 = _tmp$2515;
  }
  if (_if_result$2790) {
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$2514 =
      query$780->$3;
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* returns$2298 =
      _field$2514;
    int32_t _field$2513 = returns$2298->$1;
    int32_t len$2297 = _field$2513;
    _if_result$2791 = len$2297 > 0;
  } else {
    _if_result$2791 = 0;
  }
  if (_if_result$2791) {
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$2512;
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _arr$784;
    int32_t _len$785;
    int32_t _i$786;
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_18.data
    );
    moonbit_incref(buf$783);
    moonbit_incref(row_type$781);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, row_type$781
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_19.data
    );
    _field$2512 = query$780->$3;
    _arr$784 = _field$2512;
    _len$785 = _arr$784->$1;
    moonbit_incref(_arr$784);
    _i$786 = 0;
    while (1) {
      if (_i$786 < _len$785) {
        struct $$3c$String$2a$String$3e$** _field$2511 = _arr$784->$0;
        struct $$3c$String$2a$String$3e$** buf$2305 = _field$2511;
        struct $$3c$String$2a$String$3e$* _tmp$2510 =
          (struct $$3c$String$2a$String$3e$*)buf$2305[_i$786];
        struct $$3c$String$2a$String$3e$* ret$787 = _tmp$2510;
        moonbit_string_t _field$2509;
        moonbit_string_t _tmp$2302;
        moonbit_string_t _tmp$2301;
        moonbit_string_t _field$2508;
        int32_t _cnt$2703;
        moonbit_string_t _tmp$2304;
        moonbit_string_t _tmp$2303;
        int32_t _tmp$2306;
        moonbit_incref(ret$787);
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, (moonbit_string_t)moonbit_string_literal_20.data
        );
        _field$2509 = ret$787->$0;
        _tmp$2302 = _field$2509;
        moonbit_incref(_tmp$2302);
        _tmp$2301 = $sqlc_gen_moonbit$tools$codegen$to_snake_case(_tmp$2302);
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, _tmp$2301
        );
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, (moonbit_string_t)moonbit_string_literal_21.data
        );
        _field$2508 = ret$787->$1;
        _cnt$2703 = Moonbit_object_header(ret$787)->rc;
        if (_cnt$2703 > 1) {
          int32_t _new_cnt$2705 = _cnt$2703 - 1;
          Moonbit_object_header(ret$787)->rc = _new_cnt$2705;
          moonbit_incref(_field$2508);
        } else if (_cnt$2703 == 1) {
          moonbit_string_t _field$2704 = ret$787->$0;
          moonbit_decref(_field$2704);
          moonbit_free(ret$787);
        }
        _tmp$2304 = _field$2508;
        _tmp$2303
        = $sqlc_gen_moonbit$tools$codegen$sql_type_to_moonbit(
          _tmp$2304
        );
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, _tmp$2303
        );
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, (moonbit_string_t)moonbit_string_literal_22.data
        );
        _tmp$2306 = _i$786 + 1;
        _i$786 = _tmp$2306;
        continue;
      } else {
        moonbit_decref(_arr$784);
      }
      break;
    }
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_23.data
    );
  }
  _field$2507 = query$780->$2;
  params$2308 = _field$2507;
  _field$2506 = params$2308->$1;
  len$2307 = _field$2506;
  if (len$2307 > 0) {
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$2505;
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _arr$789;
    int32_t _len$790;
    int32_t _i$791;
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$2500;
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _arr$794;
    int32_t _len$795;
    int32_t _i$796;
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$2495;
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _arr$799;
    int32_t _len$800;
    int32_t _i$801;
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_18.data
    );
    moonbit_incref(buf$783);
    moonbit_incref(params_type$782);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, params_type$782
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_19.data
    );
    _field$2505 = query$780->$2;
    _arr$789 = _field$2505;
    _len$790 = _arr$789->$1;
    moonbit_incref(_arr$789);
    _i$791 = 0;
    while (1) {
      if (_i$791 < _len$790) {
        struct $$3c$String$2a$String$3e$** _field$2504 = _arr$789->$0;
        struct $$3c$String$2a$String$3e$** buf$2313 = _field$2504;
        struct $$3c$String$2a$String$3e$* _tmp$2503 =
          (struct $$3c$String$2a$String$3e$*)buf$2313[_i$791];
        struct $$3c$String$2a$String$3e$* param$792 = _tmp$2503;
        moonbit_string_t _field$2502;
        moonbit_string_t _tmp$2310;
        moonbit_string_t _tmp$2309;
        moonbit_string_t _field$2501;
        int32_t _cnt$2706;
        moonbit_string_t _tmp$2312;
        moonbit_string_t _tmp$2311;
        int32_t _tmp$2314;
        moonbit_incref(param$792);
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, (moonbit_string_t)moonbit_string_literal_20.data
        );
        _field$2502 = param$792->$0;
        _tmp$2310 = _field$2502;
        moonbit_incref(_tmp$2310);
        _tmp$2309 = $sqlc_gen_moonbit$tools$codegen$to_snake_case(_tmp$2310);
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, _tmp$2309
        );
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, (moonbit_string_t)moonbit_string_literal_21.data
        );
        _field$2501 = param$792->$1;
        _cnt$2706 = Moonbit_object_header(param$792)->rc;
        if (_cnt$2706 > 1) {
          int32_t _new_cnt$2708 = _cnt$2706 - 1;
          Moonbit_object_header(param$792)->rc = _new_cnt$2708;
          moonbit_incref(_field$2501);
        } else if (_cnt$2706 == 1) {
          moonbit_string_t _field$2707 = param$792->$0;
          moonbit_decref(_field$2707);
          moonbit_free(param$792);
        }
        _tmp$2312 = _field$2501;
        _tmp$2311
        = $sqlc_gen_moonbit$tools$codegen$sql_type_to_moonbit(
          _tmp$2312
        );
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, _tmp$2311
        );
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, (moonbit_string_t)moonbit_string_literal_22.data
        );
        _tmp$2314 = _i$791 + 1;
        _i$791 = _tmp$2314;
        continue;
      } else {
        moonbit_decref(_arr$789);
      }
      break;
    }
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_23.data
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_24.data
    );
    moonbit_incref(buf$783);
    moonbit_incref(params_type$782);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, params_type$782
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_25.data
    );
    _field$2500 = query$780->$2;
    _arr$794 = _field$2500;
    _len$795 = _arr$794->$1;
    moonbit_incref(_arr$794);
    _i$796 = 0;
    while (1) {
      if (_i$796 < _len$795) {
        struct $$3c$String$2a$String$3e$** _field$2499 = _arr$794->$0;
        struct $$3c$String$2a$String$3e$** buf$2319 = _field$2499;
        struct $$3c$String$2a$String$3e$* _tmp$2498 =
          (struct $$3c$String$2a$String$3e$*)buf$2319[_i$796];
        struct $$3c$String$2a$String$3e$* param$797 = _tmp$2498;
        moonbit_string_t _field$2497;
        moonbit_string_t _tmp$2316;
        moonbit_string_t _tmp$2315;
        moonbit_string_t _field$2496;
        int32_t _cnt$2709;
        moonbit_string_t _tmp$2318;
        moonbit_string_t _tmp$2317;
        int32_t _tmp$2320;
        if (_i$796 > 0) {
          moonbit_incref(param$797);
          moonbit_incref(buf$783);
          $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
            buf$783, (moonbit_string_t)moonbit_string_literal_26.data
          );
        } else {
          moonbit_incref(param$797);
        }
        _field$2497 = param$797->$0;
        _tmp$2316 = _field$2497;
        moonbit_incref(_tmp$2316);
        _tmp$2315 = $sqlc_gen_moonbit$tools$codegen$to_snake_case(_tmp$2316);
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, _tmp$2315
        );
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, (moonbit_string_t)moonbit_string_literal_21.data
        );
        _field$2496 = param$797->$1;
        _cnt$2709 = Moonbit_object_header(param$797)->rc;
        if (_cnt$2709 > 1) {
          int32_t _new_cnt$2711 = _cnt$2709 - 1;
          Moonbit_object_header(param$797)->rc = _new_cnt$2711;
          moonbit_incref(_field$2496);
        } else if (_cnt$2709 == 1) {
          moonbit_string_t _field$2710 = param$797->$0;
          moonbit_decref(_field$2710);
          moonbit_free(param$797);
        }
        _tmp$2318 = _field$2496;
        _tmp$2317
        = $sqlc_gen_moonbit$tools$codegen$sql_type_to_moonbit(
          _tmp$2318
        );
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, _tmp$2317
        );
        _tmp$2320 = _i$796 + 1;
        _i$796 = _tmp$2320;
        continue;
      } else {
        moonbit_decref(_arr$794);
      }
      break;
    }
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_27.data
    );
    moonbit_incref(buf$783);
    moonbit_incref(params_type$782);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, params_type$782
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_28.data
    );
    _field$2495 = query$780->$2;
    _arr$799 = _field$2495;
    _len$800 = _arr$799->$1;
    moonbit_incref(_arr$799);
    _i$801 = 0;
    while (1) {
      if (_i$801 < _len$800) {
        struct $$3c$String$2a$String$3e$** _field$2494 = _arr$799->$0;
        struct $$3c$String$2a$String$3e$** buf$2323 = _field$2494;
        struct $$3c$String$2a$String$3e$* _tmp$2493 =
          (struct $$3c$String$2a$String$3e$*)buf$2323[_i$801];
        struct $$3c$String$2a$String$3e$* param$802 = _tmp$2493;
        moonbit_string_t _field$2492;
        int32_t _cnt$2712;
        moonbit_string_t _tmp$2322;
        moonbit_string_t _tmp$2321;
        int32_t _tmp$2324;
        if (_i$801 > 0) {
          moonbit_incref(param$802);
          moonbit_incref(buf$783);
          $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
            buf$783, (moonbit_string_t)moonbit_string_literal_26.data
          );
        } else {
          moonbit_incref(param$802);
        }
        _field$2492 = param$802->$0;
        _cnt$2712 = Moonbit_object_header(param$802)->rc;
        if (_cnt$2712 > 1) {
          int32_t _new_cnt$2714 = _cnt$2712 - 1;
          Moonbit_object_header(param$802)->rc = _new_cnt$2714;
          moonbit_incref(_field$2492);
        } else if (_cnt$2712 == 1) {
          moonbit_string_t _field$2713 = param$802->$1;
          moonbit_decref(_field$2713);
          moonbit_free(param$802);
        }
        _tmp$2322 = _field$2492;
        _tmp$2321 = $sqlc_gen_moonbit$tools$codegen$to_snake_case(_tmp$2322);
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, _tmp$2321
        );
        _tmp$2324 = _i$801 + 1;
        _i$801 = _tmp$2324;
        continue;
      } else {
        moonbit_decref(_arr$799);
      }
      break;
    }
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_29.data
    );
  }
  moonbit_incref(func_name$779);
  sql_const$804
  = moonbit_add_string(
    func_name$779, (moonbit_string_t)moonbit_string_literal_30.data
  );
  moonbit_incref(buf$783);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$783, (moonbit_string_t)moonbit_string_literal_31.data
  );
  moonbit_incref(sql_const$804);
  moonbit_incref(buf$783);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$783, sql_const$804
  );
  moonbit_incref(buf$783);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$783, (moonbit_string_t)moonbit_string_literal_32.data
  );
  _field$2491 = query$780->$4;
  sql$2326 = _field$2491;
  moonbit_incref(sql$2326);
  _tmp$2325 = $sqlc_gen_moonbit$tools$codegen$escape_string(sql$2326);
  moonbit_incref(buf$783);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$783, _tmp$2325
  );
  moonbit_incref(buf$783);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$783, (moonbit_string_t)moonbit_string_literal_33.data
  );
  _field$2490 = query$780->$1;
  _bind$805 = _field$2490;
  if (
    moonbit_val_array_equal(
      _bind$805, (moonbit_string_t)moonbit_string_literal_16.data
    )
  ) {
    moonbit_incref(row_type$781);
    return_type$806
    = moonbit_add_string(
      row_type$781, (moonbit_string_t)moonbit_string_literal_37.data
    );
  } else if (
           moonbit_val_array_equal(
             _bind$805, (moonbit_string_t)moonbit_string_literal_17.data
           )
         ) {
    moonbit_string_t _tmp$2349;
    moonbit_incref(row_type$781);
    _tmp$2349
    = moonbit_add_string(
      (moonbit_string_t)moonbit_string_literal_35.data, row_type$781
    );
    return_type$806
    = moonbit_add_string(
      _tmp$2349, (moonbit_string_t)moonbit_string_literal_36.data
    );
  } else {
    return_type$806 = (moonbit_string_t)moonbit_string_literal_34.data;
  }
  moonbit_incref(buf$783);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$783, (moonbit_string_t)moonbit_string_literal_38.data
  );
  _field$2489 = query$780->$0;
  name$2327 = _field$2489;
  moonbit_incref(name$2327);
  moonbit_incref(buf$783);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$783, name$2327
  );
  moonbit_incref(buf$783);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$783, (moonbit_string_t)moonbit_string_literal_39.data
  );
  moonbit_incref(buf$783);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$783, func_name$779
  );
  moonbit_incref(buf$783);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$783, (moonbit_string_t)moonbit_string_literal_40.data
  );
  _field$2488 = query$780->$2;
  params$2329 = _field$2488;
  _field$2487 = params$2329->$1;
  len$2328 = _field$2487;
  if (len$2328 > 0) {
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_41.data
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, params_type$782
    );
  } else {
    moonbit_decref(params_type$782);
  }
  moonbit_incref(buf$783);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$783, (moonbit_string_t)moonbit_string_literal_27.data
  );
  moonbit_incref(buf$783);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$783, return_type$806
  );
  moonbit_incref(buf$783);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$783, (moonbit_string_t)moonbit_string_literal_19.data
  );
  moonbit_incref(buf$783);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$783, (moonbit_string_t)moonbit_string_literal_42.data
  );
  moonbit_incref(buf$783);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$783, sql_const$804
  );
  moonbit_incref(buf$783);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$783, (moonbit_string_t)moonbit_string_literal_43.data
  );
  _field$2486 = query$780->$2;
  _arr$807 = _field$2486;
  _len$808 = _arr$807->$1;
  moonbit_incref(_arr$807);
  _i$809 = 0;
  while (1) {
    if (_i$809 < _len$808) {
      struct $$3c$String$2a$String$3e$** _field$2485 = _arr$807->$0;
      struct $$3c$String$2a$String$3e$** buf$2335 = _field$2485;
      struct $$3c$String$2a$String$3e$* _tmp$2484 =
        (struct $$3c$String$2a$String$3e$*)buf$2335[_i$809];
      struct $$3c$String$2a$String$3e$* param$810 = _tmp$2484;
      int32_t _tmp$2334 = _i$809 + 1;
      moonbit_string_t idx$811;
      moonbit_string_t _field$2483;
      moonbit_string_t _tmp$2333;
      moonbit_string_t _tmp$2332;
      moonbit_string_t param_name$812;
      moonbit_string_t _field$2482;
      int32_t _cnt$2715;
      moonbit_string_t _tmp$2331;
      moonbit_string_t _tmp$2330;
      int32_t _tmp$2336;
      moonbit_incref(param$810);
      idx$811 = $Int$$to_string$inner(_tmp$2334, 10);
      _field$2483 = param$810->$0;
      _tmp$2333 = _field$2483;
      moonbit_incref(_tmp$2333);
      _tmp$2332 = $sqlc_gen_moonbit$tools$codegen$to_snake_case(_tmp$2333);
      param_name$812
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_44.data, _tmp$2332
      );
      moonbit_incref(buf$783);
      $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
        buf$783, (moonbit_string_t)moonbit_string_literal_20.data
      );
      _field$2482 = param$810->$1;
      _cnt$2715 = Moonbit_object_header(param$810)->rc;
      if (_cnt$2715 > 1) {
        int32_t _new_cnt$2717 = _cnt$2715 - 1;
        Moonbit_object_header(param$810)->rc = _new_cnt$2717;
        moonbit_incref(_field$2482);
      } else if (_cnt$2715 == 1) {
        moonbit_string_t _field$2716 = param$810->$0;
        moonbit_decref(_field$2716);
        moonbit_free(param$810);
      }
      _tmp$2331 = _field$2482;
      _tmp$2330
      = $sqlc_gen_moonbit$tools$codegen$param_to_bind_call(
        _tmp$2331, idx$811, param_name$812
      );
      moonbit_incref(buf$783);
      $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
        buf$783, _tmp$2330
      );
      moonbit_incref(buf$783);
      $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
        buf$783, (moonbit_string_t)moonbit_string_literal_45.data
      );
      _tmp$2336 = _i$809 + 1;
      _i$809 = _tmp$2336;
      continue;
    } else {
      moonbit_decref(_arr$807);
    }
    break;
  }
  _field$2481 = query$780->$1;
  _bind$814 = _field$2481;
  if (
    moonbit_val_array_equal(
      _bind$814, (moonbit_string_t)moonbit_string_literal_16.data
    )
  ) {
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$2475;
    int32_t _cnt$2718;
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _arr$815;
    int32_t _len$816;
    int32_t _i$817;
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_60.data
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_51.data
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, row_type$781
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_52.data
    );
    _field$2475 = query$780->$3;
    _cnt$2718 = Moonbit_object_header(query$780)->rc;
    if (_cnt$2718 > 1) {
      int32_t _new_cnt$2723 = _cnt$2718 - 1;
      Moonbit_object_header(query$780)->rc = _new_cnt$2723;
      moonbit_incref(_field$2475);
    } else if (_cnt$2718 == 1) {
      moonbit_string_t _field$2722 = query$780->$4;
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$2721;
      moonbit_string_t _field$2720;
      moonbit_string_t _field$2719;
      moonbit_decref(_field$2722);
      _field$2721 = query$780->$2;
      moonbit_decref(_field$2721);
      _field$2720 = query$780->$1;
      moonbit_decref(_field$2720);
      _field$2719 = query$780->$0;
      moonbit_decref(_field$2719);
      moonbit_free(query$780);
    }
    _arr$815 = _field$2475;
    _len$816 = _arr$815->$1;
    _i$817 = 0;
    while (1) {
      if (_i$817 < _len$816) {
        struct $$3c$String$2a$String$3e$** _field$2474 = _arr$815->$0;
        struct $$3c$String$2a$String$3e$** buf$2341 = _field$2474;
        struct $$3c$String$2a$String$3e$* _tmp$2473 =
          (struct $$3c$String$2a$String$3e$*)buf$2341[_i$817];
        struct $$3c$String$2a$String$3e$* ret$818 = _tmp$2473;
        moonbit_string_t _field$2472;
        moonbit_string_t _tmp$2338;
        moonbit_string_t _tmp$2337;
        moonbit_string_t _field$2471;
        int32_t _cnt$2724;
        moonbit_string_t _tmp$2340;
        moonbit_string_t _tmp$2339;
        int32_t _tmp$2342;
        moonbit_incref(ret$818);
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, (moonbit_string_t)moonbit_string_literal_53.data
        );
        _field$2472 = ret$818->$0;
        _tmp$2338 = _field$2472;
        moonbit_incref(_tmp$2338);
        _tmp$2337 = $sqlc_gen_moonbit$tools$codegen$to_snake_case(_tmp$2338);
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, _tmp$2337
        );
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, (moonbit_string_t)moonbit_string_literal_54.data
        );
        _field$2471 = ret$818->$1;
        _cnt$2724 = Moonbit_object_header(ret$818)->rc;
        if (_cnt$2724 > 1) {
          int32_t _new_cnt$2726 = _cnt$2724 - 1;
          Moonbit_object_header(ret$818)->rc = _new_cnt$2726;
          moonbit_incref(_field$2471);
        } else if (_cnt$2724 == 1) {
          moonbit_string_t _field$2725 = ret$818->$0;
          moonbit_decref(_field$2725);
          moonbit_free(ret$818);
        }
        _tmp$2340 = _field$2471;
        _tmp$2339
        = $sqlc_gen_moonbit$tools$codegen$column_to_moonbit(
          _tmp$2340, _i$817
        );
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, _tmp$2339
        );
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, (moonbit_string_t)moonbit_string_literal_55.data
        );
        _tmp$2342 = _i$817 + 1;
        _i$817 = _tmp$2342;
        continue;
      } else {
        moonbit_decref(_arr$815);
      }
      break;
    }
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_56.data
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_61.data
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_62.data
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_63.data
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_61.data
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_64.data
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_58.data
    );
  } else if (
           moonbit_val_array_equal(
             _bind$814, (moonbit_string_t)moonbit_string_literal_17.data
           )
         ) {
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$2480;
    int32_t _cnt$2727;
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _arr$820;
    int32_t _len$821;
    int32_t _i$822;
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_48.data
    );
    moonbit_incref(buf$783);
    moonbit_incref(row_type$781);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, row_type$781
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_49.data
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_50.data
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_51.data
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, row_type$781
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_52.data
    );
    _field$2480 = query$780->$3;
    _cnt$2727 = Moonbit_object_header(query$780)->rc;
    if (_cnt$2727 > 1) {
      int32_t _new_cnt$2732 = _cnt$2727 - 1;
      Moonbit_object_header(query$780)->rc = _new_cnt$2732;
      moonbit_incref(_field$2480);
    } else if (_cnt$2727 == 1) {
      moonbit_string_t _field$2731 = query$780->$4;
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$2730;
      moonbit_string_t _field$2729;
      moonbit_string_t _field$2728;
      moonbit_decref(_field$2731);
      _field$2730 = query$780->$2;
      moonbit_decref(_field$2730);
      _field$2729 = query$780->$1;
      moonbit_decref(_field$2729);
      _field$2728 = query$780->$0;
      moonbit_decref(_field$2728);
      moonbit_free(query$780);
    }
    _arr$820 = _field$2480;
    _len$821 = _arr$820->$1;
    _i$822 = 0;
    while (1) {
      if (_i$822 < _len$821) {
        struct $$3c$String$2a$String$3e$** _field$2479 = _arr$820->$0;
        struct $$3c$String$2a$String$3e$** buf$2347 = _field$2479;
        struct $$3c$String$2a$String$3e$* _tmp$2478 =
          (struct $$3c$String$2a$String$3e$*)buf$2347[_i$822];
        struct $$3c$String$2a$String$3e$* ret$823 = _tmp$2478;
        moonbit_string_t _field$2477;
        moonbit_string_t _tmp$2344;
        moonbit_string_t _tmp$2343;
        moonbit_string_t _field$2476;
        int32_t _cnt$2733;
        moonbit_string_t _tmp$2346;
        moonbit_string_t _tmp$2345;
        int32_t _tmp$2348;
        moonbit_incref(ret$823);
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, (moonbit_string_t)moonbit_string_literal_53.data
        );
        _field$2477 = ret$823->$0;
        _tmp$2344 = _field$2477;
        moonbit_incref(_tmp$2344);
        _tmp$2343 = $sqlc_gen_moonbit$tools$codegen$to_snake_case(_tmp$2344);
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, _tmp$2343
        );
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, (moonbit_string_t)moonbit_string_literal_54.data
        );
        _field$2476 = ret$823->$1;
        _cnt$2733 = Moonbit_object_header(ret$823)->rc;
        if (_cnt$2733 > 1) {
          int32_t _new_cnt$2735 = _cnt$2733 - 1;
          Moonbit_object_header(ret$823)->rc = _new_cnt$2735;
          moonbit_incref(_field$2476);
        } else if (_cnt$2733 == 1) {
          moonbit_string_t _field$2734 = ret$823->$0;
          moonbit_decref(_field$2734);
          moonbit_free(ret$823);
        }
        _tmp$2346 = _field$2476;
        _tmp$2345
        = $sqlc_gen_moonbit$tools$codegen$column_to_moonbit(
          _tmp$2346, _i$822
        );
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, _tmp$2345
        );
        moonbit_incref(buf$783);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$783, (moonbit_string_t)moonbit_string_literal_55.data
        );
        _tmp$2348 = _i$822 + 1;
        _i$822 = _tmp$2348;
        continue;
      } else {
        moonbit_decref(_arr$820);
      }
      break;
    }
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_56.data
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_57.data
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_58.data
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_47.data
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_59.data
    );
  } else {
    moonbit_decref(row_type$781);
    moonbit_decref(query$780);
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_46.data
    );
    moonbit_incref(buf$783);
    $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
      buf$783, (moonbit_string_t)moonbit_string_literal_47.data
    );
  }
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    buf$783, (moonbit_string_t)moonbit_string_literal_10.data
  );
  return 0;
}

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$to_snake_case(
  moonbit_string_t s$774
) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$771 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t prev_lower$772 = 0;
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* _it$773 = $String$$iter(s$774);
  while (1) {
    int32_t _bind$775;
    moonbit_incref(_it$773);
    _bind$775 = $$moonbitlang$core$builtin$Iter$$next$0(_it$773);
    if (_bind$775 == -1) {
      moonbit_decref(_it$773);
    } else {
      int32_t _Some$776 = _bind$775;
      int32_t _c$777 = _Some$776;
      if (_c$777 >= 65 && _c$777 <= 90) {
        int32_t _tmp$2296;
        int32_t _tmp$2295;
        int32_t _tmp$2294;
        if (prev_lower$772) {
          moonbit_incref(buf$771);
          $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
            buf$771, 95
          );
        }
        _tmp$2296 = _c$777;
        _tmp$2295 = _tmp$2296 + 32;
        _tmp$2294 = _tmp$2295;
        moonbit_incref(buf$771);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
          buf$771, _tmp$2294
        );
        prev_lower$772 = 0;
      } else {
        moonbit_incref(buf$771);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
          buf$771, _c$777
        );
        prev_lower$772 = _c$777 >= 97 && _c$777 <= 122;
      }
      continue;
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$771);
}

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$sql_type_to_moonbit(
  moonbit_string_t sql_type$770
) {
  moonbit_string_t _bind$769 = $String$$to_upper(sql_type$770);
  if (
    moonbit_val_array_equal(
      _bind$769, (moonbit_string_t)moonbit_string_literal_87.data
    )
  ) {
    moonbit_decref(_bind$769);
    return (moonbit_string_t)moonbit_string_literal_82.data;
  } else if (
           moonbit_val_array_equal(
             _bind$769, (moonbit_string_t)moonbit_string_literal_86.data
           )
         ) {
    moonbit_decref(_bind$769);
    return (moonbit_string_t)moonbit_string_literal_82.data;
  } else if (
           moonbit_val_array_equal(
             _bind$769, (moonbit_string_t)moonbit_string_literal_85.data
           )
         ) {
    moonbit_decref(_bind$769);
    return (moonbit_string_t)moonbit_string_literal_82.data;
  } else if (
           moonbit_val_array_equal(
             _bind$769, (moonbit_string_t)moonbit_string_literal_84.data
           )
         ) {
    moonbit_decref(_bind$769);
    return (moonbit_string_t)moonbit_string_literal_82.data;
  } else if (
           moonbit_val_array_equal(
             _bind$769, (moonbit_string_t)moonbit_string_literal_83.data
           )
         ) {
    moonbit_decref(_bind$769);
    return (moonbit_string_t)moonbit_string_literal_82.data;
  } else if (
           moonbit_val_array_equal(
             _bind$769, (moonbit_string_t)moonbit_string_literal_81.data
           )
         ) {
    moonbit_decref(_bind$769);
    return (moonbit_string_t)moonbit_string_literal_82.data;
  } else if (
           moonbit_val_array_equal(
             _bind$769, (moonbit_string_t)moonbit_string_literal_80.data
           )
         ) {
    moonbit_decref(_bind$769);
    return (moonbit_string_t)moonbit_string_literal_76.data;
  } else if (
           moonbit_val_array_equal(
             _bind$769, (moonbit_string_t)moonbit_string_literal_79.data
           )
         ) {
    moonbit_decref(_bind$769);
    return (moonbit_string_t)moonbit_string_literal_76.data;
  } else if (
           moonbit_val_array_equal(
             _bind$769, (moonbit_string_t)moonbit_string_literal_78.data
           )
         ) {
    moonbit_decref(_bind$769);
    return (moonbit_string_t)moonbit_string_literal_76.data;
  } else if (
           moonbit_val_array_equal(
             _bind$769, (moonbit_string_t)moonbit_string_literal_77.data
           )
         ) {
    moonbit_decref(_bind$769);
    return (moonbit_string_t)moonbit_string_literal_76.data;
  } else if (
           moonbit_val_array_equal(
             _bind$769, (moonbit_string_t)moonbit_string_literal_75.data
           )
         ) {
    moonbit_decref(_bind$769);
    return (moonbit_string_t)moonbit_string_literal_76.data;
  } else if (
           moonbit_val_array_equal(
             _bind$769, (moonbit_string_t)moonbit_string_literal_74.data
           )
         ) {
    moonbit_decref(_bind$769);
    return (moonbit_string_t)moonbit_string_literal_65.data;
  } else if (
           moonbit_val_array_equal(
             _bind$769, (moonbit_string_t)moonbit_string_literal_73.data
           )
         ) {
    moonbit_decref(_bind$769);
    return (moonbit_string_t)moonbit_string_literal_65.data;
  } else if (
           moonbit_val_array_equal(
             _bind$769, (moonbit_string_t)moonbit_string_literal_72.data
           )
         ) {
    moonbit_decref(_bind$769);
    return (moonbit_string_t)moonbit_string_literal_65.data;
  } else if (
           moonbit_val_array_equal(
             _bind$769, (moonbit_string_t)moonbit_string_literal_71.data
           )
         ) {
    moonbit_decref(_bind$769);
    return (moonbit_string_t)moonbit_string_literal_70.data;
  } else if (
           moonbit_val_array_equal(
             _bind$769, (moonbit_string_t)moonbit_string_literal_69.data
           )
         ) {
    moonbit_decref(_bind$769);
    return (moonbit_string_t)moonbit_string_literal_70.data;
  } else if (
           moonbit_val_array_equal(
             _bind$769, (moonbit_string_t)moonbit_string_literal_68.data
           )
         ) {
    moonbit_decref(_bind$769);
    return (moonbit_string_t)moonbit_string_literal_67.data;
  } else if (
           moonbit_val_array_equal(
             _bind$769, (moonbit_string_t)moonbit_string_literal_66.data
           )
         ) {
    moonbit_decref(_bind$769);
    return (moonbit_string_t)moonbit_string_literal_67.data;
  } else {
    moonbit_decref(_bind$769);
    return (moonbit_string_t)moonbit_string_literal_65.data;
  }
}

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$param_to_bind_call(
  moonbit_string_t sql_type$768,
  moonbit_string_t idx$760,
  moonbit_string_t expr$761
) {
  moonbit_string_t _bind$767 = $String$$to_upper(sql_type$768);
  moonbit_string_t _tmp$2290;
  moonbit_string_t _tmp$2289;
  moonbit_string_t _tmp$2288;
  moonbit_string_t _tmp$2287;
  moonbit_string_t _tmp$2286;
  moonbit_string_t _tmp$2285;
  moonbit_string_t _tmp$2284;
  moonbit_string_t _tmp$2283;
  moonbit_string_t _tmp$2282;
  moonbit_string_t _tmp$2281;
  moonbit_string_t _tmp$2280;
  moonbit_string_t _tmp$2279;
  moonbit_string_t _tmp$2278;
  moonbit_string_t _tmp$2277;
  moonbit_string_t _tmp$2276;
  moonbit_string_t _tmp$2275;
  moonbit_string_t _tmp$2274;
  moonbit_string_t _tmp$2273;
  if (
    moonbit_val_array_equal(
      _bind$767, (moonbit_string_t)moonbit_string_literal_87.data
    )
  ) {
    moonbit_decref(_bind$767);
    goto $join$766;
  } else if (
           moonbit_val_array_equal(
             _bind$767, (moonbit_string_t)moonbit_string_literal_86.data
           )
         ) {
    moonbit_decref(_bind$767);
    goto $join$766;
  } else if (
           moonbit_val_array_equal(
             _bind$767, (moonbit_string_t)moonbit_string_literal_85.data
           )
         ) {
    moonbit_decref(_bind$767);
    goto $join$766;
  } else if (
           moonbit_val_array_equal(
             _bind$767, (moonbit_string_t)moonbit_string_literal_81.data
           )
         ) {
    moonbit_decref(_bind$767);
    goto $join$766;
  } else if (
           moonbit_val_array_equal(
             _bind$767, (moonbit_string_t)moonbit_string_literal_84.data
           )
         ) {
    moonbit_decref(_bind$767);
    goto $join$765;
  } else if (
           moonbit_val_array_equal(
             _bind$767, (moonbit_string_t)moonbit_string_literal_83.data
           )
         ) {
    moonbit_decref(_bind$767);
    goto $join$765;
  } else if (
           moonbit_val_array_equal(
             _bind$767, (moonbit_string_t)moonbit_string_literal_80.data
           )
         ) {
    moonbit_decref(_bind$767);
    goto $join$764;
  } else if (
           moonbit_val_array_equal(
             _bind$767, (moonbit_string_t)moonbit_string_literal_79.data
           )
         ) {
    moonbit_decref(_bind$767);
    goto $join$764;
  } else if (
           moonbit_val_array_equal(
             _bind$767, (moonbit_string_t)moonbit_string_literal_78.data
           )
         ) {
    moonbit_decref(_bind$767);
    goto $join$764;
  } else if (
           moonbit_val_array_equal(
             _bind$767, (moonbit_string_t)moonbit_string_literal_74.data
           )
         ) {
    moonbit_decref(_bind$767);
    goto $join$763;
  } else if (
           moonbit_val_array_equal(
             _bind$767, (moonbit_string_t)moonbit_string_literal_73.data
           )
         ) {
    moonbit_decref(_bind$767);
    goto $join$763;
  } else if (
           moonbit_val_array_equal(
             _bind$767, (moonbit_string_t)moonbit_string_literal_72.data
           )
         ) {
    moonbit_decref(_bind$767);
    goto $join$763;
  } else if (
           moonbit_val_array_equal(
             _bind$767, (moonbit_string_t)moonbit_string_literal_71.data
           )
         ) {
    moonbit_decref(_bind$767);
    goto $join$762;
  } else if (
           moonbit_val_array_equal(
             _bind$767, (moonbit_string_t)moonbit_string_literal_69.data
           )
         ) {
    moonbit_decref(_bind$767);
    goto $join$762;
  } else if (
           moonbit_val_array_equal(
             _bind$767, (moonbit_string_t)moonbit_string_literal_68.data
           )
         ) {
    moonbit_decref(_bind$767);
    goto $join$759;
  } else if (
           moonbit_val_array_equal(
             _bind$767, (moonbit_string_t)moonbit_string_literal_66.data
           )
         ) {
    moonbit_decref(_bind$767);
    goto $join$759;
  } else {
    moonbit_string_t _tmp$2293;
    moonbit_string_t _tmp$2292;
    moonbit_string_t _tmp$2291;
    moonbit_decref(_bind$767);
    _tmp$2293
    = moonbit_add_string(
      (moonbit_string_t)moonbit_string_literal_88.data, idx$760
    );
    _tmp$2292
    = moonbit_add_string(
      _tmp$2293, (moonbit_string_t)moonbit_string_literal_89.data
    );
    _tmp$2291 = moonbit_add_string(_tmp$2292, expr$761);
    return moonbit_add_string(
             _tmp$2291, (moonbit_string_t)moonbit_string_literal_90.data
           );
  }
  $join$766:;
  _tmp$2290
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_91.data, idx$760
  );
  _tmp$2289
  = moonbit_add_string(
    _tmp$2290, (moonbit_string_t)moonbit_string_literal_26.data
  );
  _tmp$2288 = moonbit_add_string(_tmp$2289, expr$761);
  return moonbit_add_string(
           _tmp$2288, (moonbit_string_t)moonbit_string_literal_92.data
         );
  $join$765:;
  _tmp$2287
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_93.data, idx$760
  );
  _tmp$2286
  = moonbit_add_string(
    _tmp$2287, (moonbit_string_t)moonbit_string_literal_26.data
  );
  _tmp$2285 = moonbit_add_string(_tmp$2286, expr$761);
  return moonbit_add_string(
           _tmp$2285, (moonbit_string_t)moonbit_string_literal_94.data
         );
  $join$764:;
  _tmp$2284
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_95.data, idx$760
  );
  _tmp$2283
  = moonbit_add_string(
    _tmp$2284, (moonbit_string_t)moonbit_string_literal_26.data
  );
  _tmp$2282 = moonbit_add_string(_tmp$2283, expr$761);
  return moonbit_add_string(
           _tmp$2282, (moonbit_string_t)moonbit_string_literal_94.data
         );
  $join$763:;
  _tmp$2281
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_88.data, idx$760
  );
  _tmp$2280
  = moonbit_add_string(
    _tmp$2281, (moonbit_string_t)moonbit_string_literal_89.data
  );
  _tmp$2279 = moonbit_add_string(_tmp$2280, expr$761);
  return moonbit_add_string(
           _tmp$2279, (moonbit_string_t)moonbit_string_literal_90.data
         );
  $join$762:;
  _tmp$2278
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_96.data, idx$760
  );
  _tmp$2277
  = moonbit_add_string(
    _tmp$2278, (moonbit_string_t)moonbit_string_literal_26.data
  );
  _tmp$2276 = moonbit_add_string(_tmp$2277, expr$761);
  return moonbit_add_string(
           _tmp$2276, (moonbit_string_t)moonbit_string_literal_94.data
         );
  $join$759:;
  _tmp$2275
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_91.data, idx$760
  );
  _tmp$2274
  = moonbit_add_string(
    _tmp$2275, (moonbit_string_t)moonbit_string_literal_97.data
  );
  _tmp$2273 = moonbit_add_string(_tmp$2274, expr$761);
  return moonbit_add_string(
           _tmp$2273, (moonbit_string_t)moonbit_string_literal_98.data
         );
}

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$escape_string(
  moonbit_string_t s$754
) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$752 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* _it$753 = $String$$iter(s$754);
  while (1) {
    int32_t _bind$755;
    moonbit_incref(_it$753);
    _bind$755 = $$moonbitlang$core$builtin$Iter$$next$0(_it$753);
    if (_bind$755 == -1) {
      moonbit_decref(_it$753);
    } else {
      int32_t _Some$756 = _bind$755;
      int32_t _c$757 = _Some$756;
      if (_c$757 == 34) {
        moonbit_incref(buf$752);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$752, (moonbit_string_t)moonbit_string_literal_99.data
        );
      } else if (_c$757 == 92) {
        moonbit_incref(buf$752);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$752, (moonbit_string_t)moonbit_string_literal_100.data
        );
      } else if (_c$757 == 10) {
        moonbit_incref(buf$752);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$752, (moonbit_string_t)moonbit_string_literal_6.data
        );
      } else if (_c$757 == 13) {
        
      } else if (_c$757 == 9) {
        moonbit_incref(buf$752);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$752, (moonbit_string_t)moonbit_string_literal_6.data
        );
      } else {
        moonbit_incref(buf$752);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
          buf$752, _c$757
        );
      }
      continue;
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$752);
}

moonbit_string_t $sqlc_gen_moonbit$tools$codegen$column_to_moonbit(
  moonbit_string_t sql_type$751,
  int32_t idx$744
) {
  moonbit_string_t idx_str$743 = $Int$$to_string$inner(idx$744, 10);
  moonbit_string_t _bind$750 = $String$$to_upper(sql_type$751);
  moonbit_string_t _tmp$2271;
  moonbit_string_t _tmp$2270;
  moonbit_string_t _tmp$2269;
  moonbit_string_t _tmp$2268;
  moonbit_string_t _tmp$2267;
  if (
    moonbit_val_array_equal(
      _bind$750, (moonbit_string_t)moonbit_string_literal_87.data
    )
  ) {
    moonbit_decref(_bind$750);
    goto $join$749;
  } else if (
           moonbit_val_array_equal(
             _bind$750, (moonbit_string_t)moonbit_string_literal_86.data
           )
         ) {
    moonbit_decref(_bind$750);
    goto $join$749;
  } else if (
           moonbit_val_array_equal(
             _bind$750, (moonbit_string_t)moonbit_string_literal_85.data
           )
         ) {
    moonbit_decref(_bind$750);
    goto $join$749;
  } else if (
           moonbit_val_array_equal(
             _bind$750, (moonbit_string_t)moonbit_string_literal_84.data
           )
         ) {
    moonbit_decref(_bind$750);
    goto $join$749;
  } else if (
           moonbit_val_array_equal(
             _bind$750, (moonbit_string_t)moonbit_string_literal_83.data
           )
         ) {
    moonbit_decref(_bind$750);
    goto $join$749;
  } else if (
           moonbit_val_array_equal(
             _bind$750, (moonbit_string_t)moonbit_string_literal_81.data
           )
         ) {
    moonbit_decref(_bind$750);
    goto $join$749;
  } else if (
           moonbit_val_array_equal(
             _bind$750, (moonbit_string_t)moonbit_string_literal_80.data
           )
         ) {
    moonbit_decref(_bind$750);
    goto $join$748;
  } else if (
           moonbit_val_array_equal(
             _bind$750, (moonbit_string_t)moonbit_string_literal_79.data
           )
         ) {
    moonbit_decref(_bind$750);
    goto $join$748;
  } else if (
           moonbit_val_array_equal(
             _bind$750, (moonbit_string_t)moonbit_string_literal_78.data
           )
         ) {
    moonbit_decref(_bind$750);
    goto $join$748;
  } else if (
           moonbit_val_array_equal(
             _bind$750, (moonbit_string_t)moonbit_string_literal_74.data
           )
         ) {
    moonbit_decref(_bind$750);
    goto $join$747;
  } else if (
           moonbit_val_array_equal(
             _bind$750, (moonbit_string_t)moonbit_string_literal_73.data
           )
         ) {
    moonbit_decref(_bind$750);
    goto $join$747;
  } else if (
           moonbit_val_array_equal(
             _bind$750, (moonbit_string_t)moonbit_string_literal_72.data
           )
         ) {
    moonbit_decref(_bind$750);
    goto $join$747;
  } else if (
           moonbit_val_array_equal(
             _bind$750, (moonbit_string_t)moonbit_string_literal_71.data
           )
         ) {
    moonbit_decref(_bind$750);
    goto $join$746;
  } else if (
           moonbit_val_array_equal(
             _bind$750, (moonbit_string_t)moonbit_string_literal_69.data
           )
         ) {
    moonbit_decref(_bind$750);
    goto $join$746;
  } else if (
           moonbit_val_array_equal(
             _bind$750, (moonbit_string_t)moonbit_string_literal_68.data
           )
         ) {
    moonbit_decref(_bind$750);
    goto $join$745;
  } else if (
           moonbit_val_array_equal(
             _bind$750, (moonbit_string_t)moonbit_string_literal_66.data
           )
         ) {
    moonbit_decref(_bind$750);
    goto $join$745;
  } else {
    moonbit_string_t _tmp$2272;
    moonbit_decref(_bind$750);
    _tmp$2272
    = moonbit_add_string(
      (moonbit_string_t)moonbit_string_literal_101.data, idx_str$743
    );
    return moonbit_add_string(
             _tmp$2272, (moonbit_string_t)moonbit_string_literal_90.data
           );
  }
  $join$749:;
  _tmp$2271
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_102.data, idx_str$743
  );
  return moonbit_add_string(
           _tmp$2271, (moonbit_string_t)moonbit_string_literal_94.data
         );
  $join$748:;
  _tmp$2270
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_103.data, idx_str$743
  );
  return moonbit_add_string(
           _tmp$2270, (moonbit_string_t)moonbit_string_literal_94.data
         );
  $join$747:;
  _tmp$2269
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_101.data, idx_str$743
  );
  return moonbit_add_string(
           _tmp$2269, (moonbit_string_t)moonbit_string_literal_90.data
         );
  $join$746:;
  _tmp$2268
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_104.data, idx_str$743
  );
  return moonbit_add_string(
           _tmp$2268, (moonbit_string_t)moonbit_string_literal_94.data
         );
  $join$745:;
  _tmp$2267
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_105.data, idx_str$743
  );
  return moonbit_add_string(
           _tmp$2267, (moonbit_string_t)moonbit_string_literal_106.data
         );
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $moonbitlang$x$sys$get_cli_args(
  
) {
  return $moonbitlang$x$sys$internal$ffi$get_cli_args();
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $moonbitlang$x$sys$internal$ffi$get_cli_args(
  
) {
  moonbit_bytes_t* _tmp$2264 =
    $moonbitlang$x$sys$internal$ffi$internal_get_cli_args();
  struct $$3c$Bytes$3e$$3d$$3e$String* _tmp$2265 =
    (struct $$3c$Bytes$3e$$3d$$3e$String*)&$$moonbitlang$x$sys$internal$ffi$get_cli_args$fn$6$closure.data;
  moonbit_string_t* _tmp$2263 = $FixedArray$$map$0(_tmp$2264, _tmp$2265);
  return $$moonbitlang$core$builtin$Array$$from_fixed_array$0(_tmp$2263);
}

moonbit_string_t $$moonbitlang$x$sys$internal$ffi$get_cli_args$fn$6(
  struct $$3c$Bytes$3e$$3d$$3e$String* _env$2266,
  moonbit_bytes_t arg$742
) {
  moonbit_decref(_env$2266);
  return $moonbitlang$x$internal$ffi$utf8_bytes_to_mbt_string(arg$742);
}

struct moonbit_result_0 $moonbitlang$x$fs$read_file_to_string$inner(
  moonbit_string_t path$740,
  moonbit_string_t encoding$741
) {
  return $moonbitlang$x$fs$read_file_to_string_internal$inner(
           path$740, encoding$741
         );
}

struct moonbit_result_0 $moonbitlang$x$fs$read_file_to_string_internal$inner(
  moonbit_string_t path$739,
  moonbit_string_t encoding$738
) {
  if (
    moonbit_val_array_equal(
      encoding$738, (moonbit_string_t)moonbit_string_literal_107.data
    )
  ) {
    struct moonbit_result_1 _tmp$2812;
    moonbit_bytes_t _tmp$2257;
    moonbit_string_t _tmp$2256;
    struct moonbit_result_0 _result$2814;
    moonbit_decref(encoding$738);
    _tmp$2812 = $moonbitlang$x$fs$read_file_to_bytes_internal(path$739);
    if (_tmp$2812.tag) {
      moonbit_bytes_t const _ok$2258 = _tmp$2812.data.ok;
      _tmp$2257 = _ok$2258;
    } else {
      void* const _err$2259 = _tmp$2812.data.err;
      struct moonbit_result_0 _result$2813;
      _result$2813.tag = 0;
      _result$2813.data.err = _err$2259;
      return _result$2813;
    }
    _tmp$2256
    = $moonbitlang$x$internal$ffi$utf8_bytes_to_mbt_string(
      _tmp$2257
    );
    _result$2814.tag = 1;
    _result$2814.data.ok = _tmp$2256;
    return _result$2814;
  } else {
    moonbit_string_t _tmp$2262;
    moonbit_string_t _tmp$2261;
    void* moonbitlang$x$fs$IOError$IOError$2260;
    struct moonbit_result_0 _result$2815;
    moonbit_decref(path$739);
    _tmp$2262
    = moonbit_add_string(
      (moonbit_string_t)moonbit_string_literal_108.data, encoding$738
    );
    _tmp$2261
    = moonbit_add_string(
      _tmp$2262, (moonbit_string_t)moonbit_string_literal_109.data
    );
    moonbitlang$x$fs$IOError$IOError$2260
    = (void*)moonbit_malloc(
        sizeof(struct $Error$moonbitlang$x$fs$IOError$IOError)
      );
    Moonbit_object_header(moonbitlang$x$fs$IOError$IOError$2260)->meta
    = Moonbit_make_regular_object_header(
      offsetof(struct $Error$moonbitlang$x$fs$IOError$IOError, $0) >> 2, 1, 2
    );
    ((struct $Error$moonbitlang$x$fs$IOError$IOError*)moonbitlang$x$fs$IOError$IOError$2260)->$0
    = _tmp$2261;
    _result$2815.tag = 0;
    _result$2815.data.err = moonbitlang$x$fs$IOError$IOError$2260;
    return _result$2815;
  }
}

struct moonbit_result_1 $moonbitlang$x$fs$read_file_to_bytes_internal(
  moonbit_string_t path$734
) {
  moonbit_bytes_t _tmp$2255 =
    $moonbitlang$x$internal$ffi$mbt_string_to_utf8_bytes(path$734, 1);
  void* _tmp$2522 =
    $moonbitlang$x$fs$fopen_ffi(
      _tmp$2255, (moonbit_bytes_t)moonbit_bytes_literal_0.data
    );
  void* file$733;
  int32_t _tmp$2239;
  moonbit_decref(_tmp$2255);
  file$733 = _tmp$2522;
  _tmp$2239 = $moonbitlang$x$fs$is_null(file$733);
  if (_tmp$2239 == 0) {
    int32_t _tmp$2240 = $moonbitlang$x$fs$fseek_ffi(file$733, 0, 2);
    if (_tmp$2240 == 0) {
      int32_t size$735 = $moonbitlang$x$fs$ftell_ffi(file$733);
      if (size$735 != -1) {
        int32_t _tmp$2241 = $moonbitlang$x$fs$fseek_ffi(file$733, 0, 0);
        if (_tmp$2241 == 0) {
          moonbit_bytes_t bytes$736 = $Bytes$$make(size$735, 0);
          int32_t bytes_read$737 =
            $moonbitlang$x$fs$fread_ffi(bytes$736, 1, size$735, file$733);
          if (bytes_read$737 == size$735) {
            int32_t _tmp$2242 = $moonbitlang$x$fs$fclose_ffi(file$733);
            if (_tmp$2242 == 0) {
              struct moonbit_result_1 _result$2816;
              _result$2816.tag = 1;
              _result$2816.data.ok = bytes$736;
              return _result$2816;
            } else {
              moonbit_string_t _tmp$2244;
              void* moonbitlang$x$fs$IOError$IOError$2243;
              struct moonbit_result_1 _result$2817;
              moonbit_decref(bytes$736);
              _tmp$2244 = $moonbitlang$x$fs$get_error_message();
              moonbitlang$x$fs$IOError$IOError$2243
              = (void*)moonbit_malloc(
                  sizeof(struct $Error$moonbitlang$x$fs$IOError$IOError)
                );
              Moonbit_object_header(
                moonbitlang$x$fs$IOError$IOError$2243
              )->meta
              = Moonbit_make_regular_object_header(
                offsetof(
                  struct $Error$moonbitlang$x$fs$IOError$IOError, $0
                )
                >> 2,
                  1,
                  2
              );
              ((struct $Error$moonbitlang$x$fs$IOError$IOError*)moonbitlang$x$fs$IOError$IOError$2243)->$0
              = _tmp$2244;
              _result$2817.tag = 0;
              _result$2817.data.err = moonbitlang$x$fs$IOError$IOError$2243;
              return _result$2817;
            }
          } else {
            moonbit_string_t _tmp$2246;
            void* moonbitlang$x$fs$IOError$IOError$2245;
            struct moonbit_result_1 _result$2818;
            moonbit_decref(bytes$736);
            _tmp$2246 = $moonbitlang$x$fs$get_error_message();
            moonbitlang$x$fs$IOError$IOError$2245
            = (void*)moonbit_malloc(
                sizeof(struct $Error$moonbitlang$x$fs$IOError$IOError)
              );
            Moonbit_object_header(
              moonbitlang$x$fs$IOError$IOError$2245
            )->meta
            = Moonbit_make_regular_object_header(
              offsetof(
                struct $Error$moonbitlang$x$fs$IOError$IOError, $0
              )
              >> 2,
                1,
                2
            );
            ((struct $Error$moonbitlang$x$fs$IOError$IOError*)moonbitlang$x$fs$IOError$IOError$2245)->$0
            = _tmp$2246;
            _result$2818.tag = 0;
            _result$2818.data.err = moonbitlang$x$fs$IOError$IOError$2245;
            return _result$2818;
          }
        } else {
          moonbit_string_t _tmp$2248 = $moonbitlang$x$fs$get_error_message();
          void* moonbitlang$x$fs$IOError$IOError$2247 =
            (void*)moonbit_malloc(
              sizeof(struct $Error$moonbitlang$x$fs$IOError$IOError)
            );
          struct moonbit_result_1 _result$2819;
          Moonbit_object_header(moonbitlang$x$fs$IOError$IOError$2247)->meta
          = Moonbit_make_regular_object_header(
            offsetof(struct $Error$moonbitlang$x$fs$IOError$IOError, $0) >> 2,
              1,
              2
          );
          ((struct $Error$moonbitlang$x$fs$IOError$IOError*)moonbitlang$x$fs$IOError$IOError$2247)->$0
          = _tmp$2248;
          _result$2819.tag = 0;
          _result$2819.data.err = moonbitlang$x$fs$IOError$IOError$2247;
          return _result$2819;
        }
      } else {
        moonbit_string_t _tmp$2250 = $moonbitlang$x$fs$get_error_message();
        void* moonbitlang$x$fs$IOError$IOError$2249 =
          (void*)moonbit_malloc(
            sizeof(struct $Error$moonbitlang$x$fs$IOError$IOError)
          );
        struct moonbit_result_1 _result$2820;
        Moonbit_object_header(moonbitlang$x$fs$IOError$IOError$2249)->meta
        = Moonbit_make_regular_object_header(
          offsetof(struct $Error$moonbitlang$x$fs$IOError$IOError, $0) >> 2,
            1,
            2
        );
        ((struct $Error$moonbitlang$x$fs$IOError$IOError*)moonbitlang$x$fs$IOError$IOError$2249)->$0
        = _tmp$2250;
        _result$2820.tag = 0;
        _result$2820.data.err = moonbitlang$x$fs$IOError$IOError$2249;
        return _result$2820;
      }
    } else {
      moonbit_string_t _tmp$2252 = $moonbitlang$x$fs$get_error_message();
      void* moonbitlang$x$fs$IOError$IOError$2251 =
        (void*)moonbit_malloc(
          sizeof(struct $Error$moonbitlang$x$fs$IOError$IOError)
        );
      struct moonbit_result_1 _result$2821;
      Moonbit_object_header(moonbitlang$x$fs$IOError$IOError$2251)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $Error$moonbitlang$x$fs$IOError$IOError, $0) >> 2,
          1,
          2
      );
      ((struct $Error$moonbitlang$x$fs$IOError$IOError*)moonbitlang$x$fs$IOError$IOError$2251)->$0
      = _tmp$2252;
      _result$2821.tag = 0;
      _result$2821.data.err = moonbitlang$x$fs$IOError$IOError$2251;
      return _result$2821;
    }
  } else {
    moonbit_string_t _tmp$2254 = $moonbitlang$x$fs$get_error_message();
    void* moonbitlang$x$fs$IOError$IOError$2253 =
      (void*)moonbit_malloc(
        sizeof(struct $Error$moonbitlang$x$fs$IOError$IOError)
      );
    struct moonbit_result_1 _result$2822;
    Moonbit_object_header(moonbitlang$x$fs$IOError$IOError$2253)->meta
    = Moonbit_make_regular_object_header(
      offsetof(struct $Error$moonbitlang$x$fs$IOError$IOError, $0) >> 2, 1, 2
    );
    ((struct $Error$moonbitlang$x$fs$IOError$IOError*)moonbitlang$x$fs$IOError$IOError$2253)->$0
    = _tmp$2254;
    _result$2822.tag = 0;
    _result$2822.data.err = moonbitlang$x$fs$IOError$IOError$2253;
    return _result$2822;
  }
}

moonbit_string_t $moonbitlang$x$fs$get_error_message() {
  moonbit_bytes_t _tmp$2238 = $moonbitlang$x$fs$get_error_message_ffi();
  return $moonbitlang$x$internal$ffi$utf8_bytes_to_mbt_string(_tmp$2238);
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$x$fs$IOError$$output(
  void* _x_67$729,
  struct $$moonbitlang$core$builtin$Logger _x_68$732
) {
  struct $Error$moonbitlang$x$fs$IOError$IOError* _IOError$730 =
    (struct $Error$moonbitlang$x$fs$IOError$IOError*)_x_67$729;
  moonbit_string_t _field$2523 = _IOError$730->$0;
  int32_t _cnt$2736 = Moonbit_object_header(_IOError$730)->rc;
  moonbit_string_t _$2a$err_payload_69$731;
  struct $$moonbitlang$core$builtin$Logger _bind$2237;
  if (_cnt$2736 > 1) {
    int32_t _new_cnt$2737 = _cnt$2736 - 1;
    Moonbit_object_header(_IOError$730)->rc = _new_cnt$2737;
    moonbit_incref(_field$2523);
  } else if (_cnt$2736 == 1) {
    moonbit_free(_IOError$730);
  }
  _$2a$err_payload_69$731 = _field$2523;
  if (_x_68$732.$1) {
    moonbit_incref(_x_68$732.$1);
  }
  _x_68$732.$0->$method_0(
    _x_68$732.$1, (moonbit_string_t)moonbit_string_literal_110.data
  );
  if (_x_68$732.$1) {
    moonbit_incref(_x_68$732.$1);
  }
  $$moonbitlang$core$builtin$Logger$$write_object$0(
    _x_68$732, _$2a$err_payload_69$731
  );
  _bind$2237 = _x_68$732;
  _bind$2237.$0->$method_0(
    _bind$2237.$1, (moonbit_string_t)moonbit_string_literal_94.data
  );
  return 0;
}

moonbit_string_t $moonbitlang$x$internal$ffi$utf8_bytes_to_mbt_string(
  moonbit_bytes_t bytes$725
) {
  int32_t* _tmp$2236 = (int32_t*)moonbit_empty_int32_array;
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* res$723 =
    (struct $$moonbitlang$core$builtin$Array$3c$Char$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Char$3e$)
    );
  int32_t len$724;
  int32_t i$726;
  int32_t* _field$2525;
  int32_t* buf$2234;
  int32_t _field$2524;
  int32_t _cnt$2738;
  int32_t len$2235;
  struct $$moonbitlang$core$builtin$ArrayView$3c$Char$3e$ _tmp$2233;
  Moonbit_object_header(res$723)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Char$3e$, $0) >> 2,
      1,
      0
  );
  res$723->$0 = _tmp$2236;
  res$723->$1 = 0;
  len$724 = Moonbit_array_length(bytes$725);
  i$726 = 0;
  while (1) {
    int32_t _tmp$2157 = i$726;
    if (_tmp$2157 < len$724) {
      int32_t _tmp$2232 = i$726;
      int32_t _tmp$2231;
      int32_t c$727;
      int32_t _tmp$2158;
      if (_tmp$2232 < 0 || _tmp$2232 >= Moonbit_array_length(bytes$725)) {
        moonbit_panic();
      }
      _tmp$2231 = bytes$725[_tmp$2232];
      c$727 = (int32_t)_tmp$2231;
      _tmp$2158 = c$727;
      if (_tmp$2158 < 128) {
        int32_t _tmp$2160 = c$727;
        int32_t _tmp$2159 = _tmp$2160;
        int32_t _tmp$2161;
        moonbit_incref(res$723);
        $$moonbitlang$core$builtin$Array$$push$3(res$723, _tmp$2159);
        _tmp$2161 = i$726;
        i$726 = _tmp$2161 + 1;
      } else {
        int32_t _tmp$2162 = c$727;
        if (_tmp$2162 < 224) {
          int32_t _tmp$2164 = i$726;
          int32_t _tmp$2163 = _tmp$2164 + 1;
          int32_t _tmp$2172;
          int32_t _tmp$2171;
          int32_t _tmp$2165;
          int32_t _tmp$2170;
          int32_t _tmp$2169;
          int32_t _tmp$2168;
          int32_t _tmp$2167;
          int32_t _tmp$2166;
          int32_t _tmp$2174;
          int32_t _tmp$2173;
          int32_t _tmp$2175;
          if (_tmp$2163 >= len$724) {
            moonbit_decref(bytes$725);
            break;
          }
          _tmp$2172 = c$727;
          _tmp$2171 = _tmp$2172 & 31;
          _tmp$2165 = _tmp$2171 << 6;
          _tmp$2170 = i$726;
          _tmp$2169 = _tmp$2170 + 1;
          if (_tmp$2169 < 0 || _tmp$2169 >= Moonbit_array_length(bytes$725)) {
            moonbit_panic();
          }
          _tmp$2168 = bytes$725[_tmp$2169];
          _tmp$2167 = (int32_t)_tmp$2168;
          _tmp$2166 = _tmp$2167 & 63;
          c$727 = _tmp$2165 | _tmp$2166;
          _tmp$2174 = c$727;
          _tmp$2173 = _tmp$2174;
          moonbit_incref(res$723);
          $$moonbitlang$core$builtin$Array$$push$3(res$723, _tmp$2173);
          _tmp$2175 = i$726;
          i$726 = _tmp$2175 + 2;
        } else {
          int32_t _tmp$2176 = c$727;
          if (_tmp$2176 < 240) {
            int32_t _tmp$2178 = i$726;
            int32_t _tmp$2177 = _tmp$2178 + 2;
            int32_t _tmp$2193;
            int32_t _tmp$2192;
            int32_t _tmp$2185;
            int32_t _tmp$2191;
            int32_t _tmp$2190;
            int32_t _tmp$2189;
            int32_t _tmp$2188;
            int32_t _tmp$2187;
            int32_t _tmp$2186;
            int32_t _tmp$2179;
            int32_t _tmp$2184;
            int32_t _tmp$2183;
            int32_t _tmp$2182;
            int32_t _tmp$2181;
            int32_t _tmp$2180;
            int32_t _tmp$2195;
            int32_t _tmp$2194;
            int32_t _tmp$2196;
            if (_tmp$2177 >= len$724) {
              moonbit_decref(bytes$725);
              break;
            }
            _tmp$2193 = c$727;
            _tmp$2192 = _tmp$2193 & 15;
            _tmp$2185 = _tmp$2192 << 12;
            _tmp$2191 = i$726;
            _tmp$2190 = _tmp$2191 + 1;
            if (
              _tmp$2190 < 0 || _tmp$2190 >= Moonbit_array_length(bytes$725)
            ) {
              moonbit_panic();
            }
            _tmp$2189 = bytes$725[_tmp$2190];
            _tmp$2188 = (int32_t)_tmp$2189;
            _tmp$2187 = _tmp$2188 & 63;
            _tmp$2186 = _tmp$2187 << 6;
            _tmp$2179 = _tmp$2185 | _tmp$2186;
            _tmp$2184 = i$726;
            _tmp$2183 = _tmp$2184 + 2;
            if (
              _tmp$2183 < 0 || _tmp$2183 >= Moonbit_array_length(bytes$725)
            ) {
              moonbit_panic();
            }
            _tmp$2182 = bytes$725[_tmp$2183];
            _tmp$2181 = (int32_t)_tmp$2182;
            _tmp$2180 = _tmp$2181 & 63;
            c$727 = _tmp$2179 | _tmp$2180;
            _tmp$2195 = c$727;
            _tmp$2194 = _tmp$2195;
            moonbit_incref(res$723);
            $$moonbitlang$core$builtin$Array$$push$3(res$723, _tmp$2194);
            _tmp$2196 = i$726;
            i$726 = _tmp$2196 + 3;
          } else {
            int32_t _tmp$2198 = i$726;
            int32_t _tmp$2197 = _tmp$2198 + 3;
            int32_t _tmp$2220;
            int32_t _tmp$2219;
            int32_t _tmp$2212;
            int32_t _tmp$2218;
            int32_t _tmp$2217;
            int32_t _tmp$2216;
            int32_t _tmp$2215;
            int32_t _tmp$2214;
            int32_t _tmp$2213;
            int32_t _tmp$2205;
            int32_t _tmp$2211;
            int32_t _tmp$2210;
            int32_t _tmp$2209;
            int32_t _tmp$2208;
            int32_t _tmp$2207;
            int32_t _tmp$2206;
            int32_t _tmp$2199;
            int32_t _tmp$2204;
            int32_t _tmp$2203;
            int32_t _tmp$2202;
            int32_t _tmp$2201;
            int32_t _tmp$2200;
            int32_t _tmp$2221;
            int32_t _tmp$2225;
            int32_t _tmp$2224;
            int32_t _tmp$2223;
            int32_t _tmp$2222;
            int32_t _tmp$2229;
            int32_t _tmp$2228;
            int32_t _tmp$2227;
            int32_t _tmp$2226;
            int32_t _tmp$2230;
            if (_tmp$2197 >= len$724) {
              moonbit_decref(bytes$725);
              break;
            }
            _tmp$2220 = c$727;
            _tmp$2219 = _tmp$2220 & 7;
            _tmp$2212 = _tmp$2219 << 18;
            _tmp$2218 = i$726;
            _tmp$2217 = _tmp$2218 + 1;
            if (
              _tmp$2217 < 0 || _tmp$2217 >= Moonbit_array_length(bytes$725)
            ) {
              moonbit_panic();
            }
            _tmp$2216 = bytes$725[_tmp$2217];
            _tmp$2215 = (int32_t)_tmp$2216;
            _tmp$2214 = _tmp$2215 & 63;
            _tmp$2213 = _tmp$2214 << 12;
            _tmp$2205 = _tmp$2212 | _tmp$2213;
            _tmp$2211 = i$726;
            _tmp$2210 = _tmp$2211 + 2;
            if (
              _tmp$2210 < 0 || _tmp$2210 >= Moonbit_array_length(bytes$725)
            ) {
              moonbit_panic();
            }
            _tmp$2209 = bytes$725[_tmp$2210];
            _tmp$2208 = (int32_t)_tmp$2209;
            _tmp$2207 = _tmp$2208 & 63;
            _tmp$2206 = _tmp$2207 << 6;
            _tmp$2199 = _tmp$2205 | _tmp$2206;
            _tmp$2204 = i$726;
            _tmp$2203 = _tmp$2204 + 3;
            if (
              _tmp$2203 < 0 || _tmp$2203 >= Moonbit_array_length(bytes$725)
            ) {
              moonbit_panic();
            }
            _tmp$2202 = bytes$725[_tmp$2203];
            _tmp$2201 = (int32_t)_tmp$2202;
            _tmp$2200 = _tmp$2201 & 63;
            c$727 = _tmp$2199 | _tmp$2200;
            _tmp$2221 = c$727;
            c$727 = _tmp$2221 - 65536;
            _tmp$2225 = c$727;
            _tmp$2224 = _tmp$2225 >> 10;
            _tmp$2223 = _tmp$2224 + 55296;
            _tmp$2222 = _tmp$2223;
            moonbit_incref(res$723);
            $$moonbitlang$core$builtin$Array$$push$3(res$723, _tmp$2222);
            _tmp$2229 = c$727;
            _tmp$2228 = _tmp$2229 & 1023;
            _tmp$2227 = _tmp$2228 + 56320;
            _tmp$2226 = _tmp$2227;
            moonbit_incref(res$723);
            $$moonbitlang$core$builtin$Array$$push$3(res$723, _tmp$2226);
            _tmp$2230 = i$726;
            i$726 = _tmp$2230 + 4;
          }
        }
      }
      continue;
    } else {
      moonbit_decref(bytes$725);
    }
    break;
  }
  _field$2525 = res$723->$0;
  buf$2234 = _field$2525;
  _field$2524 = res$723->$1;
  _cnt$2738 = Moonbit_object_header(res$723)->rc;
  if (_cnt$2738 > 1) {
    int32_t _new_cnt$2739 = _cnt$2738 - 1;
    Moonbit_object_header(res$723)->rc = _new_cnt$2739;
    moonbit_incref(buf$2234);
  } else if (_cnt$2738 == 1) {
    moonbit_free(res$723);
  }
  len$2235 = _field$2524;
  _tmp$2233
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$Char$3e$){
    0, len$2235, buf$2234
  };
  return $String$$from_array(_tmp$2233);
}

moonbit_bytes_t $moonbitlang$x$internal$ffi$mbt_string_to_utf8_bytes(
  moonbit_string_t str$717,
  int32_t is_filename$722
) {
  moonbit_bytes_t _tmp$2156 = (moonbit_bytes_t)moonbit_empty_int8_array;
  struct $$moonbitlang$core$builtin$Array$3c$Byte$3e$* res$715 =
    (struct $$moonbitlang$core$builtin$Array$3c$Byte$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Byte$3e$)
    );
  int32_t len$716;
  int32_t i$718;
  moonbit_bytes_t _field$2527;
  moonbit_bytes_t buf$2154;
  int32_t _field$2526;
  int32_t _cnt$2740;
  int32_t len$2155;
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ _tmp$2153;
  Moonbit_object_header(res$715)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Byte$3e$, $0) >> 2,
      1,
      0
  );
  res$715->$0 = _tmp$2156;
  res$715->$1 = 0;
  len$716 = Moonbit_array_length(str$717);
  i$718 = 0;
  while (1) {
    int32_t _tmp$2094 = i$718;
    if (_tmp$2094 < len$716) {
      int32_t _tmp$2151 = i$718;
      int32_t _tmp$2150;
      int32_t c$719;
      int32_t _tmp$2096;
      int32_t _if_result$2825;
      int32_t _tmp$2105;
      int32_t _tmp$2149;
      if (_tmp$2151 < 0 || _tmp$2151 >= Moonbit_array_length(str$717)) {
        moonbit_panic();
      }
      _tmp$2150 = str$717[_tmp$2151];
      c$719 = (int32_t)_tmp$2150;
      _tmp$2096 = c$719;
      if (55296 <= _tmp$2096) {
        int32_t _tmp$2095 = c$719;
        _if_result$2825 = _tmp$2095 <= 56319;
      } else {
        _if_result$2825 = 0;
      }
      if (_if_result$2825) {
        int32_t _tmp$2097 = c$719;
        int32_t _tmp$2098;
        int32_t _tmp$2104;
        int32_t _tmp$2103;
        int32_t _tmp$2102;
        int32_t l$720;
        int32_t _tmp$2101;
        int32_t _tmp$2100;
        int32_t _tmp$2099;
        c$719 = _tmp$2097 - 55296;
        _tmp$2098 = i$718;
        i$718 = _tmp$2098 + 1;
        _tmp$2104 = i$718;
        if (_tmp$2104 < 0 || _tmp$2104 >= Moonbit_array_length(str$717)) {
          moonbit_panic();
        }
        _tmp$2103 = str$717[_tmp$2104];
        _tmp$2102 = (int32_t)_tmp$2103;
        l$720 = _tmp$2102 - 56320;
        _tmp$2101 = c$719;
        _tmp$2100 = _tmp$2101 << 10;
        _tmp$2099 = _tmp$2100 + l$720;
        c$719 = _tmp$2099 + 65536;
      }
      _tmp$2105 = c$719;
      if (_tmp$2105 < 128) {
        int32_t _tmp$2107 = c$719;
        int32_t _tmp$2106 = _tmp$2107 & 0xff;
        moonbit_incref(res$715);
        $$moonbitlang$core$builtin$Array$$push$4(res$715, _tmp$2106);
      } else {
        int32_t _tmp$2108 = c$719;
        if (_tmp$2108 < 2048) {
          int32_t _tmp$2112 = c$719;
          int32_t _tmp$2111 = _tmp$2112 >> 6;
          int32_t _tmp$2110 = 192 + _tmp$2111;
          int32_t _tmp$2109 = _tmp$2110 & 0xff;
          int32_t _tmp$2116;
          int32_t _tmp$2115;
          int32_t _tmp$2114;
          int32_t _tmp$2113;
          moonbit_incref(res$715);
          $$moonbitlang$core$builtin$Array$$push$4(res$715, _tmp$2109);
          _tmp$2116 = c$719;
          _tmp$2115 = _tmp$2116 & 63;
          _tmp$2114 = 128 + _tmp$2115;
          _tmp$2113 = _tmp$2114 & 0xff;
          moonbit_incref(res$715);
          $$moonbitlang$core$builtin$Array$$push$4(res$715, _tmp$2113);
        } else {
          int32_t _tmp$2117 = c$719;
          if (_tmp$2117 < 65536) {
            int32_t _tmp$2121 = c$719;
            int32_t _tmp$2120 = _tmp$2121 >> 12;
            int32_t _tmp$2119 = 224 + _tmp$2120;
            int32_t _tmp$2118 = _tmp$2119 & 0xff;
            int32_t _tmp$2126;
            int32_t _tmp$2125;
            int32_t _tmp$2124;
            int32_t _tmp$2123;
            int32_t _tmp$2122;
            int32_t _tmp$2130;
            int32_t _tmp$2129;
            int32_t _tmp$2128;
            int32_t _tmp$2127;
            moonbit_incref(res$715);
            $$moonbitlang$core$builtin$Array$$push$4(res$715, _tmp$2118);
            _tmp$2126 = c$719;
            _tmp$2125 = _tmp$2126 >> 6;
            _tmp$2124 = _tmp$2125 & 63;
            _tmp$2123 = 128 + _tmp$2124;
            _tmp$2122 = _tmp$2123 & 0xff;
            moonbit_incref(res$715);
            $$moonbitlang$core$builtin$Array$$push$4(res$715, _tmp$2122);
            _tmp$2130 = c$719;
            _tmp$2129 = _tmp$2130 & 63;
            _tmp$2128 = 128 + _tmp$2129;
            _tmp$2127 = _tmp$2128 & 0xff;
            moonbit_incref(res$715);
            $$moonbitlang$core$builtin$Array$$push$4(res$715, _tmp$2127);
          } else {
            int32_t _tmp$2134 = c$719;
            int32_t _tmp$2133 = _tmp$2134 >> 18;
            int32_t _tmp$2132 = 240 + _tmp$2133;
            int32_t _tmp$2131 = _tmp$2132 & 0xff;
            int32_t _tmp$2139;
            int32_t _tmp$2138;
            int32_t _tmp$2137;
            int32_t _tmp$2136;
            int32_t _tmp$2135;
            int32_t _tmp$2144;
            int32_t _tmp$2143;
            int32_t _tmp$2142;
            int32_t _tmp$2141;
            int32_t _tmp$2140;
            int32_t _tmp$2148;
            int32_t _tmp$2147;
            int32_t _tmp$2146;
            int32_t _tmp$2145;
            moonbit_incref(res$715);
            $$moonbitlang$core$builtin$Array$$push$4(res$715, _tmp$2131);
            _tmp$2139 = c$719;
            _tmp$2138 = _tmp$2139 >> 12;
            _tmp$2137 = _tmp$2138 & 63;
            _tmp$2136 = 128 + _tmp$2137;
            _tmp$2135 = _tmp$2136 & 0xff;
            moonbit_incref(res$715);
            $$moonbitlang$core$builtin$Array$$push$4(res$715, _tmp$2135);
            _tmp$2144 = c$719;
            _tmp$2143 = _tmp$2144 >> 6;
            _tmp$2142 = _tmp$2143 & 63;
            _tmp$2141 = 128 + _tmp$2142;
            _tmp$2140 = _tmp$2141 & 0xff;
            moonbit_incref(res$715);
            $$moonbitlang$core$builtin$Array$$push$4(res$715, _tmp$2140);
            _tmp$2148 = c$719;
            _tmp$2147 = _tmp$2148 & 63;
            _tmp$2146 = 128 + _tmp$2147;
            _tmp$2145 = _tmp$2146 & 0xff;
            moonbit_incref(res$715);
            $$moonbitlang$core$builtin$Array$$push$4(res$715, _tmp$2145);
          }
        }
      }
      _tmp$2149 = i$718;
      i$718 = _tmp$2149 + 1;
      continue;
    } else {
      moonbit_decref(str$717);
    }
    break;
  }
  if (is_filename$722) {
    int32_t _tmp$2152 = 0 & 0xff;
    moonbit_incref(res$715);
    $$moonbitlang$core$builtin$Array$$push$4(res$715, _tmp$2152);
  }
  _field$2527 = res$715->$0;
  buf$2154 = _field$2527;
  _field$2526 = res$715->$1;
  _cnt$2740 = Moonbit_object_header(res$715)->rc;
  if (_cnt$2740 > 1) {
    int32_t _new_cnt$2741 = _cnt$2740 - 1;
    Moonbit_object_header(res$715)->rc = _new_cnt$2741;
    moonbit_incref(buf$2154);
  } else if (_cnt$2740 == 1) {
    moonbit_free(res$715);
  }
  len$2155 = _field$2526;
  _tmp$2153
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$){
    0, len$2155, buf$2154
  };
  return $Bytes$$from_array(_tmp$2153);
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$from_fixed_array$0(
  moonbit_string_t* arr$713
) {
  int32_t len$712 = Moonbit_array_length(arr$713);
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* arr2$714 =
    $$moonbitlang$core$builtin$Array$$make_uninit$0(len$712);
  moonbit_string_t* _field$2528 = arr2$714->$0;
  moonbit_string_t* buf$2093 = _field$2528;
  moonbit_incref(buf$2093);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit_fixed$0(
    buf$2093, 0, arr$713, 0, len$712
  );
  return arr2$714;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$710,
  struct $$moonbitlang$core$builtin$Logger logger$711
) {
  moonbit_string_t _tmp$2092 = self$710;
  struct $$moonbitlang$core$builtin$SourceLocRepr* _tmp$2091 =
    $$moonbitlang$core$builtin$SourceLocRepr$$parse(_tmp$2092);
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
    _tmp$2091, logger$711
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$673,
  struct $$moonbitlang$core$builtin$Logger logger$709
) {
  struct $StringView _field$2538 =
    (struct $StringView){self$673->$0_1, self$673->$0_2, self$673->$0_0};
  struct $StringView pkg$672 = _field$2538;
  moonbit_string_t _field$2537 = pkg$672.$0;
  moonbit_string_t _data$674 = _field$2537;
  int32_t _start$675 = pkg$672.$1;
  int32_t end$2089 = pkg$672.$2;
  int32_t start$2090 = pkg$672.$1;
  int32_t _tmp$2088 = end$2089 - start$2090;
  int32_t _end$676 = _start$675 + _tmp$2088;
  int32_t _cursor$677 = _start$675;
  int32_t accept_state$678 = -1;
  int32_t match_end$679 = -1;
  int32_t match_tag_saver_0$680 = -1;
  int32_t tag_0$681 = -1;
  struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$* _bind$682;
  struct $StringView _field$2536;
  struct $StringView _module_name$705;
  void* _field$2535;
  int32_t _cnt$2742;
  void* _package_name$706;
  struct $StringView _field$2533;
  struct $StringView filename$2051;
  struct $StringView _field$2532;
  struct $StringView start_line$2052;
  struct $StringView _field$2531;
  struct $StringView start_column$2053;
  struct $StringView _field$2530;
  struct $StringView end_line$2054;
  struct $StringView _field$2529;
  int32_t _cnt$2746;
  struct $StringView end_column$2055;
  struct $$moonbitlang$core$builtin$Logger _bind$2050;
  moonbit_incref(_data$674);
  moonbit_incref(pkg$672.$0);
  while (1) {
    int32_t _tmp$2070 = _cursor$677;
    if (_tmp$2070 < _end$676) {
      int32_t _p$1189 = _cursor$677;
      int32_t next_char$692 = _data$674[_p$1189];
      int32_t _tmp$2071 = _cursor$677;
      _cursor$677 = _tmp$2071 + 1;
      if (next_char$692 < 55296) {
        if (next_char$692 < 47) {
          goto $join$690;
        } else if (next_char$692 > 47) {
          goto $join$690;
        } else {
          while (1) {
            int32_t _tmp$2072;
            tag_0$681 = _cursor$677;
            _tmp$2072 = _cursor$677;
            if (_tmp$2072 < _end$676) {
              int32_t _p$1192 = _cursor$677;
              int32_t next_char$695 = _data$674[_p$1192];
              int32_t _tmp$2073 = _cursor$677;
              _cursor$677 = _tmp$2073 + 1;
              if (next_char$695 < 55296) {
                if (next_char$695 < 47) {
                  goto $join$693;
                } else if (next_char$695 > 47) {
                  goto $join$693;
                } else {
                  while (1) {
                    int32_t _tmp$2074 = _cursor$677;
                    if (_tmp$2074 < _end$676) {
                      int32_t _p$1195 = _cursor$677;
                      int32_t next_char$698 = _data$674[_p$1195];
                      int32_t _tmp$2075 = _cursor$677;
                      _cursor$677 = _tmp$2075 + 1;
                      if (next_char$698 < 56319) {
                        if (next_char$698 < 55296) {
                          goto $join$696;
                        } else {
                          int32_t _tmp$2076 = _cursor$677;
                          if (_tmp$2076 < _end$676) {
                            int32_t _p$1198 = _cursor$677;
                            int32_t next_char$699 = _data$674[_p$1198];
                            int32_t _tmp$2077 = _cursor$677;
                            _cursor$677 = _tmp$2077 + 1;
                            if (next_char$699 < 56320) {
                              goto $join$683;
                            } else if (next_char$699 > 65535) {
                              goto $join$683;
                            } else {
                              continue;
                            }
                          } else {
                            goto $join$683;
                          }
                        }
                      } else if (next_char$698 > 56319) {
                        if (next_char$698 < 65536) {
                          goto $join$696;
                        } else {
                          goto $join$683;
                        }
                      } else {
                        int32_t _tmp$2078 = _cursor$677;
                        if (_tmp$2078 < _end$676) {
                          int32_t _p$1201 = _cursor$677;
                          int32_t next_char$700 = _data$674[_p$1201];
                          int32_t _tmp$2079 = _cursor$677;
                          _cursor$677 = _tmp$2079 + 1;
                          if (next_char$700 < 56320) {
                            goto $join$683;
                          } else if (next_char$700 > 57343) {
                            goto $join$683;
                          } else {
                            continue;
                          }
                        } else {
                          goto $join$683;
                        }
                      }
                      goto $joinlet$2832;
                      $join$696:;
                      continue;
                      $joinlet$2832:;
                    } else {
                      match_tag_saver_0$680 = tag_0$681;
                      accept_state$678 = 0;
                      match_end$679 = _cursor$677;
                      goto $join$683;
                    }
                    break;
                  }
                }
              } else if (next_char$695 > 56318) {
                if (next_char$695 < 57344) {
                  int32_t _tmp$2080 = _cursor$677;
                  if (_tmp$2080 < _end$676) {
                    int32_t _p$1204 = _cursor$677;
                    int32_t next_char$701 = _data$674[_p$1204];
                    int32_t _tmp$2081 = _cursor$677;
                    _cursor$677 = _tmp$2081 + 1;
                    if (next_char$701 < 56320) {
                      goto $join$683;
                    } else if (next_char$701 > 57343) {
                      goto $join$683;
                    } else {
                      continue;
                    }
                  } else {
                    goto $join$683;
                  }
                } else if (next_char$695 > 65535) {
                  goto $join$683;
                } else {
                  goto $join$693;
                }
              } else {
                int32_t _tmp$2082 = _cursor$677;
                if (_tmp$2082 < _end$676) {
                  int32_t _p$1207 = _cursor$677;
                  int32_t next_char$702 = _data$674[_p$1207];
                  int32_t _tmp$2083 = _cursor$677;
                  _cursor$677 = _tmp$2083 + 1;
                  if (next_char$702 < 56320) {
                    goto $join$683;
                  } else if (next_char$702 > 65535) {
                    goto $join$683;
                  } else {
                    continue;
                  }
                } else {
                  goto $join$683;
                }
              }
              goto $joinlet$2830;
              $join$693:;
              continue;
              $joinlet$2830:;
            } else {
              goto $join$683;
            }
            break;
          }
        }
      } else if (next_char$692 > 56318) {
        if (next_char$692 < 57344) {
          int32_t _tmp$2084 = _cursor$677;
          if (_tmp$2084 < _end$676) {
            int32_t _p$1210 = _cursor$677;
            int32_t next_char$703 = _data$674[_p$1210];
            int32_t _tmp$2085 = _cursor$677;
            _cursor$677 = _tmp$2085 + 1;
            if (next_char$703 < 56320) {
              goto $join$683;
            } else if (next_char$703 > 57343) {
              goto $join$683;
            } else {
              continue;
            }
          } else {
            goto $join$683;
          }
        } else if (next_char$692 > 65535) {
          goto $join$683;
        } else {
          goto $join$690;
        }
      } else {
        int32_t _tmp$2086 = _cursor$677;
        if (_tmp$2086 < _end$676) {
          int32_t _p$1213 = _cursor$677;
          int32_t next_char$704 = _data$674[_p$1213];
          int32_t _tmp$2087 = _cursor$677;
          _cursor$677 = _tmp$2087 + 1;
          if (next_char$704 < 56320) {
            goto $join$683;
          } else if (next_char$704 > 65535) {
            goto $join$683;
          } else {
            continue;
          }
        } else {
          goto $join$683;
        }
      }
      goto $joinlet$2828;
      $join$690:;
      continue;
      $joinlet$2828:;
    } else {
      goto $join$683;
    }
    break;
  }
  goto $joinlet$2826;
  $join$683:;
  switch (accept_state$678) {
    case 0: {
      void* _try_err$686;
      struct $StringView package_name$684;
      int32_t _tmp$2066;
      int32_t _tmp$2065;
      int64_t _tmp$2062;
      int32_t _tmp$2064;
      int64_t _tmp$2063;
      struct moonbit_result_2 _tmp$2834;
      void* _try_err$689;
      struct $StringView module_name$687;
      int64_t _tmp$2057;
      int32_t _tmp$2059;
      int64_t _tmp$2058;
      struct moonbit_result_2 _tmp$2836;
      void* Some$2056;
      moonbit_decref(pkg$672.$0);
      _tmp$2066 = match_tag_saver_0$680;
      _tmp$2065 = _tmp$2066 + 1;
      _tmp$2062 = (int64_t)_tmp$2065;
      _tmp$2064 = match_end$679;
      _tmp$2063 = (int64_t)_tmp$2064;
      moonbit_incref(_data$674);
      _tmp$2834 = $String$$sub(_data$674, _tmp$2062, _tmp$2063);
      if (_tmp$2834.tag) {
        struct $StringView const _ok$2067 = _tmp$2834.data.ok;
        package_name$684 = _ok$2067;
      } else {
        void* const _err$2068 = _tmp$2834.data.err;
        _try_err$686 = _err$2068;
        goto $join$685;
      }
      goto $joinlet$2833;
      $join$685:;
      moonbit_decref(_try_err$686);
      moonbit_panic();
      $joinlet$2833:;
      _tmp$2057 = (int64_t)_start$675;
      _tmp$2059 = match_tag_saver_0$680;
      _tmp$2058 = (int64_t)_tmp$2059;
      _tmp$2836 = $String$$sub(_data$674, _tmp$2057, _tmp$2058);
      if (_tmp$2836.tag) {
        struct $StringView const _ok$2060 = _tmp$2836.data.ok;
        module_name$687 = _ok$2060;
      } else {
        void* const _err$2061 = _tmp$2836.data.err;
        _try_err$689 = _err$2061;
        goto $join$688;
      }
      goto $joinlet$2835;
      $join$688:;
      moonbit_decref(_try_err$689);
      moonbit_panic();
      $joinlet$2835:;
      Some$2056
      = (void*)moonbit_malloc(sizeof(struct $Option$3c$StringView$3e$$Some));
      Moonbit_object_header(Some$2056)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $Option$3c$StringView$3e$$Some, $0_0) >> 2, 1, 1
      );
      ((struct $Option$3c$StringView$3e$$Some*)Some$2056)->$0_0
      = package_name$684.$0;
      ((struct $Option$3c$StringView$3e$$Some*)Some$2056)->$0_1
      = package_name$684.$1;
      ((struct $Option$3c$StringView$3e$$Some*)Some$2056)->$0_2
      = package_name$684.$2;
      _bind$682
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$682)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$682->$0_0 = module_name$687.$0;
      _bind$682->$0_1 = module_name$687.$1;
      _bind$682->$0_2 = module_name$687.$2;
      _bind$682->$1 = Some$2056;
      break;
    }
    default: {
      void* None$2069;
      moonbit_decref(_data$674);
      None$2069 = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _bind$682
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$682)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$682->$0_0 = pkg$672.$0;
      _bind$682->$0_1 = pkg$672.$1;
      _bind$682->$0_2 = pkg$672.$2;
      _bind$682->$1 = None$2069;
      break;
    }
  }
  $joinlet$2826:;
  _field$2536
  = (struct $StringView){
    _bind$682->$0_1, _bind$682->$0_2, _bind$682->$0_0
  };
  _module_name$705 = _field$2536;
  _field$2535 = _bind$682->$1;
  _cnt$2742 = Moonbit_object_header(_bind$682)->rc;
  if (_cnt$2742 > 1) {
    int32_t _new_cnt$2743 = _cnt$2742 - 1;
    Moonbit_object_header(_bind$682)->rc = _new_cnt$2743;
    moonbit_incref(_field$2535);
    moonbit_incref(_module_name$705.$0);
  } else if (_cnt$2742 == 1) {
    moonbit_free(_bind$682);
  }
  _package_name$706 = _field$2535;
  switch (Moonbit_object_tag(_package_name$706)) {
    case 1: {
      struct $Option$3c$StringView$3e$$Some* _Some$707 =
        (struct $Option$3c$StringView$3e$$Some*)_package_name$706;
      struct $StringView _field$2534 =
        (struct $StringView){
          _Some$707->$0_1, _Some$707->$0_2, _Some$707->$0_0
        };
      int32_t _cnt$2744 = Moonbit_object_header(_Some$707)->rc;
      struct $StringView _pkg_name$708;
      struct $$moonbitlang$core$builtin$Logger _bind$2049;
      if (_cnt$2744 > 1) {
        int32_t _new_cnt$2745 = _cnt$2744 - 1;
        Moonbit_object_header(_Some$707)->rc = _new_cnt$2745;
        moonbit_incref(_field$2534.$0);
      } else if (_cnt$2744 == 1) {
        moonbit_free(_Some$707);
      }
      _pkg_name$708 = _field$2534;
      if (logger$709.$1) {
        moonbit_incref(logger$709.$1);
      }
      logger$709.$0->$method_2(logger$709.$1, _pkg_name$708);
      _bind$2049 = logger$709;
      if (_bind$2049.$1) {
        moonbit_incref(_bind$2049.$1);
      }
      _bind$2049.$0->$method_3(_bind$2049.$1, 47);
      break;
    }
    default: {
      moonbit_decref(_package_name$706);
      break;
    }
  }
  _field$2533
  = (struct $StringView){
    self$673->$1_1, self$673->$1_2, self$673->$1_0
  };
  filename$2051 = _field$2533;
  moonbit_incref(filename$2051.$0);
  if (logger$709.$1) {
    moonbit_incref(logger$709.$1);
  }
  logger$709.$0->$method_2(logger$709.$1, filename$2051);
  if (logger$709.$1) {
    moonbit_incref(logger$709.$1);
  }
  logger$709.$0->$method_3(logger$709.$1, 58);
  _field$2532
  = (struct $StringView){
    self$673->$2_1, self$673->$2_2, self$673->$2_0
  };
  start_line$2052 = _field$2532;
  moonbit_incref(start_line$2052.$0);
  if (logger$709.$1) {
    moonbit_incref(logger$709.$1);
  }
  logger$709.$0->$method_2(logger$709.$1, start_line$2052);
  if (logger$709.$1) {
    moonbit_incref(logger$709.$1);
  }
  logger$709.$0->$method_3(logger$709.$1, 58);
  _field$2531
  = (struct $StringView){
    self$673->$3_1, self$673->$3_2, self$673->$3_0
  };
  start_column$2053 = _field$2531;
  moonbit_incref(start_column$2053.$0);
  if (logger$709.$1) {
    moonbit_incref(logger$709.$1);
  }
  logger$709.$0->$method_2(logger$709.$1, start_column$2053);
  if (logger$709.$1) {
    moonbit_incref(logger$709.$1);
  }
  logger$709.$0->$method_3(logger$709.$1, 45);
  _field$2530
  = (struct $StringView){
    self$673->$4_1, self$673->$4_2, self$673->$4_0
  };
  end_line$2054 = _field$2530;
  moonbit_incref(end_line$2054.$0);
  if (logger$709.$1) {
    moonbit_incref(logger$709.$1);
  }
  logger$709.$0->$method_2(logger$709.$1, end_line$2054);
  if (logger$709.$1) {
    moonbit_incref(logger$709.$1);
  }
  logger$709.$0->$method_3(logger$709.$1, 58);
  _field$2529
  = (struct $StringView){
    self$673->$5_1, self$673->$5_2, self$673->$5_0
  };
  _cnt$2746 = Moonbit_object_header(self$673)->rc;
  if (_cnt$2746 > 1) {
    int32_t _new_cnt$2752 = _cnt$2746 - 1;
    Moonbit_object_header(self$673)->rc = _new_cnt$2752;
    moonbit_incref(_field$2529.$0);
  } else if (_cnt$2746 == 1) {
    struct $StringView _field$2751 =
      (struct $StringView){self$673->$4_1, self$673->$4_2, self$673->$4_0};
    struct $StringView _field$2750;
    struct $StringView _field$2749;
    struct $StringView _field$2748;
    struct $StringView _field$2747;
    moonbit_decref(_field$2751.$0);
    _field$2750
    = (struct $StringView){
      self$673->$3_1, self$673->$3_2, self$673->$3_0
    };
    moonbit_decref(_field$2750.$0);
    _field$2749
    = (struct $StringView){
      self$673->$2_1, self$673->$2_2, self$673->$2_0
    };
    moonbit_decref(_field$2749.$0);
    _field$2748
    = (struct $StringView){
      self$673->$1_1, self$673->$1_2, self$673->$1_0
    };
    moonbit_decref(_field$2748.$0);
    _field$2747
    = (struct $StringView){
      self$673->$0_1, self$673->$0_2, self$673->$0_0
    };
    moonbit_decref(_field$2747.$0);
    moonbit_free(self$673);
  }
  end_column$2055 = _field$2529;
  if (logger$709.$1) {
    moonbit_incref(logger$709.$1);
  }
  logger$709.$0->$method_2(logger$709.$1, end_column$2055);
  if (logger$709.$1) {
    moonbit_incref(logger$709.$1);
  }
  logger$709.$0->$method_3(logger$709.$1, 64);
  _bind$2050 = logger$709;
  _bind$2050.$0->$method_2(_bind$2050.$1, _module_name$705);
  return 0;
}

moonbit_bytes_t $Bytes$$from_array(
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ arr$670
) {
  int32_t end$2047 = arr$670.$2;
  int32_t start$2048 = arr$670.$1;
  int32_t _tmp$2043 = end$2047 - start$2048;
  struct $Bytes$$from_array$fn$5$2d$cap* _closure$2837 =
    (struct $Bytes$$from_array$fn$5$2d$cap*)moonbit_malloc(
      sizeof(struct $Bytes$$from_array$fn$5$2d$cap)
    );
  struct $$3c$Char$3e$$3d$$3e$Bool* _tmp$2044;
  Moonbit_object_header(_closure$2837)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Bytes$$from_array$fn$5$2d$cap, $0_0) >> 2, 1, 0
  );
  _closure$2837->code = &$Bytes$$from_array$fn$5;
  _closure$2837->$0_0 = arr$670.$0;
  _closure$2837->$0_1 = arr$670.$1;
  _closure$2837->$0_2 = arr$670.$2;
  _tmp$2044 = (struct $$3c$Char$3e$$3d$$3e$Bool*)_closure$2837;
  return $Bytes$$makei$0(_tmp$2043, _tmp$2044);
}

int32_t $Bytes$$from_array$fn$5(
  struct $$3c$Char$3e$$3d$$3e$Bool* _env$2045,
  int32_t i$671
) {
  struct $Bytes$$from_array$fn$5$2d$cap* _casted_env$2046 =
    (struct $Bytes$$from_array$fn$5$2d$cap*)_env$2045;
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ _field$2539 =
    (struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$){
      _casted_env$2046->$0_1, _casted_env$2046->$0_2, _casted_env$2046->$0_0
    };
  int32_t _cnt$2753 = Moonbit_object_header(_casted_env$2046)->rc;
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ arr$670;
  if (_cnt$2753 > 1) {
    int32_t _new_cnt$2754 = _cnt$2753 - 1;
    Moonbit_object_header(_casted_env$2046)->rc = _new_cnt$2754;
    moonbit_incref(_field$2539.$0);
  } else if (_cnt$2753 == 1) {
    moonbit_free(_casted_env$2046);
  }
  arr$670 = _field$2539;
  return $$moonbitlang$core$builtin$ArrayView$$at$0(arr$670, i$671);
}

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$669) {
  moonbit_println(input$669);
  moonbit_decref(input$669);
  return 0;
}

moonbit_bytes_t $Bytes$$makei$0(
  int32_t length$664,
  struct $$3c$Char$3e$$3d$$3e$Bool* value$666
) {
  int32_t _tmp$2042;
  moonbit_bytes_t arr$665;
  int32_t i$667;
  if (length$664 <= 0) {
    moonbit_decref(value$666);
    return (moonbit_bytes_t)moonbit_bytes_literal_1.data;
  }
  moonbit_incref(value$666);
  _tmp$2042 = value$666->code(value$666, 0);
  arr$665 = (moonbit_bytes_t)moonbit_make_bytes(length$664, _tmp$2042);
  i$667 = 1;
  while (1) {
    if (i$667 < length$664) {
      int32_t _tmp$2040;
      int32_t _tmp$2041;
      moonbit_incref(value$666);
      _tmp$2040 = value$666->code(value$666, i$667);
      if (i$667 < 0 || i$667 >= Moonbit_array_length(arr$665)) {
        moonbit_panic();
      }
      arr$665[i$667] = _tmp$2040;
      _tmp$2041 = i$667 + 1;
      i$667 = _tmp$2041;
      continue;
    } else {
      moonbit_decref(value$666);
    }
    break;
  }
  return arr$665;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit_fixed$0(
  moonbit_string_t* dst$658,
  int32_t dst_offset$659,
  moonbit_string_t* src$660,
  int32_t src_offset$661,
  int32_t len$663
) {
  int32_t _tmp$2039 = len$663 - 1;
  int32_t i$657 = _tmp$2039;
  while (1) {
    if (i$657 >= 0) {
      int32_t _tmp$2035 = dst_offset$659 + i$657;
      int32_t _tmp$2037 = src_offset$661 + i$657;
      moonbit_string_t _tmp$2541;
      moonbit_string_t _tmp$2036;
      moonbit_string_t _old$2540;
      int32_t _tmp$2038;
      if (_tmp$2037 < 0 || _tmp$2037 >= Moonbit_array_length(src$660)) {
        moonbit_panic();
      }
      _tmp$2541 = (moonbit_string_t)src$660[_tmp$2037];
      _tmp$2036 = _tmp$2541;
      if (_tmp$2035 < 0 || _tmp$2035 >= Moonbit_array_length(dst$658)) {
        moonbit_panic();
      }
      _old$2540 = (moonbit_string_t)dst$658[_tmp$2035];
      moonbit_incref(_tmp$2036);
      moonbit_decref(_old$2540);
      dst$658[_tmp$2035] = _tmp$2036;
      _tmp$2038 = i$657 - 1;
      i$657 = _tmp$2038;
      continue;
    } else {
      moonbit_decref(src$660);
      moonbit_decref(dst$658);
    }
    break;
  }
  return 0;
}

moonbit_bytes_t $Bytes$$make(int32_t len$655, int32_t init$656) {
  if (len$655 < 0) {
    return (moonbit_bytes_t)moonbit_bytes_literal_1.data;
  }
  return moonbit_make_bytes(len$655, init$656);
}

struct $$3c$Int$2a$Char$3e$* $$moonbitlang$core$builtin$Iter2$$next$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$* self$654
) {
  return $$moonbitlang$core$builtin$Iter$$next$1(self$654);
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$make_uninit$0(
  int32_t len$653
) {
  moonbit_string_t* _tmp$2034 =
    (moonbit_string_t*)moonbit_make_ref_array(
      len$653, (moonbit_string_t)moonbit_string_literal_111.data
    );
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$2840 =
    (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_block$2840)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _block$2840->$0 = _tmp$2034;
  _block$2840->$1 = len$653;
  return _block$2840;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$at$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ self$652,
  int32_t index$651
) {
  int32_t _if_result$2841;
  if (index$651 >= 0) {
    int32_t end$2021 = self$652.$2;
    int32_t start$2022 = self$652.$1;
    int32_t _tmp$2020 = end$2021 - start$2022;
    _if_result$2841 = index$651 < _tmp$2020;
  } else {
    _if_result$2841 = 0;
  }
  if (_if_result$2841) {
    moonbit_bytes_t _field$2544 = self$652.$0;
    moonbit_bytes_t buf$2023 = _field$2544;
    int32_t _field$2543 = self$652.$1;
    int32_t start$2025 = _field$2543;
    int32_t _tmp$2024 = start$2025 + index$651;
    int32_t _tmp$2542;
    if (_tmp$2024 < 0 || _tmp$2024 >= Moonbit_array_length(buf$2023)) {
      moonbit_panic();
    }
    _tmp$2542 = (int32_t)buf$2023[_tmp$2024];
    moonbit_decref(buf$2023);
    return _tmp$2542;
  } else {
    int32_t end$2032 = self$652.$2;
    int32_t _field$2545 = self$652.$1;
    int32_t start$2033;
    int32_t _tmp$2031;
    moonbit_string_t _tmp$2030;
    moonbit_string_t _tmp$2029;
    moonbit_string_t _tmp$2027;
    moonbit_string_t _tmp$2028;
    moonbit_string_t _tmp$2026;
    moonbit_decref(self$652.$0);
    start$2033 = _field$2545;
    _tmp$2031 = end$2032 - start$2033;
    _tmp$2030
    = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
      _tmp$2031
    );
    _tmp$2029
    = moonbit_add_string(
      (moonbit_string_t)moonbit_string_literal_112.data, _tmp$2030
    );
    _tmp$2027
    = moonbit_add_string(
      _tmp$2029, (moonbit_string_t)moonbit_string_literal_113.data
    );
    _tmp$2028
    = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
      index$651
    );
    _tmp$2026 = moonbit_add_string(_tmp$2027, _tmp$2028);
    return $moonbitlang$core$builtin$abort$3(
             _tmp$2026, (moonbit_string_t)moonbit_string_literal_114.data
           );
  }
}

moonbit_string_t* $FixedArray$$map$0(
  moonbit_bytes_t* self$645,
  struct $$3c$Bytes$3e$$3d$$3e$String* f$647
) {
  int32_t _tmp$2013 = Moonbit_array_length(self$645);
  int32_t _tmp$2017;
  moonbit_bytes_t _tmp$2548;
  moonbit_bytes_t _tmp$2019;
  moonbit_string_t _tmp$2018;
  moonbit_string_t* res$646;
  int32_t _end2240$648;
  int32_t i$649;
  if (_tmp$2013 == 0) {
    moonbit_decref(f$647);
    moonbit_decref(self$645);
    return (moonbit_string_t*)moonbit_empty_ref_array;
  }
  _tmp$2017 = Moonbit_array_length(self$645);
  if (0 < 0 || 0 >= Moonbit_array_length(self$645)) {
    moonbit_panic();
  }
  _tmp$2548 = (moonbit_bytes_t)self$645[0];
  _tmp$2019 = _tmp$2548;
  moonbit_incref(_tmp$2019);
  moonbit_incref(f$647);
  _tmp$2018 = f$647->code(f$647, _tmp$2019);
  res$646 = (moonbit_string_t*)moonbit_make_ref_array(_tmp$2017, _tmp$2018);
  _end2240$648 = Moonbit_array_length(self$645);
  i$649 = 1;
  while (1) {
    if (i$649 < _end2240$648) {
      moonbit_bytes_t _tmp$2547;
      moonbit_bytes_t _tmp$2015;
      moonbit_string_t _tmp$2014;
      moonbit_string_t _old$2546;
      int32_t _tmp$2016;
      if (i$649 < 0 || i$649 >= Moonbit_array_length(self$645)) {
        moonbit_panic();
      }
      _tmp$2547 = (moonbit_bytes_t)self$645[i$649];
      _tmp$2015 = _tmp$2547;
      moonbit_incref(_tmp$2015);
      moonbit_incref(f$647);
      _tmp$2014 = f$647->code(f$647, _tmp$2015);
      if (i$649 < 0 || i$649 >= Moonbit_array_length(res$646)) {
        moonbit_panic();
      }
      _old$2546 = (moonbit_string_t)res$646[i$649];
      moonbit_decref(_old$2546);
      res$646[i$649] = _tmp$2014;
      _tmp$2016 = i$649 + 1;
      i$649 = _tmp$2016;
      continue;
    } else {
      moonbit_decref(f$647);
      moonbit_decref(self$645);
    }
    break;
  }
  return res$646;
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$644
) {
  return self$644;
}

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$643,
  struct $$moonbitlang$core$builtin$Logger logger$642
) {
  moonbit_string_t _tmp$2012 = $Int$$to_string$inner(self$643, 10);
  logger$642.$0->$method_0(logger$642.$1, _tmp$2012);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* $String$$to_array(
  moonbit_string_t self$639
) {
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* _p$1167;
  int32_t _tmp$2549;
  int32_t _tmp$2011;
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* _p$1168;
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* _p$1171;
  moonbit_incref(self$639);
  _p$1167 = $String$$iter(self$639);
  _tmp$2549 = Moonbit_array_length(self$639);
  moonbit_decref(self$639);
  _tmp$2011 = _tmp$2549;
  _p$1168 = $$moonbitlang$core$builtin$Array$$new$inner$0(_tmp$2011);
  _p$1171 = _p$1168;
  while (1) {
    int32_t _p$1173;
    moonbit_incref(_p$1167);
    _p$1173 = $$moonbitlang$core$builtin$Iter$$next$0(_p$1167);
    if (_p$1173 == -1) {
      moonbit_decref(_p$1167);
    } else {
      int32_t _p$1174 = _p$1173;
      int32_t _p$1175 = _p$1174;
      struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* _p$1176 = _p$1171;
      moonbit_incref(_p$1176);
      $$moonbitlang$core$builtin$Array$$push$3(_p$1176, _p$1175);
      _p$1171 = _p$1176;
      continue;
    }
    break;
  }
  return _p$1171;
}

moonbit_string_t $String$$to_upper(moonbit_string_t self$628) {
  struct $$3c$Char$3e$$3d$$3e$Bool* _tmp$2009 =
    (struct $$3c$Char$3e$$3d$$3e$Bool*)&$String$$to_upper$fn$4$closure.data;
  int64_t _bind$627;
  moonbit_incref(self$628);
  _bind$627 = $String$$find_by(self$628, _tmp$2009);
  if (_bind$627 == 4294967296ll) {
    return self$628;
  } else {
    int64_t _Some$630 = _bind$627;
    int32_t _idx$631 = (int32_t)_Some$630;
    int32_t _tmp$2008 = Moonbit_array_length(self$628);
    struct $$moonbitlang$core$builtin$StringBuilder* buf$632 =
      $$moonbitlang$core$builtin$StringBuilder$$new$inner(_tmp$2008);
    int64_t _tmp$2007 = (int64_t)_idx$631;
    struct $StringView head$633;
    moonbit_string_t _field$2551;
    moonbit_string_t str$1998;
    int32_t start$1999;
    int32_t end$2001;
    int32_t _field$2550;
    int32_t start$2002;
    int32_t _tmp$2000;
    struct $StringView _tmp$2006;
    struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* _it$634;
    moonbit_incref(self$628);
    head$633 = $String$$view$inner(self$628, 0, _tmp$2007);
    _field$2551 = head$633.$0;
    str$1998 = _field$2551;
    start$1999 = head$633.$1;
    end$2001 = head$633.$2;
    _field$2550 = head$633.$1;
    start$2002 = _field$2550;
    _tmp$2000 = end$2001 - start$2002;
    moonbit_incref(buf$632);
    $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
      buf$632, str$1998, start$1999, _tmp$2000
    );
    _tmp$2006 = $String$$view$inner(self$628, _idx$631, 4294967296ll);
    _it$634 = $StringView$$iter(_tmp$2006);
    while (1) {
      int32_t _bind$635;
      moonbit_incref(_it$634);
      _bind$635 = $$moonbitlang$core$builtin$Iter$$next$0(_it$634);
      if (_bind$635 == -1) {
        moonbit_decref(_it$634);
      } else {
        int32_t _Some$636 = _bind$635;
        int32_t _c$637 = _Some$636;
        if ($Char$$is_ascii_lowercase(_c$637)) {
          int32_t _tmp$2005 = _c$637;
          int32_t _tmp$2004 = _tmp$2005 - 32;
          int32_t _tmp$2003 = _tmp$2004;
          moonbit_incref(buf$632);
          $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
            buf$632, _tmp$2003
          );
        } else {
          moonbit_incref(buf$632);
          $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
            buf$632, _c$637
          );
        }
        continue;
      }
      break;
    }
    return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$632);
  }
}

int32_t $String$$to_upper$fn$4(
  struct $$3c$Char$3e$$3d$$3e$Bool* _env$2010,
  int32_t _hole3791$629
) {
  moonbit_decref(_env$2010);
  return $Char$$is_ascii_lowercase(_hole3791$629);
}

int32_t $Char$$is_ascii_lowercase(int32_t self$626) {
  return self$626 >= 97 && self$626 <= 122 || 0;
}

struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* $String$$iter(
  moonbit_string_t self$621
) {
  int32_t len$620 = Moonbit_array_length(self$621);
  struct $Ref$3c$Int$3e$* index$622 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  struct $String$$iter$$2a$p$fn$3$2d$cap* _closure$2845;
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* _p$1158;
  Moonbit_object_header(index$622)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  index$622->$0 = 0;
  _closure$2845
  = (struct $String$$iter$$2a$p$fn$3$2d$cap*)moonbit_malloc(
      sizeof(struct $String$$iter$$2a$p$fn$3$2d$cap)
    );
  Moonbit_object_header(_closure$2845)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $String$$iter$$2a$p$fn$3$2d$cap, $0) >> 2, 2, 0
  );
  _closure$2845->code = &$String$$iter$$2a$p$fn$3;
  _closure$2845->$0 = index$622;
  _closure$2845->$1 = self$621;
  _closure$2845->$2 = len$620;
  _p$1158 = (struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$*)_closure$2845;
  return _p$1158;
}

int32_t $String$$iter$$2a$p$fn$3(
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* _env$1985
) {
  struct $String$$iter$$2a$p$fn$3$2d$cap* _casted_env$1986 =
    (struct $String$$iter$$2a$p$fn$3$2d$cap*)_env$1985;
  int32_t len$620 = _casted_env$1986->$2;
  moonbit_string_t _field$2554 = _casted_env$1986->$1;
  moonbit_string_t self$621 = _field$2554;
  struct $Ref$3c$Int$3e$* _field$2553 = _casted_env$1986->$0;
  int32_t _cnt$2755 = Moonbit_object_header(_casted_env$1986)->rc;
  struct $Ref$3c$Int$3e$* index$622;
  int32_t val$1987;
  if (_cnt$2755 > 1) {
    int32_t _new_cnt$2756 = _cnt$2755 - 1;
    Moonbit_object_header(_casted_env$1986)->rc = _new_cnt$2756;
    moonbit_incref(self$621);
    moonbit_incref(_field$2553);
  } else if (_cnt$2755 == 1) {
    moonbit_free(_casted_env$1986);
  }
  index$622 = _field$2553;
  val$1987 = index$622->$0;
  if (val$1987 < len$620) {
    int32_t val$1997 = index$622->$0;
    int32_t c1$623 = self$621[val$1997];
    int32_t _if_result$2846;
    int32_t val$1995;
    int32_t _tmp$1994;
    int32_t _tmp$1996;
    if (55296 <= c1$623 && c1$623 <= 56319) {
      int32_t val$1989 = index$622->$0;
      int32_t _tmp$1988 = val$1989 + 1;
      _if_result$2846 = _tmp$1988 < len$620;
    } else {
      _if_result$2846 = 0;
    }
    if (_if_result$2846) {
      int32_t val$1993 = index$622->$0;
      int32_t _tmp$1992 = val$1993 + 1;
      int32_t _tmp$2552 = self$621[_tmp$1992];
      int32_t c2$624;
      moonbit_decref(self$621);
      c2$624 = _tmp$2552;
      if (56320 <= c2$624 && c2$624 <= 57343) {
        int32_t c$625 =
          $moonbitlang$core$builtin$code_point_of_surrogate_pair(
            c1$623, c2$624
          );
        int32_t val$1991 = index$622->$0;
        int32_t _tmp$1990 = val$1991 + 2;
        index$622->$0 = _tmp$1990;
        moonbit_decref(index$622);
        return c$625;
      }
    } else {
      moonbit_decref(self$621);
    }
    val$1995 = index$622->$0;
    _tmp$1994 = val$1995 + 1;
    index$622->$0 = _tmp$1994;
    moonbit_decref(index$622);
    _tmp$1996 = c1$623;
    return _tmp$1996;
  } else {
    moonbit_decref(index$622);
    moonbit_decref(self$621);
    return -1;
  }
}

struct $StringView $String$$trim(
  moonbit_string_t self$619,
  void* chars$opt$617
) {
  struct $StringView chars$616;
  switch (Moonbit_object_tag(chars$opt$617)) {
    case 1: {
      struct $Option$3c$StringView$3e$$Some* _Some$618 =
        (struct $Option$3c$StringView$3e$$Some*)chars$opt$617;
      struct $StringView _field$2555 =
        (struct $StringView){
          _Some$618->$0_1, _Some$618->$0_2, _Some$618->$0_0
        };
      int32_t _cnt$2757 = Moonbit_object_header(_Some$618)->rc;
      if (_cnt$2757 > 1) {
        int32_t _new_cnt$2758 = _cnt$2757 - 1;
        Moonbit_object_header(_Some$618)->rc = _new_cnt$2758;
        moonbit_incref(_field$2555.$0);
      } else if (_cnt$2757 == 1) {
        moonbit_free(_Some$618);
      }
      chars$616 = _field$2555;
      break;
    }
    default: {
      int32_t _tmp$1984;
      moonbit_decref(chars$opt$617);
      _tmp$1984
      = Moonbit_array_length(
        $moonbitlang$core$builtin$trim$$2a$bind$7c$6141
      );
      moonbit_incref($moonbitlang$core$builtin$trim$$2a$bind$7c$6141);
      chars$616
      = (struct $StringView){
        0, _tmp$1984, $moonbitlang$core$builtin$trim$$2a$bind$7c$6141
      };
      break;
    }
  }
  return $String$$trim$inner(self$619, chars$616);
}

struct $StringView $String$$trim$inner(
  moonbit_string_t self$614,
  struct $StringView chars$615
) {
  int32_t _tmp$1983 = Moonbit_array_length(self$614);
  struct $StringView _tmp$1982 = (struct $StringView){0, _tmp$1983, self$614};
  return $StringView$$trim$inner(_tmp$1982, chars$615);
}

struct $StringView $StringView$$trim$inner(
  struct $StringView self$612,
  struct $StringView chars$613
) {
  struct $StringView _tmp$1981;
  moonbit_incref(chars$613.$0);
  _tmp$1981 = $StringView$$trim_start$inner(self$612, chars$613);
  return $StringView$$trim_end$inner(_tmp$1981, chars$613);
}

struct $StringView $StringView$$trim_end$inner(
  struct $StringView self$611,
  struct $StringView chars$609
) {
  struct $StringView _param$606 = self$611;
  while (1) {
    moonbit_string_t _field$2560 = _param$606.$0;
    moonbit_string_t str$1962 = _field$2560;
    int32_t start$1963 = _param$606.$1;
    int32_t end$1965 = _param$606.$2;
    int64_t _tmp$1964 = (int64_t)end$1965;
    moonbit_incref(str$1962);
    if ($String$$char_length_eq$inner(str$1962, 0, start$1963, _tmp$1964)) {
      moonbit_decref(chars$609.$0);
      return _param$606;
    } else {
      moonbit_string_t _field$2559 = _param$606.$0;
      moonbit_string_t str$1974 = _field$2559;
      moonbit_string_t _field$2558 = _param$606.$0;
      moonbit_string_t str$1977 = _field$2558;
      int32_t start$1978 = _param$606.$1;
      int32_t end$1980 = _param$606.$2;
      int64_t _tmp$1979 = (int64_t)end$1980;
      int64_t _tmp$1976;
      int32_t _tmp$1975;
      int32_t _c$607;
      moonbit_string_t _field$2557;
      moonbit_string_t str$1966;
      int32_t start$1967;
      moonbit_string_t _field$2556;
      moonbit_string_t str$1970;
      int32_t start$1971;
      int32_t end$1973;
      int64_t _tmp$1972;
      int64_t _tmp$1969;
      int32_t _tmp$1968;
      struct $StringView _x$608;
      moonbit_incref(str$1977);
      moonbit_incref(str$1974);
      _tmp$1976
      = $String$$offset_of_nth_char$inner(
        str$1977, -1, start$1978, _tmp$1979
      );
      _tmp$1975 = (int32_t)_tmp$1976;
      _c$607 = $String$$unsafe_char_at(str$1974, _tmp$1975);
      _field$2557 = _param$606.$0;
      str$1966 = _field$2557;
      start$1967 = _param$606.$1;
      _field$2556 = _param$606.$0;
      str$1970 = _field$2556;
      start$1971 = _param$606.$1;
      end$1973 = _param$606.$2;
      _tmp$1972 = (int64_t)end$1973;
      moonbit_incref(str$1970);
      moonbit_incref(str$1966);
      _tmp$1969
      = $String$$offset_of_nth_char$inner(
        str$1970, -1, start$1971, _tmp$1972
      );
      _tmp$1968 = (int32_t)_tmp$1969;
      _x$608 = (struct $StringView){start$1967, _tmp$1968, str$1966};
      moonbit_incref(chars$609.$0);
      if ($StringView$$contains_char(chars$609, _c$607)) {
        moonbit_decref(_param$606.$0);
        _param$606 = _x$608;
        continue;
      } else {
        moonbit_decref(chars$609.$0);
        moonbit_decref(_x$608.$0);
        return _param$606;
      }
    }
    break;
  }
}

struct $StringView $StringView$$trim_start$inner(
  struct $StringView self$605,
  struct $StringView chars$603
) {
  struct $StringView _param$599 = self$605;
  while (1) {
    moonbit_string_t _field$2565 = _param$599.$0;
    moonbit_string_t str$1944 = _field$2565;
    int32_t start$1945 = _param$599.$1;
    int32_t end$1947 = _param$599.$2;
    int64_t _tmp$1946 = (int64_t)end$1947;
    moonbit_incref(str$1944);
    if ($String$$char_length_eq$inner(str$1944, 0, start$1945, _tmp$1946)) {
      moonbit_decref(chars$603.$0);
      return _param$599;
    } else {
      moonbit_string_t _field$2564 = _param$599.$0;
      moonbit_string_t str$1955 = _field$2564;
      moonbit_string_t _field$2563 = _param$599.$0;
      moonbit_string_t str$1958 = _field$2563;
      int32_t start$1959 = _param$599.$1;
      int32_t end$1961 = _param$599.$2;
      int64_t _tmp$1960 = (int64_t)end$1961;
      int64_t _tmp$1957;
      int32_t _tmp$1956;
      int32_t _c$600;
      moonbit_string_t _field$2562;
      moonbit_string_t str$1948;
      moonbit_string_t _field$2561;
      moonbit_string_t str$1951;
      int32_t start$1952;
      int32_t end$1954;
      int64_t _tmp$1953;
      int64_t _bind$903;
      int32_t _tmp$1949;
      int32_t end$1950;
      struct $StringView _x$601;
      moonbit_incref(str$1958);
      moonbit_incref(str$1955);
      _tmp$1957
      = $String$$offset_of_nth_char$inner(
        str$1958, 0, start$1959, _tmp$1960
      );
      _tmp$1956 = (int32_t)_tmp$1957;
      _c$600 = $String$$unsafe_char_at(str$1955, _tmp$1956);
      _field$2562 = _param$599.$0;
      str$1948 = _field$2562;
      _field$2561 = _param$599.$0;
      str$1951 = _field$2561;
      start$1952 = _param$599.$1;
      end$1954 = _param$599.$2;
      _tmp$1953 = (int64_t)end$1954;
      moonbit_incref(str$1951);
      moonbit_incref(str$1948);
      _bind$903
      = $String$$offset_of_nth_char$inner(
        str$1951, 1, start$1952, _tmp$1953
      );
      if (_bind$903 == 4294967296ll) {
        _tmp$1949 = _param$599.$2;
      } else {
        int64_t _Some$602 = _bind$903;
        _tmp$1949 = (int32_t)_Some$602;
      }
      end$1950 = _param$599.$2;
      _x$601 = (struct $StringView){_tmp$1949, end$1950, str$1948};
      moonbit_incref(chars$603.$0);
      if ($StringView$$contains_char(chars$603, _c$600)) {
        moonbit_decref(_param$599.$0);
        _param$599 = _x$601;
        continue;
      } else {
        moonbit_decref(chars$603.$0);
        moonbit_decref(_x$601.$0);
        return _param$599;
      }
    }
    break;
  }
}

int32_t $StringView$$contains_char(
  struct $StringView self$589,
  int32_t c$591
) {
  int32_t end$1942 = self$589.$2;
  int32_t start$1943 = self$589.$1;
  int32_t len$588 = end$1942 - start$1943;
  if (len$588 > 0) {
    int32_t c$590 = c$591;
    if (c$590 <= 65535) {
      int32_t i$592 = 0;
      while (1) {
        if (i$592 < len$588) {
          moonbit_string_t _field$2567 = self$589.$0;
          moonbit_string_t str$1924 = _field$2567;
          int32_t start$1926 = self$589.$1;
          int32_t _tmp$1925 = start$1926 + i$592;
          int32_t _tmp$2566 = str$1924[_tmp$1925];
          int32_t _tmp$1923 = _tmp$2566;
          int32_t _tmp$1927;
          if (_tmp$1923 == c$590) {
            moonbit_decref(self$589.$0);
            return 1;
          }
          _tmp$1927 = i$592 + 1;
          i$592 = _tmp$1927;
          continue;
        } else {
          moonbit_decref(self$589.$0);
        }
        break;
      }
    } else if (len$588 >= 2) {
      int32_t adj$594 = c$590 - 65536;
      int32_t _tmp$1941 = adj$594 >> 10;
      int32_t high$595 = 55296 + _tmp$1941;
      int32_t _tmp$1940 = adj$594 & 1023;
      int32_t low$596 = 56320 + _tmp$1940;
      int32_t i$597 = 0;
      while (1) {
        int32_t _tmp$1928 = i$597;
        int32_t _tmp$1929 = len$588 - 1;
        if (_tmp$1928 < _tmp$1929) {
          int32_t _p$1148 = i$597;
          moonbit_string_t _field$2571 = self$589.$0;
          moonbit_string_t str$1931 = _field$2571;
          int32_t start$1933 = self$589.$1;
          int32_t _tmp$1932 = start$1933 + _p$1148;
          int32_t _tmp$2570 = str$1931[_tmp$1932];
          int32_t _tmp$1930 = _tmp$2570;
          int32_t _tmp$1939;
          if (_tmp$1930 == high$595) {
            int32_t _tmp$1934 = i$597;
            int32_t _p$1151;
            moonbit_string_t _field$2569;
            moonbit_string_t str$1936;
            int32_t start$1938;
            int32_t _tmp$1937;
            int32_t _tmp$2568;
            int32_t _tmp$1935;
            i$597 = _tmp$1934 + 1;
            _p$1151 = i$597;
            _field$2569 = self$589.$0;
            str$1936 = _field$2569;
            start$1938 = self$589.$1;
            _tmp$1937 = start$1938 + _p$1151;
            _tmp$2568 = str$1936[_tmp$1937];
            _tmp$1935 = _tmp$2568;
            if (_tmp$1935 == low$596) {
              moonbit_decref(self$589.$0);
              return 1;
            }
          }
          _tmp$1939 = i$597;
          i$597 = _tmp$1939 + 1;
          continue;
        } else {
          moonbit_decref(self$589.$0);
        }
        break;
      }
    } else {
      moonbit_decref(self$589.$0);
      return 0;
    }
    return 0;
  } else {
    moonbit_decref(self$589.$0);
    return 0;
  }
}

struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* $$moonbitlang$core$builtin$Iter$$fold$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* self$583,
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* init$581,
  struct $$3c$$moonbitlang$core$builtin$Array$3c$Char$3e$$2a$Char$3e$$3d$$3e$$moonbitlang$core$builtin$Array$3c$Char$3e$* f$586
) {
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* acc$580 = init$581;
  while (1) {
    int32_t _bind$582;
    moonbit_incref(self$583);
    _bind$582 = $$moonbitlang$core$builtin$Iter$$next$0(self$583);
    if (_bind$582 == -1) {
      moonbit_decref(f$586);
      moonbit_decref(self$583);
    } else {
      int32_t _Some$584 = _bind$582;
      int32_t _x$585 = _Some$584;
      struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* _tmp$1922 =
        acc$580;
      moonbit_incref(f$586);
      acc$580 = f$586->code(f$586, _tmp$1922, _x$585);
      continue;
    }
    break;
  }
  return acc$580;
}

struct $$3c$Int$2a$Char$3e$* $$moonbitlang$core$builtin$Iter$$next$1(
  struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$* self$579
) {
  struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$* _func$578 =
    self$579;
  return _func$578->code(_func$578);
}

int32_t $$moonbitlang$core$builtin$Iter$$next$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* self$577
) {
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* _func$576 = self$577;
  return _func$576->code(_func$576);
}

int32_t $$moonbitlang$core$builtin$Array$$push$4(
  struct $$moonbitlang$core$builtin$Array$3c$Byte$3e$* self$573,
  int32_t value$575
) {
  int32_t len$1917 = self$573->$1;
  moonbit_bytes_t _field$2574 = self$573->$0;
  moonbit_bytes_t buf$1919 = _field$2574;
  int32_t _tmp$2573 = Moonbit_array_length(buf$1919);
  int32_t _tmp$1918 = _tmp$2573;
  int32_t length$574;
  moonbit_bytes_t _field$2572;
  moonbit_bytes_t buf$1920;
  int32_t _tmp$1921;
  if (len$1917 == _tmp$1918) {
    moonbit_incref(self$573);
    $$moonbitlang$core$builtin$Array$$realloc$4(self$573);
  }
  length$574 = self$573->$1;
  _field$2572 = self$573->$0;
  buf$1920 = _field$2572;
  buf$1920[length$574] = value$575;
  _tmp$1921 = length$574 + 1;
  self$573->$1 = _tmp$1921;
  moonbit_decref(self$573);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$3(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$570,
  int32_t value$572
) {
  int32_t len$1912 = self$570->$1;
  int32_t* _field$2577 = self$570->$0;
  int32_t* buf$1914 = _field$2577;
  int32_t _tmp$2576 = Moonbit_array_length(buf$1914);
  int32_t _tmp$1913 = _tmp$2576;
  int32_t length$571;
  int32_t* _field$2575;
  int32_t* buf$1915;
  int32_t _tmp$1916;
  if (len$1912 == _tmp$1913) {
    moonbit_incref(self$570);
    $$moonbitlang$core$builtin$Array$$realloc$3(self$570);
  }
  length$571 = self$570->$1;
  _field$2575 = self$570->$0;
  buf$1915 = _field$2575;
  buf$1915[length$571] = value$572;
  _tmp$1916 = length$571 + 1;
  self$570->$1 = _tmp$1916;
  moonbit_decref(self$570);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$2(
  struct $$moonbitlang$core$builtin$Array$3c$QueryDef$3e$* self$567,
  struct $QueryDef* value$569
) {
  int32_t len$1907 = self$567->$1;
  struct $QueryDef** _field$2581 = self$567->$0;
  struct $QueryDef** buf$1909 = _field$2581;
  int32_t _tmp$2580 = Moonbit_array_length(buf$1909);
  int32_t _tmp$1908 = _tmp$2580;
  int32_t length$568;
  struct $QueryDef** _field$2579;
  struct $QueryDef** buf$1910;
  struct $QueryDef* _old$2578;
  int32_t _tmp$1911;
  if (len$1907 == _tmp$1908) {
    moonbit_incref(self$567);
    $$moonbitlang$core$builtin$Array$$realloc$2(self$567);
  }
  length$568 = self$567->$1;
  _field$2579 = self$567->$0;
  buf$1910 = _field$2579;
  _old$2578 = (struct $QueryDef*)buf$1910[length$568];
  if (_old$2578) {
    moonbit_decref(_old$2578);
  }
  buf$1910[length$568] = value$569;
  _tmp$1911 = length$568 + 1;
  self$567->$1 = _tmp$1911;
  moonbit_decref(self$567);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$564,
  moonbit_string_t value$566
) {
  int32_t len$1902 = self$564->$1;
  moonbit_string_t* _field$2585 = self$564->$0;
  moonbit_string_t* buf$1904 = _field$2585;
  int32_t _tmp$2584 = Moonbit_array_length(buf$1904);
  int32_t _tmp$1903 = _tmp$2584;
  int32_t length$565;
  moonbit_string_t* _field$2583;
  moonbit_string_t* buf$1905;
  moonbit_string_t _old$2582;
  int32_t _tmp$1906;
  if (len$1902 == _tmp$1903) {
    moonbit_incref(self$564);
    $$moonbitlang$core$builtin$Array$$realloc$1(self$564);
  }
  length$565 = self$564->$1;
  _field$2583 = self$564->$0;
  buf$1905 = _field$2583;
  _old$2582 = (moonbit_string_t)buf$1905[length$565];
  moonbit_decref(_old$2582);
  buf$1905[length$565] = value$566;
  _tmp$1906 = length$565 + 1;
  self$564->$1 = _tmp$1906;
  moonbit_decref(self$564);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* self$561,
  struct $$3c$String$2a$String$3e$* value$563
) {
  int32_t len$1897 = self$561->$1;
  struct $$3c$String$2a$String$3e$** _field$2589 = self$561->$0;
  struct $$3c$String$2a$String$3e$** buf$1899 = _field$2589;
  int32_t _tmp$2588 = Moonbit_array_length(buf$1899);
  int32_t _tmp$1898 = _tmp$2588;
  int32_t length$562;
  struct $$3c$String$2a$String$3e$** _field$2587;
  struct $$3c$String$2a$String$3e$** buf$1900;
  struct $$3c$String$2a$String$3e$* _old$2586;
  int32_t _tmp$1901;
  if (len$1897 == _tmp$1898) {
    moonbit_incref(self$561);
    $$moonbitlang$core$builtin$Array$$realloc$0(self$561);
  }
  length$562 = self$561->$1;
  _field$2587 = self$561->$0;
  buf$1900 = _field$2587;
  _old$2586 = (struct $$3c$String$2a$String$3e$*)buf$1900[length$562];
  if (_old$2586) {
    moonbit_decref(_old$2586);
  }
  buf$1900[length$562] = value$563;
  _tmp$1901 = length$562 + 1;
  self$561->$1 = _tmp$1901;
  moonbit_decref(self$561);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$4(
  struct $$moonbitlang$core$builtin$Array$3c$Byte$3e$* self$559
) {
  int32_t old_cap$558 = self$559->$1;
  int32_t new_cap$560;
  if (old_cap$558 == 0) {
    new_cap$560 = 8;
  } else {
    new_cap$560 = old_cap$558 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$4(self$559, new_cap$560);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$3(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$556
) {
  int32_t old_cap$555 = self$556->$1;
  int32_t new_cap$557;
  if (old_cap$555 == 0) {
    new_cap$557 = 8;
  } else {
    new_cap$557 = old_cap$555 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$3(self$556, new_cap$557);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$2(
  struct $$moonbitlang$core$builtin$Array$3c$QueryDef$3e$* self$553
) {
  int32_t old_cap$552 = self$553->$1;
  int32_t new_cap$554;
  if (old_cap$552 == 0) {
    new_cap$554 = 8;
  } else {
    new_cap$554 = old_cap$552 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$2(self$553, new_cap$554);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$550
) {
  int32_t old_cap$549 = self$550->$1;
  int32_t new_cap$551;
  if (old_cap$549 == 0) {
    new_cap$551 = 8;
  } else {
    new_cap$551 = old_cap$549 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$1(self$550, new_cap$551);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* self$547
) {
  int32_t old_cap$546 = self$547->$1;
  int32_t new_cap$548;
  if (old_cap$546 == 0) {
    new_cap$548 = 8;
  } else {
    new_cap$548 = old_cap$546 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$0(self$547, new_cap$548);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$4(
  struct $$moonbitlang$core$builtin$Array$3c$Byte$3e$* self$543,
  int32_t new_capacity$541
) {
  moonbit_bytes_t new_buf$540 =
    (moonbit_bytes_t)moonbit_make_bytes_raw(new_capacity$541);
  moonbit_bytes_t _field$2591 = self$543->$0;
  moonbit_bytes_t old_buf$542 = _field$2591;
  int32_t old_cap$544 = Moonbit_array_length(old_buf$542);
  int32_t copy_len$545;
  moonbit_bytes_t _old$2590;
  if (old_cap$544 < new_capacity$541) {
    copy_len$545 = old_cap$544;
  } else {
    copy_len$545 = new_capacity$541;
  }
  moonbit_incref(old_buf$542);
  moonbit_incref(new_buf$540);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$4(
    new_buf$540, 0, old_buf$542, 0, copy_len$545
  );
  _old$2590 = self$543->$0;
  moonbit_decref(_old$2590);
  self$543->$0 = new_buf$540;
  moonbit_decref(self$543);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$3(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$537,
  int32_t new_capacity$535
) {
  int32_t* new_buf$534 =
    (int32_t*)moonbit_make_int32_array_raw(new_capacity$535);
  int32_t* _field$2593 = self$537->$0;
  int32_t* old_buf$536 = _field$2593;
  int32_t old_cap$538 = Moonbit_array_length(old_buf$536);
  int32_t copy_len$539;
  int32_t* _old$2592;
  if (old_cap$538 < new_capacity$535) {
    copy_len$539 = old_cap$538;
  } else {
    copy_len$539 = new_capacity$535;
  }
  moonbit_incref(old_buf$536);
  moonbit_incref(new_buf$534);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$3(
    new_buf$534, 0, old_buf$536, 0, copy_len$539
  );
  _old$2592 = self$537->$0;
  moonbit_decref(_old$2592);
  self$537->$0 = new_buf$534;
  moonbit_decref(self$537);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$QueryDef$3e$* self$531,
  int32_t new_capacity$529
) {
  struct $QueryDef** new_buf$528 =
    (struct $QueryDef**)moonbit_make_ref_array(new_capacity$529, 0);
  struct $QueryDef** _field$2595 = self$531->$0;
  struct $QueryDef** old_buf$530 = _field$2595;
  int32_t old_cap$532 = Moonbit_array_length(old_buf$530);
  int32_t copy_len$533;
  struct $QueryDef** _old$2594;
  if (old_cap$532 < new_capacity$529) {
    copy_len$533 = old_cap$532;
  } else {
    copy_len$533 = new_capacity$529;
  }
  moonbit_incref(old_buf$530);
  moonbit_incref(new_buf$528);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
    new_buf$528, 0, old_buf$530, 0, copy_len$533
  );
  _old$2594 = self$531->$0;
  moonbit_decref(_old$2594);
  self$531->$0 = new_buf$528;
  moonbit_decref(self$531);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$525,
  int32_t new_capacity$523
) {
  moonbit_string_t* new_buf$522 =
    (moonbit_string_t*)moonbit_make_ref_array(
      new_capacity$523, (moonbit_string_t)moonbit_string_literal_111.data
    );
  moonbit_string_t* _field$2597 = self$525->$0;
  moonbit_string_t* old_buf$524 = _field$2597;
  int32_t old_cap$526 = Moonbit_array_length(old_buf$524);
  int32_t copy_len$527;
  moonbit_string_t* _old$2596;
  if (old_cap$526 < new_capacity$523) {
    copy_len$527 = old_cap$526;
  } else {
    copy_len$527 = new_capacity$523;
  }
  moonbit_incref(old_buf$524);
  moonbit_incref(new_buf$522);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
    new_buf$522, 0, old_buf$524, 0, copy_len$527
  );
  _old$2596 = self$525->$0;
  moonbit_decref(_old$2596);
  self$525->$0 = new_buf$522;
  moonbit_decref(self$525);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* self$519,
  int32_t new_capacity$517
) {
  struct $$3c$String$2a$String$3e$** new_buf$516 =
    (struct $$3c$String$2a$String$3e$**)moonbit_make_ref_array(
      new_capacity$517, 0
    );
  struct $$3c$String$2a$String$3e$** _field$2599 = self$519->$0;
  struct $$3c$String$2a$String$3e$** old_buf$518 = _field$2599;
  int32_t old_cap$520 = Moonbit_array_length(old_buf$518);
  int32_t copy_len$521;
  struct $$3c$String$2a$String$3e$** _old$2598;
  if (old_cap$520 < new_capacity$517) {
    copy_len$521 = old_cap$520;
  } else {
    copy_len$521 = new_capacity$517;
  }
  moonbit_incref(old_buf$518);
  moonbit_incref(new_buf$516);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
    new_buf$516, 0, old_buf$518, 0, copy_len$521
  );
  _old$2598 = self$519->$0;
  moonbit_decref(_old$2598);
  self$519->$0 = new_buf$516;
  moonbit_decref(self$519);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$515
) {
  if (capacity$515 == 0) {
    int32_t* _tmp$1895 = (int32_t*)moonbit_empty_int32_array;
    struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* _block$2852 =
      (struct $$moonbitlang$core$builtin$Array$3c$Char$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$Char$3e$)
      );
    Moonbit_object_header(_block$2852)->meta
    = Moonbit_make_regular_object_header(
      offsetof(struct $$moonbitlang$core$builtin$Array$3c$Char$3e$, $0) >> 2,
        1,
        0
    );
    _block$2852->$0 = _tmp$1895;
    _block$2852->$1 = 0;
    return _block$2852;
  } else {
    int32_t* _tmp$1896 = (int32_t*)moonbit_make_int32_array_raw(capacity$515);
    struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* _block$2853 =
      (struct $$moonbitlang$core$builtin$Array$3c$Char$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$Char$3e$)
      );
    Moonbit_object_header(_block$2853)->meta
    = Moonbit_make_regular_object_header(
      offsetof(struct $$moonbitlang$core$builtin$Array$3c$Char$3e$, $0) >> 2,
        1,
        0
    );
    _block$2853->$0 = _tmp$1896;
    _block$2853->$1 = 0;
    return _block$2853;
  }
}

int32_t $String$$has_prefix(
  moonbit_string_t self$513,
  struct $StringView str$514
) {
  int32_t _tmp$1894 = Moonbit_array_length(self$513);
  struct $StringView _tmp$1893 = (struct $StringView){0, _tmp$1894, self$513};
  return $StringView$$has_prefix(_tmp$1893, str$514);
}

int32_t $StringView$$has_prefix(
  struct $StringView self$509,
  struct $StringView str$510
) {
  int64_t _bind$508 = $StringView$$find(self$509, str$510);
  if (_bind$508 == 4294967296ll) {
    return 0;
  } else {
    int64_t _Some$511 = _bind$508;
    int32_t _i$512 = (int32_t)_Some$511;
    return _i$512 == 0;
  }
}

int32_t $String$$has_suffix(
  moonbit_string_t self$506,
  struct $StringView str$507
) {
  int32_t _tmp$1892 = Moonbit_array_length(self$506);
  struct $StringView _tmp$1891 = (struct $StringView){0, _tmp$1892, self$506};
  return $StringView$$has_suffix(_tmp$1891, str$507);
}

int32_t $StringView$$has_suffix(
  struct $StringView self$502,
  struct $StringView str$503
) {
  int64_t _bind$501;
  moonbit_incref(str$503.$0);
  moonbit_incref(self$502.$0);
  _bind$501 = $StringView$$rev_find(self$502, str$503);
  if (_bind$501 == 4294967296ll) {
    moonbit_decref(str$503.$0);
    moonbit_decref(self$502.$0);
    return 0;
  } else {
    int64_t _Some$504 = _bind$501;
    int32_t _i$505 = (int32_t)_Some$504;
    int32_t end$1889 = self$502.$2;
    int32_t _field$2601 = self$502.$1;
    int32_t start$1890;
    int32_t _tmp$1885;
    int32_t end$1887;
    int32_t _field$2600;
    int32_t start$1888;
    int32_t _tmp$1886;
    int32_t _tmp$1884;
    moonbit_decref(self$502.$0);
    start$1890 = _field$2601;
    _tmp$1885 = end$1889 - start$1890;
    end$1887 = str$503.$2;
    _field$2600 = str$503.$1;
    moonbit_decref(str$503.$0);
    start$1888 = _field$2600;
    _tmp$1886 = end$1887 - start$1888;
    _tmp$1884 = _tmp$1885 - _tmp$1886;
    return _i$505 == _tmp$1884;
  }
}

int64_t $StringView$$rev_find(
  struct $StringView self$500,
  struct $StringView str$499
) {
  int32_t end$1882 = str$499.$2;
  int32_t start$1883 = str$499.$1;
  int32_t _tmp$1881 = end$1882 - start$1883;
  if (_tmp$1881 <= 4) {
    return $moonbitlang$core$builtin$brute_force_rev_find(self$500, str$499);
  } else {
    return $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
             self$500, str$499
           );
  }
}

int64_t $moonbitlang$core$builtin$brute_force_rev_find(
  struct $StringView haystack$490,
  struct $StringView needle$492
) {
  int32_t end$1879 = haystack$490.$2;
  int32_t start$1880 = haystack$490.$1;
  int32_t haystack_len$489 = end$1879 - start$1880;
  int32_t end$1877 = needle$492.$2;
  int32_t start$1878 = needle$492.$1;
  int32_t needle_len$491 = end$1877 - start$1878;
  if (needle_len$491 > 0) {
    if (haystack_len$489 >= needle_len$491) {
      int32_t _p$1117 = 0;
      moonbit_string_t _field$2609 = needle$492.$0;
      moonbit_string_t str$1874 = _field$2609;
      int32_t start$1876 = needle$492.$1;
      int32_t _tmp$1875 = start$1876 + _p$1117;
      int32_t _tmp$2608 = str$1874[_tmp$1875];
      int32_t needle_first$493 = _tmp$2608;
      int32_t i$494 = haystack_len$489 - needle_len$491;
      while (1) {
        int32_t _tmp$1854 = i$494;
        if (_tmp$1854 >= 0) {
          int32_t _tmp$1861;
          while (1) {
            int32_t _tmp$1859 = i$494;
            int32_t _if_result$2856;
            if (_tmp$1859 >= 0) {
              int32_t _p$1120 = i$494;
              moonbit_string_t _field$2607 = haystack$490.$0;
              moonbit_string_t str$1856 = _field$2607;
              int32_t start$1858 = haystack$490.$1;
              int32_t _tmp$1857 = start$1858 + _p$1120;
              int32_t _tmp$2606 = str$1856[_tmp$1857];
              int32_t _tmp$1855 = _tmp$2606;
              _if_result$2856 = _tmp$1855 != needle_first$493;
            } else {
              _if_result$2856 = 0;
            }
            if (_if_result$2856) {
              int32_t _tmp$1860 = i$494;
              i$494 = _tmp$1860 - 1;
              continue;
            }
            break;
          }
          _tmp$1861 = i$494;
          if (_tmp$1861 >= 0) {
            int32_t j$496 = 1;
            int32_t _tmp$1873;
            while (1) {
              if (j$496 < needle_len$491) {
                int32_t _tmp$1870 = i$494;
                int32_t _p$1123 = _tmp$1870 + j$496;
                moonbit_string_t _field$2605 = haystack$490.$0;
                moonbit_string_t str$1867 = _field$2605;
                int32_t start$1869 = haystack$490.$1;
                int32_t _tmp$1868 = start$1869 + _p$1123;
                int32_t _tmp$2604 = str$1867[_tmp$1868];
                int32_t _tmp$1862 = _tmp$2604;
                moonbit_string_t _field$2603 = needle$492.$0;
                moonbit_string_t str$1864 = _field$2603;
                int32_t start$1866 = needle$492.$1;
                int32_t _tmp$1865 = start$1866 + j$496;
                int32_t _tmp$2602 = str$1864[_tmp$1865];
                int32_t _tmp$1863 = _tmp$2602;
                int32_t _tmp$1871;
                if (_tmp$1862 != _tmp$1863) {
                  break;
                }
                _tmp$1871 = j$496 + 1;
                j$496 = _tmp$1871;
                continue;
              } else {
                int32_t _tmp$1872;
                moonbit_decref(needle$492.$0);
                moonbit_decref(haystack$490.$0);
                _tmp$1872 = i$494;
                return (int64_t)_tmp$1872;
              }
              break;
            }
            _tmp$1873 = i$494;
            i$494 = _tmp$1873 - 1;
          }
          continue;
        } else {
          moonbit_decref(needle$492.$0);
          moonbit_decref(haystack$490.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$492.$0);
      moonbit_decref(haystack$490.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$492.$0);
    moonbit_decref(haystack$490.$0);
    return (int64_t)haystack_len$489;
  }
}

int64_t $String$$find_by(
  moonbit_string_t self$487,
  struct $$3c$Char$3e$$3d$$3e$Bool* pred$488
) {
  int32_t _tmp$1853 = Moonbit_array_length(self$487);
  struct $StringView _tmp$1852 = (struct $StringView){0, _tmp$1853, self$487};
  return $StringView$$find_by(_tmp$1852, pred$488);
}

int64_t $StringView$$find_by(
  struct $StringView self$479,
  struct $$3c$Char$3e$$3d$$3e$Bool* pred$485
) {
  struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$* _it$478 =
    $StringView$$iter2(self$479);
  while (1) {
    struct $$3c$Int$2a$Char$3e$* _bind$480;
    moonbit_incref(_it$478);
    _bind$480 = $$moonbitlang$core$builtin$Iter2$$next$0(_it$478);
    if (_bind$480 == 0) {
      moonbit_decref(pred$485);
      if (_bind$480) {
        moonbit_decref(_bind$480);
      }
      moonbit_decref(_it$478);
    } else {
      struct $$3c$Int$2a$Char$3e$* _Some$481 = _bind$480;
      struct $$3c$Int$2a$Char$3e$* _x$482 = _Some$481;
      int32_t _i$483 = _x$482->$0;
      int32_t _field$2610 = _x$482->$1;
      int32_t _c$484;
      moonbit_decref(_x$482);
      _c$484 = _field$2610;
      moonbit_incref(pred$485);
      if (pred$485->code(pred$485, _c$484)) {
        moonbit_decref(pred$485);
        moonbit_decref(_it$478);
        return (int64_t)_i$483;
      }
      continue;
    }
    break;
  }
  return 4294967296ll;
}

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
  struct $StringView haystack$468,
  struct $StringView needle$470
) {
  int32_t end$1850 = haystack$468.$2;
  int32_t start$1851 = haystack$468.$1;
  int32_t haystack_len$467 = end$1850 - start$1851;
  int32_t end$1848 = needle$470.$2;
  int32_t start$1849 = needle$470.$1;
  int32_t needle_len$469 = end$1848 - start$1849;
  if (needle_len$469 > 0) {
    if (haystack_len$467 >= needle_len$469) {
      int32_t* skip_table$471 =
        (int32_t*)moonbit_make_int32_array(256, needle_len$469);
      int32_t _tmp$1830 = needle_len$469 - 1;
      int32_t i$472 = _tmp$1830;
      int32_t _tmp$1847;
      int32_t i$474;
      while (1) {
        if (i$472 > 0) {
          moonbit_string_t _field$2618 = needle$470.$0;
          moonbit_string_t str$1826 = _field$2618;
          int32_t start$1828 = needle$470.$1;
          int32_t _tmp$1827 = start$1828 + i$472;
          int32_t _tmp$2617 = str$1826[_tmp$1827];
          int32_t _tmp$1825 = _tmp$2617;
          int32_t _tmp$1824 = _tmp$1825 & 255;
          int32_t _tmp$1829;
          if (
            _tmp$1824 < 0
            || _tmp$1824 >= Moonbit_array_length(skip_table$471)
          ) {
            moonbit_panic();
          }
          skip_table$471[_tmp$1824] = i$472;
          _tmp$1829 = i$472 - 1;
          i$472 = _tmp$1829;
          continue;
        }
        break;
      }
      _tmp$1847 = haystack_len$467 - needle_len$469;
      i$474 = _tmp$1847;
      while (1) {
        if (i$474 >= 0) {
          int32_t j$475 = 0;
          moonbit_string_t _field$2612;
          moonbit_string_t str$1844;
          int32_t start$1846;
          int32_t _tmp$1845;
          int32_t _tmp$2611;
          int32_t _tmp$1843;
          int32_t _tmp$1842;
          int32_t _tmp$1841;
          int32_t _tmp$1840;
          while (1) {
            if (j$475 < needle_len$469) {
              int32_t _p$1106 = i$474 + j$475;
              moonbit_string_t _field$2616 = haystack$468.$0;
              moonbit_string_t str$1836 = _field$2616;
              int32_t start$1838 = haystack$468.$1;
              int32_t _tmp$1837 = start$1838 + _p$1106;
              int32_t _tmp$2615 = str$1836[_tmp$1837];
              int32_t _tmp$1831 = _tmp$2615;
              moonbit_string_t _field$2614 = needle$470.$0;
              moonbit_string_t str$1833 = _field$2614;
              int32_t start$1835 = needle$470.$1;
              int32_t _tmp$1834 = start$1835 + j$475;
              int32_t _tmp$2613 = str$1833[_tmp$1834];
              int32_t _tmp$1832 = _tmp$2613;
              int32_t _tmp$1839;
              if (_tmp$1831 != _tmp$1832) {
                break;
              }
              _tmp$1839 = j$475 + 1;
              j$475 = _tmp$1839;
              continue;
            } else {
              moonbit_decref(skip_table$471);
              moonbit_decref(needle$470.$0);
              moonbit_decref(haystack$468.$0);
              return (int64_t)i$474;
            }
            break;
          }
          _field$2612 = haystack$468.$0;
          str$1844 = _field$2612;
          start$1846 = haystack$468.$1;
          _tmp$1845 = start$1846 + i$474;
          _tmp$2611 = str$1844[_tmp$1845];
          _tmp$1843 = _tmp$2611;
          _tmp$1842 = _tmp$1843 & 255;
          if (
            _tmp$1842 < 0
            || _tmp$1842 >= Moonbit_array_length(skip_table$471)
          ) {
            moonbit_panic();
          }
          _tmp$1841 = (int32_t)skip_table$471[_tmp$1842];
          _tmp$1840 = i$474 - _tmp$1841;
          i$474 = _tmp$1840;
          continue;
        } else {
          moonbit_decref(skip_table$471);
          moonbit_decref(needle$470.$0);
          moonbit_decref(haystack$468.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$470.$0);
      moonbit_decref(haystack$468.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$470.$0);
    moonbit_decref(haystack$468.$0);
    return (int64_t)haystack_len$467;
  }
}

int64_t $StringView$$find(
  struct $StringView self$466,
  struct $StringView str$465
) {
  int32_t end$1822 = str$465.$2;
  int32_t start$1823 = str$465.$1;
  int32_t _tmp$1821 = end$1822 - start$1823;
  if (_tmp$1821 <= 4) {
    return $moonbitlang$core$builtin$brute_force_find(self$466, str$465);
  } else {
    return $moonbitlang$core$builtin$boyer_moore_horspool_find(
             self$466, str$465
           );
  }
}

int64_t $moonbitlang$core$builtin$brute_force_find(
  struct $StringView haystack$455,
  struct $StringView needle$457
) {
  int32_t end$1819 = haystack$455.$2;
  int32_t start$1820 = haystack$455.$1;
  int32_t haystack_len$454 = end$1819 - start$1820;
  int32_t end$1817 = needle$457.$2;
  int32_t start$1818 = needle$457.$1;
  int32_t needle_len$456 = end$1817 - start$1818;
  if (needle_len$456 > 0) {
    if (haystack_len$454 >= needle_len$456) {
      int32_t _p$1087 = 0;
      moonbit_string_t _field$2626 = needle$457.$0;
      moonbit_string_t str$1814 = _field$2626;
      int32_t start$1816 = needle$457.$1;
      int32_t _tmp$1815 = start$1816 + _p$1087;
      int32_t _tmp$2625 = str$1814[_tmp$1815];
      int32_t needle_first$458 = _tmp$2625;
      int32_t forward_len$459 = haystack_len$454 - needle_len$456;
      int32_t i$460 = 0;
      while (1) {
        int32_t _tmp$1794 = i$460;
        if (_tmp$1794 <= forward_len$459) {
          int32_t _tmp$1801;
          while (1) {
            int32_t _tmp$1799 = i$460;
            int32_t _if_result$2864;
            if (_tmp$1799 <= forward_len$459) {
              int32_t _p$1090 = i$460;
              moonbit_string_t _field$2624 = haystack$455.$0;
              moonbit_string_t str$1796 = _field$2624;
              int32_t start$1798 = haystack$455.$1;
              int32_t _tmp$1797 = start$1798 + _p$1090;
              int32_t _tmp$2623 = str$1796[_tmp$1797];
              int32_t _tmp$1795 = _tmp$2623;
              _if_result$2864 = _tmp$1795 != needle_first$458;
            } else {
              _if_result$2864 = 0;
            }
            if (_if_result$2864) {
              int32_t _tmp$1800 = i$460;
              i$460 = _tmp$1800 + 1;
              continue;
            }
            break;
          }
          _tmp$1801 = i$460;
          if (_tmp$1801 <= forward_len$459) {
            int32_t j$462 = 1;
            int32_t _tmp$1813;
            while (1) {
              if (j$462 < needle_len$456) {
                int32_t _tmp$1810 = i$460;
                int32_t _p$1093 = _tmp$1810 + j$462;
                moonbit_string_t _field$2622 = haystack$455.$0;
                moonbit_string_t str$1807 = _field$2622;
                int32_t start$1809 = haystack$455.$1;
                int32_t _tmp$1808 = start$1809 + _p$1093;
                int32_t _tmp$2621 = str$1807[_tmp$1808];
                int32_t _tmp$1802 = _tmp$2621;
                moonbit_string_t _field$2620 = needle$457.$0;
                moonbit_string_t str$1804 = _field$2620;
                int32_t start$1806 = needle$457.$1;
                int32_t _tmp$1805 = start$1806 + j$462;
                int32_t _tmp$2619 = str$1804[_tmp$1805];
                int32_t _tmp$1803 = _tmp$2619;
                int32_t _tmp$1811;
                if (_tmp$1802 != _tmp$1803) {
                  break;
                }
                _tmp$1811 = j$462 + 1;
                j$462 = _tmp$1811;
                continue;
              } else {
                int32_t _tmp$1812;
                moonbit_decref(needle$457.$0);
                moonbit_decref(haystack$455.$0);
                _tmp$1812 = i$460;
                return (int64_t)_tmp$1812;
              }
              break;
            }
            _tmp$1813 = i$460;
            i$460 = _tmp$1813 + 1;
          }
          continue;
        } else {
          moonbit_decref(needle$457.$0);
          moonbit_decref(haystack$455.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$457.$0);
      moonbit_decref(haystack$455.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$457.$0);
    moonbit_decref(haystack$455.$0);
    return $moonbitlang$core$builtin$brute_force_find$constr$453;
  }
}

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find(
  struct $StringView haystack$441,
  struct $StringView needle$443
) {
  int32_t end$1792 = haystack$441.$2;
  int32_t start$1793 = haystack$441.$1;
  int32_t haystack_len$440 = end$1792 - start$1793;
  int32_t end$1790 = needle$443.$2;
  int32_t start$1791 = needle$443.$1;
  int32_t needle_len$442 = end$1790 - start$1791;
  if (needle_len$442 > 0) {
    if (haystack_len$440 >= needle_len$442) {
      int32_t* skip_table$444 =
        (int32_t*)moonbit_make_int32_array(256, needle_len$442);
      int32_t _end4061$445 = needle_len$442 - 1;
      int32_t i$446 = 0;
      int32_t i$448;
      while (1) {
        if (i$446 < _end4061$445) {
          moonbit_string_t _field$2634 = needle$443.$0;
          moonbit_string_t str$1768 = _field$2634;
          int32_t start$1770 = needle$443.$1;
          int32_t _tmp$1769 = start$1770 + i$446;
          int32_t _tmp$2633 = str$1768[_tmp$1769];
          int32_t _tmp$1767 = _tmp$2633;
          int32_t _tmp$1764 = _tmp$1767 & 255;
          int32_t _tmp$1766 = needle_len$442 - 1;
          int32_t _tmp$1765 = _tmp$1766 - i$446;
          int32_t _tmp$1771;
          if (
            _tmp$1764 < 0
            || _tmp$1764 >= Moonbit_array_length(skip_table$444)
          ) {
            moonbit_panic();
          }
          skip_table$444[_tmp$1764] = _tmp$1765;
          _tmp$1771 = i$446 + 1;
          i$446 = _tmp$1771;
          continue;
        }
        break;
      }
      i$448 = 0;
      while (1) {
        int32_t _tmp$1772 = haystack_len$440 - needle_len$442;
        if (i$448 <= _tmp$1772) {
          int32_t _end4067$449 = needle_len$442 - 1;
          int32_t j$450 = 0;
          int32_t _tmp$1789;
          int32_t _p$1080;
          moonbit_string_t _field$2628;
          moonbit_string_t str$1786;
          int32_t start$1788;
          int32_t _tmp$1787;
          int32_t _tmp$2627;
          int32_t _tmp$1785;
          int32_t _tmp$1784;
          int32_t _tmp$1783;
          int32_t _tmp$1782;
          while (1) {
            if (j$450 <= _end4067$449) {
              int32_t _p$1075 = i$448 + j$450;
              moonbit_string_t _field$2632 = haystack$441.$0;
              moonbit_string_t str$1778 = _field$2632;
              int32_t start$1780 = haystack$441.$1;
              int32_t _tmp$1779 = start$1780 + _p$1075;
              int32_t _tmp$2631 = str$1778[_tmp$1779];
              int32_t _tmp$1773 = _tmp$2631;
              moonbit_string_t _field$2630 = needle$443.$0;
              moonbit_string_t str$1775 = _field$2630;
              int32_t start$1777 = needle$443.$1;
              int32_t _tmp$1776 = start$1777 + j$450;
              int32_t _tmp$2629 = str$1775[_tmp$1776];
              int32_t _tmp$1774 = _tmp$2629;
              int32_t _tmp$1781;
              if (_tmp$1773 != _tmp$1774) {
                break;
              }
              _tmp$1781 = j$450 + 1;
              j$450 = _tmp$1781;
              continue;
            } else {
              moonbit_decref(skip_table$444);
              moonbit_decref(needle$443.$0);
              moonbit_decref(haystack$441.$0);
              return (int64_t)i$448;
            }
            break;
          }
          _tmp$1789 = i$448 + needle_len$442;
          _p$1080 = _tmp$1789 - 1;
          _field$2628 = haystack$441.$0;
          str$1786 = _field$2628;
          start$1788 = haystack$441.$1;
          _tmp$1787 = start$1788 + _p$1080;
          _tmp$2627 = str$1786[_tmp$1787];
          _tmp$1785 = _tmp$2627;
          _tmp$1784 = _tmp$1785 & 255;
          if (
            _tmp$1784 < 0
            || _tmp$1784 >= Moonbit_array_length(skip_table$444)
          ) {
            moonbit_panic();
          }
          _tmp$1783 = (int32_t)skip_table$444[_tmp$1784];
          _tmp$1782 = i$448 + _tmp$1783;
          i$448 = _tmp$1782;
          continue;
        } else {
          moonbit_decref(skip_table$444);
          moonbit_decref(needle$443.$0);
          moonbit_decref(haystack$441.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$443.$0);
      moonbit_decref(haystack$441.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$443.$0);
    moonbit_decref(haystack$441.$0);
    return $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$439;
  }
}

int32_t $$moonbitlang$core$builtin$StringBuilder$$reset(
  struct $$moonbitlang$core$builtin$StringBuilder* self$438
) {
  self$438->$1 = 0;
  moonbit_decref(self$438);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$436,
  struct $StringView str$437
) {
  int32_t len$1746 = self$436->$1;
  int32_t end$1749 = str$437.$2;
  int32_t start$1750 = str$437.$1;
  int32_t _tmp$1748 = end$1749 - start$1750;
  int32_t _tmp$1747 = _tmp$1748 * 2;
  int32_t _tmp$1745 = len$1746 + _tmp$1747;
  moonbit_bytes_t _field$2637;
  moonbit_bytes_t data$1751;
  int32_t len$1752;
  moonbit_string_t _field$2636;
  moonbit_string_t str$1753;
  int32_t start$1754;
  int32_t end$1756;
  int32_t start$1757;
  int32_t _tmp$1755;
  int32_t len$1759;
  int32_t end$1762;
  int32_t _field$2635;
  int32_t start$1763;
  int32_t _tmp$1761;
  int32_t _tmp$1760;
  int32_t _tmp$1758;
  moonbit_incref(self$436);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$436, _tmp$1745
  );
  _field$2637 = self$436->$0;
  data$1751 = _field$2637;
  len$1752 = self$436->$1;
  _field$2636 = str$437.$0;
  str$1753 = _field$2636;
  start$1754 = str$437.$1;
  end$1756 = str$437.$2;
  start$1757 = str$437.$1;
  _tmp$1755 = end$1756 - start$1757;
  moonbit_incref(str$1753);
  moonbit_incref(data$1751);
  $FixedArray$$blit_from_string(
    data$1751, len$1752, str$1753, start$1754, _tmp$1755
  );
  len$1759 = self$436->$1;
  end$1762 = str$437.$2;
  _field$2635 = str$437.$1;
  moonbit_decref(str$437.$0);
  start$1763 = _field$2635;
  _tmp$1761 = end$1762 - start$1763;
  _tmp$1760 = _tmp$1761 * 2;
  _tmp$1758 = len$1759 + _tmp$1760;
  self$436->$1 = _tmp$1758;
  moonbit_decref(self$436);
  return 0;
}

int64_t $String$$offset_of_nth_char$inner(
  moonbit_string_t self$433,
  int32_t i$434,
  int32_t start_offset$435,
  int64_t end_offset$431
) {
  int32_t end_offset$430;
  if (end_offset$431 == 4294967296ll) {
    end_offset$430 = Moonbit_array_length(self$433);
  } else {
    int64_t _Some$432 = end_offset$431;
    end_offset$430 = (int32_t)_Some$432;
  }
  if (i$434 >= 0) {
    return $String$$offset_of_nth_char_forward(
             self$433, i$434, start_offset$435, end_offset$430
           );
  } else {
    int32_t _tmp$1744 = -i$434;
    return $String$$offset_of_nth_char_backward(
             self$433, _tmp$1744, start_offset$435, end_offset$430
           );
  }
}

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$428,
  int32_t n$426,
  int32_t start_offset$422,
  int32_t end_offset$423
) {
  if (start_offset$422 >= 0 && start_offset$422 <= end_offset$423) {
    int32_t utf16_offset$424 = start_offset$422;
    int32_t char_count$425 = 0;
    int32_t _tmp$1742;
    int32_t _if_result$2871;
    while (1) {
      int32_t _tmp$1736 = utf16_offset$424;
      int32_t _if_result$2870;
      if (_tmp$1736 < end_offset$423) {
        int32_t _tmp$1735 = char_count$425;
        _if_result$2870 = _tmp$1735 < n$426;
      } else {
        _if_result$2870 = 0;
      }
      if (_if_result$2870) {
        int32_t _tmp$1740 = utf16_offset$424;
        int32_t c$427 = self$428[_tmp$1740];
        int32_t _tmp$1739;
        if (55296 <= c$427 && c$427 <= 56319) {
          int32_t _tmp$1737 = utf16_offset$424;
          utf16_offset$424 = _tmp$1737 + 2;
        } else {
          int32_t _tmp$1738 = utf16_offset$424;
          utf16_offset$424 = _tmp$1738 + 1;
        }
        _tmp$1739 = char_count$425;
        char_count$425 = _tmp$1739 + 1;
        continue;
      } else {
        moonbit_decref(self$428);
      }
      break;
    }
    _tmp$1742 = char_count$425;
    if (_tmp$1742 < n$426) {
      _if_result$2871 = 1;
    } else {
      int32_t _tmp$1741 = utf16_offset$424;
      _if_result$2871 = _tmp$1741 >= end_offset$423;
    }
    if (_if_result$2871) {
      return 4294967296ll;
    } else {
      int32_t _tmp$1743 = utf16_offset$424;
      return (int64_t)_tmp$1743;
    }
  } else {
    moonbit_decref(self$428);
    return $moonbitlang$core$builtin$abort$4(
             (moonbit_string_t)moonbit_string_literal_115.data,
               (moonbit_string_t)moonbit_string_literal_116.data
           );
  }
}

int64_t $String$$offset_of_nth_char_backward(
  moonbit_string_t self$420,
  int32_t n$418,
  int32_t start_offset$417,
  int32_t end_offset$416
) {
  int32_t char_count$414 = 0;
  int32_t utf16_offset$415 = end_offset$416;
  int32_t _tmp$1733;
  int32_t _if_result$2874;
  while (1) {
    int32_t _tmp$1726 = utf16_offset$415;
    int32_t _tmp$1725 = _tmp$1726 - 1;
    int32_t _if_result$2873;
    if (_tmp$1725 >= start_offset$417) {
      int32_t _tmp$1724 = char_count$414;
      _if_result$2873 = _tmp$1724 < n$418;
    } else {
      _if_result$2873 = 0;
    }
    if (_if_result$2873) {
      int32_t _tmp$1731 = utf16_offset$415;
      int32_t _tmp$1730 = _tmp$1731 - 1;
      int32_t c$419 = self$420[_tmp$1730];
      int32_t _tmp$1729;
      if (56320 <= c$419 && c$419 <= 57343) {
        int32_t _tmp$1727 = utf16_offset$415;
        utf16_offset$415 = _tmp$1727 - 2;
      } else {
        int32_t _tmp$1728 = utf16_offset$415;
        utf16_offset$415 = _tmp$1728 - 1;
      }
      _tmp$1729 = char_count$414;
      char_count$414 = _tmp$1729 + 1;
      continue;
    } else {
      moonbit_decref(self$420);
    }
    break;
  }
  _tmp$1733 = char_count$414;
  if (_tmp$1733 < n$418) {
    _if_result$2874 = 1;
  } else {
    int32_t _tmp$1732 = utf16_offset$415;
    _if_result$2874 = _tmp$1732 < start_offset$417;
  }
  if (_if_result$2874) {
    return 4294967296ll;
  } else {
    int32_t _tmp$1734 = utf16_offset$415;
    return (int64_t)_tmp$1734;
  }
}

int32_t $String$$char_length_eq$inner(
  moonbit_string_t self$406,
  int32_t len$409,
  int32_t start_offset$413,
  int64_t end_offset$404
) {
  int32_t end_offset$403;
  int32_t index$407;
  int32_t count$408;
  if (end_offset$404 == 4294967296ll) {
    end_offset$403 = Moonbit_array_length(self$406);
  } else {
    int64_t _Some$405 = end_offset$404;
    end_offset$403 = (int32_t)_Some$405;
  }
  index$407 = start_offset$413;
  count$408 = 0;
  while (1) {
    if (index$407 < end_offset$403 && count$408 < len$409) {
      int32_t c1$410 = self$406[index$407];
      int32_t _if_result$2876;
      int32_t _tmp$1722;
      int32_t _tmp$1723;
      if (55296 <= c1$410 && c1$410 <= 56319) {
        int32_t _tmp$1718 = index$407 + 1;
        _if_result$2876 = _tmp$1718 < end_offset$403;
      } else {
        _if_result$2876 = 0;
      }
      if (_if_result$2876) {
        int32_t _tmp$1721 = index$407 + 1;
        int32_t c2$411 = self$406[_tmp$1721];
        if (56320 <= c2$411 && c2$411 <= 57343) {
          int32_t _tmp$1719 = index$407 + 2;
          int32_t _tmp$1720 = count$408 + 1;
          index$407 = _tmp$1719;
          count$408 = _tmp$1720;
          continue;
        } else {
          $moonbitlang$core$builtin$abort$1(
            (moonbit_string_t)moonbit_string_literal_117.data,
              (moonbit_string_t)moonbit_string_literal_118.data
          );
        }
      }
      _tmp$1722 = index$407 + 1;
      _tmp$1723 = count$408 + 1;
      index$407 = _tmp$1722;
      count$408 = _tmp$1723;
      continue;
    } else {
      moonbit_decref(self$406);
      return count$408 == len$409 && index$407 == end_offset$403;
    }
    break;
  }
}

moonbit_string_t $String$$from_array(
  struct $$moonbitlang$core$builtin$ArrayView$3c$Char$3e$ chars$398
) {
  int32_t end$1716 = chars$398.$2;
  int32_t start$1717 = chars$398.$1;
  int32_t _tmp$1715 = end$1716 - start$1717;
  int32_t _tmp$1714 = _tmp$1715 * 4;
  struct $$moonbitlang$core$builtin$StringBuilder* buf$397 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(_tmp$1714);
  int32_t end$1712 = chars$398.$2;
  int32_t start$1713 = chars$398.$1;
  int32_t _len$399 = end$1712 - start$1713;
  int32_t _i$400 = 0;
  while (1) {
    if (_i$400 < _len$399) {
      int32_t* _field$2639 = chars$398.$0;
      int32_t* buf$1708 = _field$2639;
      int32_t start$1710 = chars$398.$1;
      int32_t _tmp$1709 = start$1710 + _i$400;
      int32_t _tmp$2638 = (int32_t)buf$1708[_tmp$1709];
      int32_t c$401 = _tmp$2638;
      int32_t _tmp$1711;
      moonbit_incref(buf$397);
      $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
        buf$397, c$401
      );
      _tmp$1711 = _i$400 + 1;
      _i$400 = _tmp$1711;
      continue;
    } else {
      moonbit_decref(chars$398.$0);
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$397);
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ self$396
) {
  int32_t end$1706 = self$396.$2;
  int32_t _field$2640 = self$396.$1;
  int32_t start$1707;
  moonbit_decref(self$396.$0);
  start$1707 = _field$2640;
  return end$1706 - start$1707;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$Char$3e$ self$395
) {
  int32_t end$1704 = self$395.$2;
  int32_t _field$2641 = self$395.$1;
  int32_t start$1705;
  moonbit_decref(self$395.$0);
  start$1705 = _field$2641;
  return end$1704 - start$1705;
}

struct $StringView $String$$view$inner(
  moonbit_string_t self$393,
  int32_t start_offset$394,
  int64_t end_offset$391
) {
  int32_t end_offset$390;
  int32_t _if_result$2878;
  if (end_offset$391 == 4294967296ll) {
    end_offset$390 = Moonbit_array_length(self$393);
  } else {
    int64_t _Some$392 = end_offset$391;
    end_offset$390 = (int32_t)_Some$392;
  }
  if (start_offset$394 >= 0) {
    if (start_offset$394 <= end_offset$390) {
      int32_t _tmp$1703 = Moonbit_array_length(self$393);
      _if_result$2878 = end_offset$390 <= _tmp$1703;
    } else {
      _if_result$2878 = 0;
    }
  } else {
    _if_result$2878 = 0;
  }
  if (_if_result$2878) {
    return (struct $StringView){start_offset$394, end_offset$390, self$393};
  } else {
    moonbit_decref(self$393);
    return $moonbitlang$core$builtin$abort$2(
             (moonbit_string_t)moonbit_string_literal_119.data,
               (moonbit_string_t)moonbit_string_literal_120.data
           );
  }
}

struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$* $StringView$$iter2(
  struct $StringView self$382
) {
  int32_t start$381 = self$382.$1;
  int32_t end$383 = self$382.$2;
  struct $Ref$3c$Int$3e$* index$384 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  struct $Ref$3c$Int$3e$* char_index$385;
  struct $StringView$$iter2$fn$2$2d$cap* _closure$2879;
  struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$* _tmp$1679;
  Moonbit_object_header(index$384)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  index$384->$0 = start$381;
  char_index$385
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(char_index$385)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  char_index$385->$0 = 0;
  _closure$2879
  = (struct $StringView$$iter2$fn$2$2d$cap*)moonbit_malloc(
      sizeof(struct $StringView$$iter2$fn$2$2d$cap)
    );
  Moonbit_object_header(_closure$2879)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $StringView$$iter2$fn$2$2d$cap, $0) >> 2, 3, 0
  );
  _closure$2879->code = &$StringView$$iter2$fn$2;
  _closure$2879->$0 = char_index$385;
  _closure$2879->$1 = index$384;
  _closure$2879->$2 = end$383;
  _closure$2879->$3_0 = self$382.$0;
  _closure$2879->$3_1 = self$382.$1;
  _closure$2879->$3_2 = self$382.$2;
  _tmp$1679
  = (struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$*)_closure$2879;
  return $$moonbitlang$core$builtin$Iter2$$new$0(_tmp$1679);
}

struct $$3c$Int$2a$Char$3e$* $StringView$$iter2$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$* _env$1680
) {
  struct $StringView$$iter2$fn$2$2d$cap* _casted_env$1681 =
    (struct $StringView$$iter2$fn$2$2d$cap*)_env$1680;
  struct $StringView _field$2648 =
    (struct $StringView){
      _casted_env$1681->$3_1, _casted_env$1681->$3_2, _casted_env$1681->$3_0
    };
  struct $StringView self$382 = _field$2648;
  int32_t end$383 = _casted_env$1681->$2;
  struct $Ref$3c$Int$3e$* _field$2647 = _casted_env$1681->$1;
  struct $Ref$3c$Int$3e$* index$384 = _field$2647;
  struct $Ref$3c$Int$3e$* _field$2646 = _casted_env$1681->$0;
  int32_t _cnt$2759 = Moonbit_object_header(_casted_env$1681)->rc;
  struct $Ref$3c$Int$3e$* char_index$385;
  int32_t val$1682;
  if (_cnt$2759 > 1) {
    int32_t _new_cnt$2760 = _cnt$2759 - 1;
    Moonbit_object_header(_casted_env$1681)->rc = _new_cnt$2760;
    moonbit_incref(self$382.$0);
    moonbit_incref(index$384);
    moonbit_incref(_field$2646);
  } else if (_cnt$2759 == 1) {
    moonbit_free(_casted_env$1681);
  }
  char_index$385 = _field$2646;
  val$1682 = index$384->$0;
  if (val$1682 < end$383) {
    moonbit_string_t _field$2645 = self$382.$0;
    moonbit_string_t str$1701 = _field$2645;
    int32_t val$1702 = index$384->$0;
    int32_t _tmp$2644 = str$1701[val$1702];
    int32_t c1$386 = _tmp$2644;
    int32_t _if_result$2880;
    int32_t val$1699;
    int32_t _tmp$1700;
    struct $$3c$Int$2a$Char$3e$* result$389;
    int32_t val$1696;
    int32_t _tmp$1695;
    int32_t val$1698;
    int32_t _tmp$1697;
    if (55296 <= c1$386 && c1$386 <= 56319) {
      int32_t val$1685 = index$384->$0;
      int32_t _tmp$1683 = val$1685 + 1;
      int32_t end$1684 = self$382.$2;
      _if_result$2880 = _tmp$1683 < end$1684;
    } else {
      _if_result$2880 = 0;
    }
    if (_if_result$2880) {
      moonbit_string_t _field$2643 = self$382.$0;
      moonbit_string_t str$1692 = _field$2643;
      int32_t val$1694 = index$384->$0;
      int32_t _tmp$1693 = val$1694 + 1;
      int32_t _tmp$2642 = str$1692[_tmp$1693];
      int32_t c2$387;
      moonbit_decref(str$1692);
      c2$387 = _tmp$2642;
      if (56320 <= c2$387 && c2$387 <= 57343) {
        int32_t val$1690 = char_index$385->$0;
        int32_t _tmp$1691 =
          $moonbitlang$core$builtin$code_point_of_surrogate_pair(
            c1$386, c2$387
          );
        struct $$3c$Int$2a$Char$3e$* result$388 =
          (struct $$3c$Int$2a$Char$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Char$3e$)
          );
        int32_t val$1687;
        int32_t _tmp$1686;
        int32_t val$1689;
        int32_t _tmp$1688;
        Moonbit_object_header(result$388)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Char$3e$) >> 2, 0, 0
        );
        result$388->$0 = val$1690;
        result$388->$1 = _tmp$1691;
        val$1687 = index$384->$0;
        _tmp$1686 = val$1687 + 2;
        index$384->$0 = _tmp$1686;
        moonbit_decref(index$384);
        val$1689 = char_index$385->$0;
        _tmp$1688 = val$1689 + 1;
        char_index$385->$0 = _tmp$1688;
        moonbit_decref(char_index$385);
        return result$388;
      }
    } else {
      moonbit_decref(self$382.$0);
    }
    val$1699 = char_index$385->$0;
    _tmp$1700 = c1$386;
    result$389
    = (struct $$3c$Int$2a$Char$3e$*)moonbit_malloc(
        sizeof(struct $$3c$Int$2a$Char$3e$)
      );
    Moonbit_object_header(result$389)->meta
    = Moonbit_make_regular_object_header(
      sizeof(struct $$3c$Int$2a$Char$3e$) >> 2, 0, 0
    );
    result$389->$0 = val$1699;
    result$389->$1 = _tmp$1700;
    val$1696 = index$384->$0;
    _tmp$1695 = val$1696 + 1;
    index$384->$0 = _tmp$1695;
    moonbit_decref(index$384);
    val$1698 = char_index$385->$0;
    _tmp$1697 = val$1698 + 1;
    char_index$385->$0 = _tmp$1697;
    moonbit_decref(char_index$385);
    return result$389;
  } else {
    moonbit_decref(char_index$385);
    moonbit_decref(index$384);
    moonbit_decref(self$382.$0);
    return 0;
  }
}

struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$* $$moonbitlang$core$builtin$Iter2$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$* f$380
) {
  return f$380;
}

struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* $StringView$$iter(
  struct $StringView self$375
) {
  int32_t start$374 = self$375.$1;
  int32_t end$376 = self$375.$2;
  struct $Ref$3c$Int$3e$* index$377 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  struct $StringView$$iter$$2a$p$fn$1$2d$cap* _closure$2881;
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* _p$1038;
  Moonbit_object_header(index$377)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  index$377->$0 = start$374;
  _closure$2881
  = (struct $StringView$$iter$$2a$p$fn$1$2d$cap*)moonbit_malloc(
      sizeof(struct $StringView$$iter$$2a$p$fn$1$2d$cap)
    );
  Moonbit_object_header(_closure$2881)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $StringView$$iter$$2a$p$fn$1$2d$cap, $0) >> 2, 2, 0
  );
  _closure$2881->code = &$StringView$$iter$$2a$p$fn$1;
  _closure$2881->$0 = index$377;
  _closure$2881->$1 = end$376;
  _closure$2881->$2_0 = self$375.$0;
  _closure$2881->$2_1 = self$375.$1;
  _closure$2881->$2_2 = self$375.$2;
  _p$1038 = (struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$*)_closure$2881;
  return _p$1038;
}

int32_t $StringView$$iter$$2a$p$fn$1(
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* _env$1662
) {
  struct $StringView$$iter$$2a$p$fn$1$2d$cap* _casted_env$1663 =
    (struct $StringView$$iter$$2a$p$fn$1$2d$cap*)_env$1662;
  struct $StringView _field$2654 =
    (struct $StringView){
      _casted_env$1663->$2_1, _casted_env$1663->$2_2, _casted_env$1663->$2_0
    };
  struct $StringView self$375 = _field$2654;
  int32_t end$376 = _casted_env$1663->$1;
  struct $Ref$3c$Int$3e$* _field$2653 = _casted_env$1663->$0;
  int32_t _cnt$2761 = Moonbit_object_header(_casted_env$1663)->rc;
  struct $Ref$3c$Int$3e$* index$377;
  int32_t val$1664;
  if (_cnt$2761 > 1) {
    int32_t _new_cnt$2762 = _cnt$2761 - 1;
    Moonbit_object_header(_casted_env$1663)->rc = _new_cnt$2762;
    moonbit_incref(self$375.$0);
    moonbit_incref(_field$2653);
  } else if (_cnt$2761 == 1) {
    moonbit_free(_casted_env$1663);
  }
  index$377 = _field$2653;
  val$1664 = index$377->$0;
  if (val$1664 < end$376) {
    moonbit_string_t _field$2652 = self$375.$0;
    moonbit_string_t str$1677 = _field$2652;
    int32_t val$1678 = index$377->$0;
    int32_t _tmp$2651 = str$1677[val$1678];
    int32_t c1$378 = _tmp$2651;
    int32_t _if_result$2882;
    int32_t val$1675;
    int32_t _tmp$1674;
    int32_t _tmp$1676;
    if (55296 <= c1$378 && c1$378 <= 56319) {
      int32_t val$1667 = index$377->$0;
      int32_t _tmp$1665 = val$1667 + 1;
      int32_t end$1666 = self$375.$2;
      _if_result$2882 = _tmp$1665 < end$1666;
    } else {
      _if_result$2882 = 0;
    }
    if (_if_result$2882) {
      moonbit_string_t _field$2650 = self$375.$0;
      moonbit_string_t str$1671 = _field$2650;
      int32_t val$1673 = index$377->$0;
      int32_t _tmp$1672 = val$1673 + 1;
      int32_t _tmp$2649 = str$1671[_tmp$1672];
      int32_t c2$379;
      moonbit_decref(str$1671);
      c2$379 = _tmp$2649;
      if (56320 <= c2$379 && c2$379 <= 57343) {
        int32_t val$1669 = index$377->$0;
        int32_t _tmp$1668 = val$1669 + 2;
        int32_t _tmp$1670;
        index$377->$0 = _tmp$1668;
        moonbit_decref(index$377);
        _tmp$1670
        = $moonbitlang$core$builtin$code_point_of_surrogate_pair(
          c1$378, c2$379
        );
        return _tmp$1670;
      }
    } else {
      moonbit_decref(self$375.$0);
    }
    val$1675 = index$377->$0;
    _tmp$1674 = val$1675 + 1;
    index$377->$0 = _tmp$1674;
    moonbit_decref(index$377);
    _tmp$1676 = c1$378;
    return _tmp$1676;
  } else {
    moonbit_decref(index$377);
    moonbit_decref(self$375.$0);
    return -1;
  }
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$StringView$$to_string(
  struct $StringView self$373
) {
  moonbit_string_t _field$2656 = self$373.$0;
  moonbit_string_t str$1659 = _field$2656;
  int32_t start$1660 = self$373.$1;
  int32_t _field$2655 = self$373.$2;
  int32_t end$1661 = _field$2655;
  return $String$$unsafe_substring(str$1659, start$1660, end$1661);
}

int32_t $$moonbitlang$core$builtin$Show$$String$$output(
  moonbit_string_t self$365,
  struct $$moonbitlang$core$builtin$Logger logger$363
) {
  struct $$3c$String$2a$$moonbitlang$core$builtin$Logger$3e$* _env$364;
  int32_t len$366;
  int32_t i$367;
  int32_t seg$368;
  if (logger$363.$1) {
    moonbit_incref(logger$363.$1);
  }
  logger$363.$0->$method_3(logger$363.$1, 34);
  moonbit_incref(self$365);
  if (logger$363.$1) {
    moonbit_incref(logger$363.$1);
  }
  _env$364
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Logger$3e$*)moonbit_malloc(
      sizeof(struct $$3c$String$2a$$moonbitlang$core$builtin$Logger$3e$)
    );
  Moonbit_object_header(_env$364)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Logger$3e$, $0
    )
    >> 2,
      3,
      0
  );
  _env$364->$0 = self$365;
  _env$364->$1_0 = logger$363.$0;
  _env$364->$1_1 = logger$363.$1;
  len$366 = Moonbit_array_length(self$365);
  i$367 = 0;
  seg$368 = 0;
  $$2a$for$369:;
  while (1) {
    int32_t code$370;
    int32_t c$372;
    struct $$moonbitlang$core$builtin$Logger _bind$1641;
    int32_t _tmp$1642;
    int32_t _tmp$1643;
    int32_t _tmp$1644;
    int32_t _tmp$2886;
    int32_t _tmp$2887;
    if (i$367 >= len$366) {
      moonbit_decref(self$365);
      $moonbitlang$core$builtin$output$flush_segment$7c$3594(
        _env$364, seg$368, i$367
      );
      break;
    }
    code$370 = self$365[i$367];
    switch (code$370) {
      case 34: {
        c$372 = code$370;
        goto $join$371;
        break;
      }
      
      case 92: {
        c$372 = code$370;
        goto $join$371;
        break;
      }
      
      case 10: {
        int32_t _tmp$1645;
        int32_t _tmp$1646;
        moonbit_incref(_env$364);
        $moonbitlang$core$builtin$output$flush_segment$7c$3594(
          _env$364, seg$368, i$367
        );
        if (logger$363.$1) {
          moonbit_incref(logger$363.$1);
        }
        logger$363.$0->$method_0(
          logger$363.$1, (moonbit_string_t)moonbit_string_literal_121.data
        );
        _tmp$1645 = i$367 + 1;
        _tmp$1646 = i$367 + 1;
        i$367 = _tmp$1645;
        seg$368 = _tmp$1646;
        goto $$2a$for$369;
        break;
      }
      
      case 13: {
        int32_t _tmp$1647;
        int32_t _tmp$1648;
        moonbit_incref(_env$364);
        $moonbitlang$core$builtin$output$flush_segment$7c$3594(
          _env$364, seg$368, i$367
        );
        if (logger$363.$1) {
          moonbit_incref(logger$363.$1);
        }
        logger$363.$0->$method_0(
          logger$363.$1, (moonbit_string_t)moonbit_string_literal_122.data
        );
        _tmp$1647 = i$367 + 1;
        _tmp$1648 = i$367 + 1;
        i$367 = _tmp$1647;
        seg$368 = _tmp$1648;
        goto $$2a$for$369;
        break;
      }
      
      case 8: {
        int32_t _tmp$1649;
        int32_t _tmp$1650;
        moonbit_incref(_env$364);
        $moonbitlang$core$builtin$output$flush_segment$7c$3594(
          _env$364, seg$368, i$367
        );
        if (logger$363.$1) {
          moonbit_incref(logger$363.$1);
        }
        logger$363.$0->$method_0(
          logger$363.$1, (moonbit_string_t)moonbit_string_literal_123.data
        );
        _tmp$1649 = i$367 + 1;
        _tmp$1650 = i$367 + 1;
        i$367 = _tmp$1649;
        seg$368 = _tmp$1650;
        goto $$2a$for$369;
        break;
      }
      
      case 9: {
        int32_t _tmp$1651;
        int32_t _tmp$1652;
        moonbit_incref(_env$364);
        $moonbitlang$core$builtin$output$flush_segment$7c$3594(
          _env$364, seg$368, i$367
        );
        if (logger$363.$1) {
          moonbit_incref(logger$363.$1);
        }
        logger$363.$0->$method_0(
          logger$363.$1, (moonbit_string_t)moonbit_string_literal_124.data
        );
        _tmp$1651 = i$367 + 1;
        _tmp$1652 = i$367 + 1;
        i$367 = _tmp$1651;
        seg$368 = _tmp$1652;
        goto $$2a$for$369;
        break;
      }
      default: {
        if (code$370 < 32) {
          int32_t _tmp$1655;
          moonbit_string_t _tmp$1654;
          struct $$moonbitlang$core$builtin$Logger _bind$1653;
          int32_t _tmp$1656;
          int32_t _tmp$1657;
          moonbit_incref(_env$364);
          $moonbitlang$core$builtin$output$flush_segment$7c$3594(
            _env$364, seg$368, i$367
          );
          if (logger$363.$1) {
            moonbit_incref(logger$363.$1);
          }
          logger$363.$0->$method_0(
            logger$363.$1, (moonbit_string_t)moonbit_string_literal_125.data
          );
          _tmp$1655 = code$370 & 0xff;
          _tmp$1654 = $Byte$$to_hex(_tmp$1655);
          if (logger$363.$1) {
            moonbit_incref(logger$363.$1);
          }
          logger$363.$0->$method_0(logger$363.$1, _tmp$1654);
          _bind$1653 = logger$363;
          if (_bind$1653.$1) {
            moonbit_incref(_bind$1653.$1);
          }
          _bind$1653.$0->$method_3(_bind$1653.$1, 125);
          _tmp$1656 = i$367 + 1;
          _tmp$1657 = i$367 + 1;
          i$367 = _tmp$1656;
          seg$368 = _tmp$1657;
          goto $$2a$for$369;
        } else {
          int32_t _tmp$1658 = i$367 + 1;
          int32_t _tmp$2885 = seg$368;
          i$367 = _tmp$1658;
          seg$368 = _tmp$2885;
          goto $$2a$for$369;
        }
        break;
      }
    }
    goto $joinlet$2884;
    $join$371:;
    moonbit_incref(_env$364);
    $moonbitlang$core$builtin$output$flush_segment$7c$3594(
      _env$364, seg$368, i$367
    );
    if (logger$363.$1) {
      moonbit_incref(logger$363.$1);
    }
    logger$363.$0->$method_3(logger$363.$1, 92);
    _bind$1641 = logger$363;
    _tmp$1642 = c$372;
    if (_bind$1641.$1) {
      moonbit_incref(_bind$1641.$1);
    }
    _bind$1641.$0->$method_3(_bind$1641.$1, _tmp$1642);
    _tmp$1643 = i$367 + 1;
    _tmp$1644 = i$367 + 1;
    i$367 = _tmp$1643;
    seg$368 = _tmp$1644;
    continue;
    $joinlet$2884:;
    _tmp$2886 = i$367;
    _tmp$2887 = seg$368;
    i$367 = _tmp$2886;
    seg$368 = _tmp$2887;
    continue;
    break;
  }
  logger$363.$0->$method_3(logger$363.$1, 34);
  return 0;
}

int32_t $moonbitlang$core$builtin$output$flush_segment$7c$3594(
  struct $$3c$String$2a$$moonbitlang$core$builtin$Logger$3e$* _env$359,
  int32_t seg$362,
  int32_t i$361
) {
  struct $$moonbitlang$core$builtin$Logger _field$2658 =
    (struct $$moonbitlang$core$builtin$Logger){
      _env$359->$1_0, _env$359->$1_1
    };
  struct $$moonbitlang$core$builtin$Logger logger$358 = _field$2658;
  moonbit_string_t _field$2657 = _env$359->$0;
  int32_t _cnt$2763 = Moonbit_object_header(_env$359)->rc;
  moonbit_string_t self$360;
  if (_cnt$2763 > 1) {
    int32_t _new_cnt$2764 = _cnt$2763 - 1;
    Moonbit_object_header(_env$359)->rc = _new_cnt$2764;
    if (logger$358.$1) {
      moonbit_incref(logger$358.$1);
    }
    moonbit_incref(_field$2657);
  } else if (_cnt$2763 == 1) {
    moonbit_free(_env$359);
  }
  self$360 = _field$2657;
  if (i$361 > seg$362) {
    int32_t _tmp$1640 = i$361 - seg$362;
    logger$358.$0->$method_1(logger$358.$1, self$360, seg$362, _tmp$1640);
  } else {
    moonbit_decref(self$360);
    if (logger$358.$1) {
      moonbit_decref(logger$358.$1);
    }
  }
  return 0;
}

moonbit_string_t $Byte$$to_hex(int32_t b$357) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$356 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t _p$1028 = 16;
  int32_t _tmp$1633 = (int32_t)b$357;
  int32_t _tmp$1634 = (int32_t)_p$1028;
  int32_t _tmp$1632 = _tmp$1633 / _tmp$1634;
  int32_t _tmp$1631 = _tmp$1632 & 0xff;
  int32_t _tmp$1630 =
    $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3604(_tmp$1631);
  int32_t _p$1031;
  int32_t _tmp$1638;
  int32_t _tmp$1639;
  int32_t _tmp$1637;
  int32_t _tmp$1636;
  int32_t _tmp$1635;
  struct $$moonbitlang$core$builtin$StringBuilder* _tmp$1629;
  moonbit_incref(_self$356);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$356, _tmp$1630
  );
  _p$1031 = 16;
  _tmp$1638 = (int32_t)b$357;
  _tmp$1639 = (int32_t)_p$1031;
  _tmp$1637 = _tmp$1638 % _tmp$1639;
  _tmp$1636 = _tmp$1637 & 0xff;
  _tmp$1635
  = $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3604(
    _tmp$1636
  );
  moonbit_incref(_self$356);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$356, _tmp$1635
  );
  _tmp$1629 = _self$356;
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(_tmp$1629);
}

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3604(int32_t i$355) {
  if (i$355 < 10) {
    int32_t _p$1012 = 48;
    int32_t _tmp$1620 = (int32_t)i$355;
    int32_t _tmp$1621 = (int32_t)_p$1012;
    int32_t _tmp$1619 = _tmp$1620 + _tmp$1621;
    int32_t _p$1015 = _tmp$1619 & 0xff;
    int32_t _tmp$1618 = (int32_t)_p$1015;
    return _tmp$1618;
  } else {
    int32_t _p$1018 = 97;
    int32_t _tmp$1627 = (int32_t)i$355;
    int32_t _tmp$1628 = (int32_t)_p$1018;
    int32_t _tmp$1626 = _tmp$1627 + _tmp$1628;
    int32_t _p$1021 = _tmp$1626 & 0xff;
    int32_t _p$1022 = 10;
    int32_t _tmp$1624 = (int32_t)_p$1021;
    int32_t _tmp$1625 = (int32_t)_p$1022;
    int32_t _tmp$1623 = _tmp$1624 - _tmp$1625;
    int32_t _p$1025 = _tmp$1623 & 0xff;
    int32_t _tmp$1622 = (int32_t)_p$1025;
    return _tmp$1622;
  }
}

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$353,
  int32_t that$354
) {
  int32_t _tmp$1616 = (int32_t)self$353;
  int32_t _tmp$1617 = (int32_t)that$354;
  int32_t _tmp$1615 = _tmp$1616 - _tmp$1617;
  return _tmp$1615 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$351,
  int32_t that$352
) {
  int32_t _tmp$1613 = (int32_t)self$351;
  int32_t _tmp$1614 = (int32_t)that$352;
  int32_t _tmp$1612 = _tmp$1613 % _tmp$1614;
  return _tmp$1612 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$349,
  int32_t that$350
) {
  int32_t _tmp$1610 = (int32_t)self$349;
  int32_t _tmp$1611 = (int32_t)that$350;
  int32_t _tmp$1609 = _tmp$1610 / _tmp$1611;
  return _tmp$1609 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$347,
  int32_t that$348
) {
  int32_t _tmp$1607 = (int32_t)self$347;
  int32_t _tmp$1608 = (int32_t)that$348;
  int32_t _tmp$1606 = _tmp$1607 + _tmp$1608;
  return _tmp$1606 & 0xff;
}

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$344,
  int32_t start$342,
  int32_t end$343
) {
  int32_t _if_result$2888;
  int32_t len$345;
  int32_t _tmp$1605;
  moonbit_bytes_t bytes$346;
  moonbit_bytes_t _tmp$1604;
  if (start$342 == 0) {
    int32_t _tmp$1603 = Moonbit_array_length(str$344);
    _if_result$2888 = end$343 == _tmp$1603;
  } else {
    _if_result$2888 = 0;
  }
  if (_if_result$2888) {
    return str$344;
  }
  len$345 = end$343 - start$342;
  _tmp$1605 = len$345 * 2;
  bytes$346 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1605, 0);
  moonbit_incref(bytes$346);
  $FixedArray$$blit_from_string(bytes$346, 0, str$344, start$342, len$345);
  _tmp$1604 = bytes$346;
  return $Bytes$$to_unchecked_string$inner(_tmp$1604, 0, 4294967296ll);
}

int32_t $StringView$$unsafe_charcode_at(
  struct $StringView self$340,
  int32_t index$341
) {
  moonbit_string_t _field$2661 = self$340.$0;
  moonbit_string_t str$1600 = _field$2661;
  int32_t _field$2660 = self$340.$1;
  int32_t start$1602 = _field$2660;
  int32_t _tmp$1601 = start$1602 + index$341;
  int32_t _tmp$2659 = str$1600[_tmp$1601];
  moonbit_decref(str$1600);
  return _tmp$2659;
}

struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$* $$moonbitlang$core$builtin$Iter$$new$1(
  struct $$3c$$3e$$3d$$3e$Option$3c$$3c$Int$2a$Char$3e$$3e$* f$339
) {
  return f$339;
}

struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* $$moonbitlang$core$builtin$Iter$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* f$338
) {
  return f$338;
}

moonbit_string_t $Int$$to_string$inner(int32_t self$322, int32_t radix$321) {
  int32_t is_negative$323;
  uint32_t num$324;
  uint16_t* buffer$325;
  if (radix$321 < 2 || radix$321 > 36) {
    $moonbitlang$core$builtin$abort$1(
      (moonbit_string_t)moonbit_string_literal_126.data,
        (moonbit_string_t)moonbit_string_literal_127.data
    );
  }
  if (self$322 == 0) {
    return (moonbit_string_t)moonbit_string_literal_128.data;
  }
  is_negative$323 = self$322 < 0;
  if (is_negative$323) {
    int32_t _tmp$1599 = -self$322;
    num$324 = *(uint32_t*)&_tmp$1599;
  } else {
    num$324 = *(uint32_t*)&self$322;
  }
  switch (radix$321) {
    case 10: {
      int32_t digit_len$326 = $moonbitlang$core$builtin$dec_count32(num$324);
      int32_t _tmp$1596;
      int32_t total_len$327;
      uint16_t* buffer$328;
      int32_t digit_start$329;
      if (is_negative$323) {
        _tmp$1596 = 1;
      } else {
        _tmp$1596 = 0;
      }
      total_len$327 = digit_len$326 + _tmp$1596;
      buffer$328 = (uint16_t*)moonbit_make_string(total_len$327, 0);
      if (is_negative$323) {
        digit_start$329 = 1;
      } else {
        digit_start$329 = 0;
      }
      moonbit_incref(buffer$328);
      $moonbitlang$core$builtin$int_to_string_dec(
        buffer$328, num$324, digit_start$329, total_len$327
      );
      buffer$325 = buffer$328;
      break;
    }
    
    case 16: {
      int32_t digit_len$330 = $moonbitlang$core$builtin$hex_count32(num$324);
      int32_t _tmp$1597;
      int32_t total_len$331;
      uint16_t* buffer$332;
      int32_t digit_start$333;
      if (is_negative$323) {
        _tmp$1597 = 1;
      } else {
        _tmp$1597 = 0;
      }
      total_len$331 = digit_len$330 + _tmp$1597;
      buffer$332 = (uint16_t*)moonbit_make_string(total_len$331, 0);
      if (is_negative$323) {
        digit_start$333 = 1;
      } else {
        digit_start$333 = 0;
      }
      moonbit_incref(buffer$332);
      $moonbitlang$core$builtin$int_to_string_hex(
        buffer$332, num$324, digit_start$333, total_len$331
      );
      buffer$325 = buffer$332;
      break;
    }
    default: {
      int32_t digit_len$334 =
        $moonbitlang$core$builtin$radix_count32(num$324, radix$321);
      int32_t _tmp$1598;
      int32_t total_len$335;
      uint16_t* buffer$336;
      int32_t digit_start$337;
      if (is_negative$323) {
        _tmp$1598 = 1;
      } else {
        _tmp$1598 = 0;
      }
      total_len$335 = digit_len$334 + _tmp$1598;
      buffer$336 = (uint16_t*)moonbit_make_string(total_len$335, 0);
      if (is_negative$323) {
        digit_start$337 = 1;
      } else {
        digit_start$337 = 0;
      }
      moonbit_incref(buffer$336);
      $moonbitlang$core$builtin$int_to_string_generic(
        buffer$336, num$324, digit_start$337, total_len$335, radix$321
      );
      buffer$325 = buffer$336;
      break;
    }
  }
  if (is_negative$323) {
    buffer$325[0] = 45;
  }
  return buffer$325;
}

int32_t $moonbitlang$core$builtin$radix_count32(
  uint32_t value$315,
  int32_t radix$318
) {
  uint32_t num$316;
  uint32_t base$317;
  int32_t count$319;
  if (value$315 == 0u) {
    return 1;
  }
  num$316 = value$315;
  base$317 = *(uint32_t*)&radix$318;
  count$319 = 0;
  while (1) {
    uint32_t _tmp$1593 = num$316;
    if (_tmp$1593 > 0u) {
      int32_t _tmp$1594 = count$319;
      uint32_t _tmp$1595;
      count$319 = _tmp$1594 + 1;
      _tmp$1595 = num$316;
      num$316 = _tmp$1595 / base$317;
      continue;
    }
    break;
  }
  return count$319;
}

int32_t $moonbitlang$core$builtin$hex_count32(uint32_t value$313) {
  if (value$313 == 0u) {
    return 1;
  } else {
    int32_t leading_zeros$314 = moonbit_clz32(value$313);
    int32_t _tmp$1592 = 31 - leading_zeros$314;
    int32_t _tmp$1591 = _tmp$1592 / 4;
    return _tmp$1591 + 1;
  }
}

int32_t $moonbitlang$core$builtin$dec_count32(uint32_t value$312) {
  if (value$312 >= 100000u) {
    if (value$312 >= 10000000u) {
      if (value$312 >= 1000000000u) {
        return 10;
      } else if (value$312 >= 100000000u) {
        return 9;
      } else {
        return 8;
      }
    } else if (value$312 >= 1000000u) {
      return 7;
    } else {
      return 6;
    }
  } else if (value$312 >= 1000u) {
    if (value$312 >= 10000u) {
      return 5;
    } else {
      return 4;
    }
  } else if (value$312 >= 100u) {
    return 3;
  } else if (value$312 >= 10u) {
    return 2;
  } else {
    return 1;
  }
}

int32_t $moonbitlang$core$builtin$int_to_string_dec(
  uint16_t* buffer$302,
  uint32_t num$290,
  int32_t digit_start$293,
  int32_t total_len$292
) {
  uint32_t num$289 = num$290;
  int32_t offset$291 = total_len$292 - digit_start$293;
  uint32_t _tmp$1590;
  int32_t remaining$304;
  int32_t _tmp$1571;
  while (1) {
    uint32_t _tmp$1534 = num$289;
    if (_tmp$1534 >= 10000u) {
      uint32_t _tmp$1557 = num$289;
      uint32_t t$294 = _tmp$1557 / 10000u;
      uint32_t _tmp$1556 = num$289;
      uint32_t _tmp$1555 = _tmp$1556 % 10000u;
      int32_t r$295 = *(int32_t*)&_tmp$1555;
      int32_t d1$296;
      int32_t d2$297;
      int32_t _tmp$1535;
      int32_t _tmp$1554;
      int32_t _tmp$1553;
      int32_t d1_hi$298;
      int32_t _tmp$1552;
      int32_t _tmp$1551;
      int32_t d1_lo$299;
      int32_t _tmp$1550;
      int32_t _tmp$1549;
      int32_t d2_hi$300;
      int32_t _tmp$1548;
      int32_t _tmp$1547;
      int32_t d2_lo$301;
      int32_t _tmp$1537;
      int32_t _tmp$1536;
      int32_t _tmp$1540;
      int32_t _tmp$1539;
      int32_t _tmp$1538;
      int32_t _tmp$1543;
      int32_t _tmp$1542;
      int32_t _tmp$1541;
      int32_t _tmp$1546;
      int32_t _tmp$1545;
      int32_t _tmp$1544;
      num$289 = t$294;
      d1$296 = r$295 / 100;
      d2$297 = r$295 % 100;
      _tmp$1535 = offset$291;
      offset$291 = _tmp$1535 - 4;
      _tmp$1554 = d1$296 / 10;
      _tmp$1553 = 48 + _tmp$1554;
      d1_hi$298 = (uint16_t)_tmp$1553;
      _tmp$1552 = d1$296 % 10;
      _tmp$1551 = 48 + _tmp$1552;
      d1_lo$299 = (uint16_t)_tmp$1551;
      _tmp$1550 = d2$297 / 10;
      _tmp$1549 = 48 + _tmp$1550;
      d2_hi$300 = (uint16_t)_tmp$1549;
      _tmp$1548 = d2$297 % 10;
      _tmp$1547 = 48 + _tmp$1548;
      d2_lo$301 = (uint16_t)_tmp$1547;
      _tmp$1537 = offset$291;
      _tmp$1536 = digit_start$293 + _tmp$1537;
      buffer$302[_tmp$1536] = d1_hi$298;
      _tmp$1540 = offset$291;
      _tmp$1539 = digit_start$293 + _tmp$1540;
      _tmp$1538 = _tmp$1539 + 1;
      buffer$302[_tmp$1538] = d1_lo$299;
      _tmp$1543 = offset$291;
      _tmp$1542 = digit_start$293 + _tmp$1543;
      _tmp$1541 = _tmp$1542 + 2;
      buffer$302[_tmp$1541] = d2_hi$300;
      _tmp$1546 = offset$291;
      _tmp$1545 = digit_start$293 + _tmp$1546;
      _tmp$1544 = _tmp$1545 + 3;
      buffer$302[_tmp$1544] = d2_lo$301;
      continue;
    }
    break;
  }
  _tmp$1590 = num$289;
  remaining$304 = *(int32_t*)&_tmp$1590;
  while (1) {
    int32_t _tmp$1558 = remaining$304;
    if (_tmp$1558 >= 100) {
      int32_t _tmp$1570 = remaining$304;
      int32_t t$305 = _tmp$1570 / 100;
      int32_t _tmp$1569 = remaining$304;
      int32_t d$306 = _tmp$1569 % 100;
      int32_t _tmp$1559;
      int32_t _tmp$1568;
      int32_t _tmp$1567;
      int32_t d_hi$307;
      int32_t _tmp$1566;
      int32_t _tmp$1565;
      int32_t d_lo$308;
      int32_t _tmp$1561;
      int32_t _tmp$1560;
      int32_t _tmp$1564;
      int32_t _tmp$1563;
      int32_t _tmp$1562;
      remaining$304 = t$305;
      _tmp$1559 = offset$291;
      offset$291 = _tmp$1559 - 2;
      _tmp$1568 = d$306 / 10;
      _tmp$1567 = 48 + _tmp$1568;
      d_hi$307 = (uint16_t)_tmp$1567;
      _tmp$1566 = d$306 % 10;
      _tmp$1565 = 48 + _tmp$1566;
      d_lo$308 = (uint16_t)_tmp$1565;
      _tmp$1561 = offset$291;
      _tmp$1560 = digit_start$293 + _tmp$1561;
      buffer$302[_tmp$1560] = d_hi$307;
      _tmp$1564 = offset$291;
      _tmp$1563 = digit_start$293 + _tmp$1564;
      _tmp$1562 = _tmp$1563 + 1;
      buffer$302[_tmp$1562] = d_lo$308;
      continue;
    }
    break;
  }
  _tmp$1571 = remaining$304;
  if (_tmp$1571 >= 10) {
    int32_t _tmp$1572 = offset$291;
    int32_t _tmp$1583;
    int32_t _tmp$1582;
    int32_t _tmp$1581;
    int32_t d_hi$310;
    int32_t _tmp$1580;
    int32_t _tmp$1579;
    int32_t _tmp$1578;
    int32_t d_lo$311;
    int32_t _tmp$1574;
    int32_t _tmp$1573;
    int32_t _tmp$1577;
    int32_t _tmp$1576;
    int32_t _tmp$1575;
    offset$291 = _tmp$1572 - 2;
    _tmp$1583 = remaining$304;
    _tmp$1582 = _tmp$1583 / 10;
    _tmp$1581 = 48 + _tmp$1582;
    d_hi$310 = (uint16_t)_tmp$1581;
    _tmp$1580 = remaining$304;
    _tmp$1579 = _tmp$1580 % 10;
    _tmp$1578 = 48 + _tmp$1579;
    d_lo$311 = (uint16_t)_tmp$1578;
    _tmp$1574 = offset$291;
    _tmp$1573 = digit_start$293 + _tmp$1574;
    buffer$302[_tmp$1573] = d_hi$310;
    _tmp$1577 = offset$291;
    _tmp$1576 = digit_start$293 + _tmp$1577;
    _tmp$1575 = _tmp$1576 + 1;
    buffer$302[_tmp$1575] = d_lo$311;
    moonbit_decref(buffer$302);
  } else {
    int32_t _tmp$1584 = offset$291;
    int32_t _tmp$1589;
    int32_t _tmp$1585;
    int32_t _tmp$1588;
    int32_t _tmp$1587;
    int32_t _tmp$1586;
    offset$291 = _tmp$1584 - 1;
    _tmp$1589 = offset$291;
    _tmp$1585 = digit_start$293 + _tmp$1589;
    _tmp$1588 = remaining$304;
    _tmp$1587 = 48 + _tmp$1588;
    _tmp$1586 = (uint16_t)_tmp$1587;
    buffer$302[_tmp$1585] = _tmp$1586;
    moonbit_decref(buffer$302);
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int_to_string_generic(
  uint16_t* buffer$284,
  uint32_t num$278,
  int32_t digit_start$276,
  int32_t total_len$275,
  int32_t radix$280
) {
  int32_t offset$274 = total_len$275 - digit_start$276;
  uint32_t n$277 = num$278;
  uint32_t base$279 = *(uint32_t*)&radix$280;
  int32_t _tmp$1514 = radix$280 - 1;
  int32_t _tmp$1513 = radix$280 & _tmp$1514;
  if (_tmp$1513 == 0) {
    int32_t shift$281 = moonbit_ctz32(radix$280);
    uint32_t mask$282 = base$279 - 1u;
    while (1) {
      uint32_t _tmp$1515 = n$277;
      if (_tmp$1515 > 0u) {
        int32_t _tmp$1516 = offset$274;
        uint32_t _tmp$1523;
        uint32_t _tmp$1522;
        int32_t digit$283;
        int32_t _tmp$1520;
        int32_t _tmp$1517;
        int32_t _tmp$1519;
        int32_t _tmp$1518;
        uint32_t _tmp$1521;
        offset$274 = _tmp$1516 - 1;
        _tmp$1523 = n$277;
        _tmp$1522 = _tmp$1523 & mask$282;
        digit$283 = *(int32_t*)&_tmp$1522;
        _tmp$1520 = offset$274;
        _tmp$1517 = digit_start$276 + _tmp$1520;
        _tmp$1519
        = ((moonbit_string_t)moonbit_string_literal_129.data)[
          digit$283
        ];
        _tmp$1518 = (uint16_t)_tmp$1519;
        buffer$284[_tmp$1517] = _tmp$1518;
        _tmp$1521 = n$277;
        n$277 = _tmp$1521 >> (shift$281 & 31);
        continue;
      } else {
        moonbit_decref(buffer$284);
      }
      break;
    }
  } else {
    while (1) {
      uint32_t _tmp$1524 = n$277;
      if (_tmp$1524 > 0u) {
        int32_t _tmp$1525 = offset$274;
        uint32_t _tmp$1533;
        uint32_t q$286;
        uint32_t _tmp$1531;
        uint32_t _tmp$1532;
        uint32_t _tmp$1530;
        int32_t digit$287;
        int32_t _tmp$1529;
        int32_t _tmp$1526;
        int32_t _tmp$1528;
        int32_t _tmp$1527;
        offset$274 = _tmp$1525 - 1;
        _tmp$1533 = n$277;
        q$286 = _tmp$1533 / base$279;
        _tmp$1531 = n$277;
        _tmp$1532 = q$286 * base$279;
        _tmp$1530 = _tmp$1531 - _tmp$1532;
        digit$287 = *(int32_t*)&_tmp$1530;
        _tmp$1529 = offset$274;
        _tmp$1526 = digit_start$276 + _tmp$1529;
        _tmp$1528
        = ((moonbit_string_t)moonbit_string_literal_129.data)[
          digit$287
        ];
        _tmp$1527 = (uint16_t)_tmp$1528;
        buffer$284[_tmp$1526] = _tmp$1527;
        n$277 = q$286;
        continue;
      } else {
        moonbit_decref(buffer$284);
      }
      break;
    }
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int_to_string_hex(
  uint16_t* buffer$271,
  uint32_t num$267,
  int32_t digit_start$265,
  int32_t total_len$264
) {
  int32_t offset$263 = total_len$264 - digit_start$265;
  uint32_t n$266 = num$267;
  int32_t _tmp$1508;
  while (1) {
    int32_t _tmp$1494 = offset$263;
    if (_tmp$1494 >= 2) {
      int32_t _tmp$1495 = offset$263;
      uint32_t _tmp$1507;
      uint32_t _tmp$1506;
      int32_t byte_val$268;
      int32_t hi$269;
      int32_t lo$270;
      int32_t _tmp$1499;
      int32_t _tmp$1496;
      int32_t _tmp$1498;
      int32_t _tmp$1497;
      int32_t _tmp$1504;
      int32_t _tmp$1503;
      int32_t _tmp$1500;
      int32_t _tmp$1502;
      int32_t _tmp$1501;
      uint32_t _tmp$1505;
      offset$263 = _tmp$1495 - 2;
      _tmp$1507 = n$266;
      _tmp$1506 = _tmp$1507 & 255u;
      byte_val$268 = *(int32_t*)&_tmp$1506;
      hi$269 = byte_val$268 / 16;
      lo$270 = byte_val$268 % 16;
      _tmp$1499 = offset$263;
      _tmp$1496 = digit_start$265 + _tmp$1499;
      _tmp$1498 = ((moonbit_string_t)moonbit_string_literal_129.data)[hi$269];
      _tmp$1497 = (uint16_t)_tmp$1498;
      buffer$271[_tmp$1496] = _tmp$1497;
      _tmp$1504 = offset$263;
      _tmp$1503 = digit_start$265 + _tmp$1504;
      _tmp$1500 = _tmp$1503 + 1;
      _tmp$1502 = ((moonbit_string_t)moonbit_string_literal_129.data)[lo$270];
      _tmp$1501 = (uint16_t)_tmp$1502;
      buffer$271[_tmp$1500] = _tmp$1501;
      _tmp$1505 = n$266;
      n$266 = _tmp$1505 >> 8;
      continue;
    }
    break;
  }
  _tmp$1508 = offset$263;
  if (_tmp$1508 == 1) {
    uint32_t _tmp$1512 = n$266;
    uint32_t _tmp$1511 = _tmp$1512 & 15u;
    int32_t nibble$273 = *(int32_t*)&_tmp$1511;
    int32_t _tmp$1510 =
      ((moonbit_string_t)moonbit_string_literal_129.data)[nibble$273];
    int32_t _tmp$1509 = (uint16_t)_tmp$1510;
    buffer$271[digit_start$265] = _tmp$1509;
    moonbit_decref(buffer$271);
  } else {
    moonbit_decref(buffer$271);
  }
  return 0;
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  int32_t self$262
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$261 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1493;
  moonbit_incref(logger$261);
  _tmp$1493
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$261
  };
  $$moonbitlang$core$builtin$Show$$Int$$output(self$262, _tmp$1493);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$261);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$260
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$259 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1492;
  moonbit_incref(logger$259);
  _tmp$1492
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$259
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
    self$260, _tmp$1492
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$259);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  void* self$258
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$257 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1491;
  moonbit_incref(logger$257);
  _tmp$1491
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$257
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$x$fs$IOError$$output(
    self$258, _tmp$1491
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$257);
}

int32_t $StringView$$start_offset(struct $StringView self$256) {
  int32_t _field$2662 = self$256.$1;
  moonbit_decref(self$256.$0);
  return _field$2662;
}

int32_t $StringView$$length(struct $StringView self$255) {
  int32_t end$1489 = self$255.$2;
  int32_t _field$2663 = self$255.$1;
  int32_t start$1490;
  moonbit_decref(self$255.$0);
  start$1490 = _field$2663;
  return end$1489 - start$1490;
}

moonbit_string_t $StringView$$data(struct $StringView self$254) {
  moonbit_string_t _field$2664 = self$254.$0;
  return _field$2664;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$248,
  moonbit_string_t value$251,
  int32_t start$252,
  int32_t len$253
) {
  void* _try_err$250;
  struct $StringView _tmp$1484;
  int32_t _tmp$1486 = start$252 + len$253;
  int64_t _tmp$1485 = (int64_t)_tmp$1486;
  struct moonbit_result_2 _tmp$2896 =
    $String$$sub$inner(value$251, start$252, _tmp$1485);
  if (_tmp$2896.tag) {
    struct $StringView const _ok$1487 = _tmp$2896.data.ok;
    _tmp$1484 = _ok$1487;
  } else {
    void* const _err$1488 = _tmp$2896.data.err;
    _try_err$250 = _err$1488;
    goto $join$249;
  }
  goto $joinlet$2895;
  $join$249:;
  moonbit_decref(_try_err$250);
  moonbit_panic();
  $joinlet$2895:;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    self$248, _tmp$1484
  );
  return 0;
}

struct moonbit_result_2 $String$$sub(
  moonbit_string_t self$246,
  int64_t start$opt$244,
  int64_t end$247
) {
  int32_t start$243;
  if (start$opt$244 == 4294967296ll) {
    start$243 = 0;
  } else {
    int64_t _Some$245 = start$opt$244;
    start$243 = (int32_t)_Some$245;
  }
  return $String$$sub$inner(self$246, start$243, end$247);
}

struct moonbit_result_2 $String$$sub$inner(
  moonbit_string_t self$236,
  int32_t start$242,
  int64_t end$238
) {
  int32_t len$235 = Moonbit_array_length(self$236);
  int32_t end$237;
  int32_t start$241;
  if (end$238 == 4294967296ll) {
    end$237 = len$235;
  } else {
    int64_t _Some$239 = end$238;
    int32_t _end$240 = (int32_t)_Some$239;
    if (_end$240 < 0) {
      end$237 = len$235 + _end$240;
    } else {
      end$237 = _end$240;
    }
  }
  if (start$242 < 0) {
    start$241 = len$235 + start$242;
  } else {
    start$241 = start$242;
  }
  if (start$241 >= 0 && start$241 <= end$237 && end$237 <= len$235) {
    int32_t _if_result$2897;
    int32_t _if_result$2899;
    struct $StringView _tmp$1482;
    struct moonbit_result_2 _result$2901;
    if (start$241 < len$235) {
      int32_t _p$1004 = self$236[start$241];
      _if_result$2897 = 56320 <= _p$1004 && _p$1004 <= 57343;
    } else {
      _if_result$2897 = 0;
    }
    if (_if_result$2897) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1480;
      struct moonbit_result_2 _result$2898;
      moonbit_decref(self$236);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1480
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$2898.tag = 0;
      _result$2898.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1480;
      return _result$2898;
    }
    if (end$237 < len$235) {
      int32_t _p$1007 = self$236[end$237];
      _if_result$2899 = 56320 <= _p$1007 && _p$1007 <= 57343;
    } else {
      _if_result$2899 = 0;
    }
    if (_if_result$2899) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1481;
      struct moonbit_result_2 _result$2900;
      moonbit_decref(self$236);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1481
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$2900.tag = 0;
      _result$2900.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1481;
      return _result$2900;
    }
    _tmp$1482 = (struct $StringView){start$241, end$237, self$236};
    _result$2901.tag = 1;
    _result$2901.data.ok = _tmp$1482;
    return _result$2901;
  } else {
    void* moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1483;
    struct moonbit_result_2 _result$2902;
    moonbit_decref(self$236);
    moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1483
    = (struct moonbit_object*)&moonbit_constant_constructor_1 + 1;
    _result$2902.tag = 0;
    _result$2902.data.err
    = moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1483;
    return _result$2902;
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$233,
  moonbit_string_t str$234
) {
  int32_t len$1470 = self$233->$1;
  int32_t _tmp$1472 = Moonbit_array_length(str$234);
  int32_t _tmp$1471 = _tmp$1472 * 2;
  int32_t _tmp$1469 = len$1470 + _tmp$1471;
  moonbit_bytes_t _field$2666;
  moonbit_bytes_t data$1473;
  int32_t len$1474;
  int32_t _tmp$1475;
  int32_t len$1477;
  int32_t _tmp$2665;
  int32_t _tmp$1479;
  int32_t _tmp$1478;
  int32_t _tmp$1476;
  moonbit_incref(self$233);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$233, _tmp$1469
  );
  _field$2666 = self$233->$0;
  data$1473 = _field$2666;
  len$1474 = self$233->$1;
  _tmp$1475 = Moonbit_array_length(str$234);
  moonbit_incref(data$1473);
  moonbit_incref(str$234);
  $FixedArray$$blit_from_string(data$1473, len$1474, str$234, 0, _tmp$1475);
  len$1477 = self$233->$1;
  _tmp$2665 = Moonbit_array_length(str$234);
  moonbit_decref(str$234);
  _tmp$1479 = _tmp$2665;
  _tmp$1478 = _tmp$1479 * 2;
  _tmp$1476 = len$1477 + _tmp$1478;
  self$233->$1 = _tmp$1476;
  moonbit_decref(self$233);
  return 0;
}

int32_t $FixedArray$$blit_from_string(
  moonbit_bytes_t self$225,
  int32_t bytes_offset$220,
  moonbit_string_t str$227,
  int32_t str_offset$223,
  int32_t length$221
) {
  int32_t _tmp$1468 = length$221 * 2;
  int32_t _tmp$1467 = bytes_offset$220 + _tmp$1468;
  int32_t e1$219 = _tmp$1467 - 1;
  int32_t _tmp$1466 = str_offset$223 + length$221;
  int32_t e2$222 = _tmp$1466 - 1;
  int32_t len1$224 = Moonbit_array_length(self$225);
  int32_t len2$226 = Moonbit_array_length(str$227);
  if (
    length$221 >= 0
    && bytes_offset$220 >= 0
    && e1$219 < len1$224
    && str_offset$223 >= 0
    && e2$222 < len2$226
  ) {
    int32_t end_str_offset$228 = str_offset$223 + length$221;
    int32_t i$229 = str_offset$223;
    int32_t j$230 = bytes_offset$220;
    while (1) {
      if (i$229 < end_str_offset$228) {
        int32_t _tmp$1463 = str$227[i$229];
        uint32_t c$231 = *(uint32_t*)&_tmp$1463;
        uint32_t _p$998 = c$231 & 255u;
        int32_t _tmp$1459 = *(int32_t*)&_p$998;
        int32_t _tmp$1458 = _tmp$1459 & 0xff;
        int32_t _tmp$1460;
        uint32_t _p$1001;
        int32_t _tmp$1462;
        int32_t _tmp$1461;
        int32_t _tmp$1464;
        int32_t _tmp$1465;
        if (j$230 < 0 || j$230 >= Moonbit_array_length(self$225)) {
          moonbit_panic();
        }
        self$225[j$230] = _tmp$1458;
        _tmp$1460 = j$230 + 1;
        _p$1001 = c$231 >> 8;
        _tmp$1462 = *(int32_t*)&_p$1001;
        _tmp$1461 = _tmp$1462 & 0xff;
        if (_tmp$1460 < 0 || _tmp$1460 >= Moonbit_array_length(self$225)) {
          moonbit_panic();
        }
        self$225[_tmp$1460] = _tmp$1461;
        _tmp$1464 = i$229 + 1;
        _tmp$1465 = j$230 + 2;
        i$229 = _tmp$1464;
        j$230 = _tmp$1465;
        continue;
      } else {
        moonbit_decref(str$227);
        moonbit_decref(self$225);
      }
      break;
    }
  } else {
    moonbit_decref(str$227);
    moonbit_decref(self$225);
    moonbit_panic();
  }
  return 0;
}

struct $$moonbitlang$core$builtin$SourceLocRepr* $$moonbitlang$core$builtin$SourceLocRepr$$parse(
  moonbit_string_t repr$142
) {
  int32_t _tmp$1457 = Moonbit_array_length(repr$142);
  struct $StringView _bind$141 = (struct $StringView){0, _tmp$1457, repr$142};
  moonbit_string_t _field$2668 = _bind$141.$0;
  moonbit_string_t _data$143 = _field$2668;
  int32_t _start$144 = _bind$141.$1;
  int32_t end$1455 = _bind$141.$2;
  int32_t _field$2667 = _bind$141.$1;
  int32_t start$1456 = _field$2667;
  int32_t _tmp$1454 = end$1455 - start$1456;
  int32_t _end$145 = _start$144 + _tmp$1454;
  int32_t _cursor$146 = _start$144;
  int32_t accept_state$147 = -1;
  int32_t match_end$148 = -1;
  int32_t match_tag_saver_0$149 = -1;
  int32_t match_tag_saver_1$150 = -1;
  int32_t match_tag_saver_2$151 = -1;
  int32_t match_tag_saver_3$152 = -1;
  int32_t match_tag_saver_4$153 = -1;
  int32_t tag_0$154 = -1;
  int32_t tag_1$155 = -1;
  int32_t tag_1_1$156 = -1;
  int32_t tag_1_2$157 = -1;
  int32_t tag_3$158 = -1;
  int32_t tag_2$159 = -1;
  int32_t tag_2_1$160 = -1;
  int32_t tag_4$161 = -1;
  int32_t join_dispatch_19$182;
  int32_t _tmp$1444 = _cursor$146;
  int32_t dispatch_19$183;
  if (_tmp$1444 < _end$145) {
    int32_t _p$944 = _cursor$146;
    int32_t next_char$211 = _data$143[_p$944];
    int32_t _tmp$1445 = _cursor$146;
    _cursor$146 = _tmp$1445 + 1;
    if (next_char$211 < 65) {
      if (next_char$211 < 64) {
        goto $join$162;
      } else {
        while (1) {
          int32_t _tmp$1446;
          tag_0$154 = _cursor$146;
          _tmp$1446 = _cursor$146;
          if (_tmp$1446 < _end$145) {
            int32_t _p$947 = _cursor$146;
            int32_t next_char$214 = _data$143[_p$947];
            int32_t _tmp$1447 = _cursor$146;
            _cursor$146 = _tmp$1447 + 1;
            if (next_char$214 < 55296) {
              if (next_char$214 < 58) {
                goto $join$212;
              } else if (next_char$214 > 58) {
                goto $join$212;
              } else {
                int32_t _tmp$1448 = _cursor$146;
                if (_tmp$1448 < _end$145) {
                  int32_t _p$950 = _cursor$146;
                  int32_t next_char$216 = _data$143[_p$950];
                  int32_t _tmp$1449 = _cursor$146;
                  _cursor$146 = _tmp$1449 + 1;
                  if (next_char$216 < 56319) {
                    if (next_char$216 < 55296) {
                      goto $join$215;
                    } else {
                      join_dispatch_19$182 = 7;
                      goto $join$181;
                    }
                  } else if (next_char$216 > 56319) {
                    if (next_char$216 < 65536) {
                      goto $join$215;
                    } else {
                      goto $join$162;
                    }
                  } else {
                    join_dispatch_19$182 = 8;
                    goto $join$181;
                  }
                  $join$215:;
                  join_dispatch_19$182 = 0;
                  goto $join$181;
                } else {
                  goto $join$162;
                }
              }
            } else if (next_char$214 > 56318) {
              if (next_char$214 < 57344) {
                int32_t _tmp$1450 = _cursor$146;
                if (_tmp$1450 < _end$145) {
                  int32_t _p$953 = _cursor$146;
                  int32_t next_char$217 = _data$143[_p$953];
                  int32_t _tmp$1451 = _cursor$146;
                  _cursor$146 = _tmp$1451 + 1;
                  if (next_char$217 < 56320) {
                    goto $join$162;
                  } else if (next_char$217 > 57343) {
                    goto $join$162;
                  } else {
                    continue;
                  }
                } else {
                  goto $join$162;
                }
              } else if (next_char$214 > 65535) {
                goto $join$162;
              } else {
                goto $join$212;
              }
            } else {
              int32_t _tmp$1452 = _cursor$146;
              if (_tmp$1452 < _end$145) {
                int32_t _p$956 = _cursor$146;
                int32_t next_char$218 = _data$143[_p$956];
                int32_t _tmp$1453 = _cursor$146;
                _cursor$146 = _tmp$1453 + 1;
                if (next_char$218 < 56320) {
                  goto $join$162;
                } else if (next_char$218 > 65535) {
                  goto $join$162;
                } else {
                  continue;
                }
              } else {
                goto $join$162;
              }
            }
            $join$212:;
            continue;
          } else {
            goto $join$162;
          }
          break;
        }
      }
    } else {
      goto $join$162;
    }
  } else {
    goto $join$162;
  }
  $join$181:;
  dispatch_19$183 = join_dispatch_19$182;
  $loop_label_19$186:;
  while (1) {
    int32_t _tmp$1418;
    switch (dispatch_19$183) {
      case 3: {
        int32_t _tmp$1420;
        tag_1_2$157 = tag_1_1$156;
        tag_1_1$156 = tag_1$155;
        tag_1$155 = _cursor$146;
        _tmp$1420 = _cursor$146;
        if (_tmp$1420 < _end$145) {
          int32_t _p$959 = _cursor$146;
          int32_t next_char$190 = _data$143[_p$959];
          int32_t _tmp$1421 = _cursor$146;
          _cursor$146 = _tmp$1421 + 1;
          if (next_char$190 < 55296) {
            if (next_char$190 < 58) {
              if (next_char$190 < 48) {
                goto $join$189;
              } else {
                int32_t _tmp$1422;
                tag_1$155 = _cursor$146;
                tag_2_1$160 = tag_2$159;
                tag_2$159 = _cursor$146;
                tag_3$158 = _cursor$146;
                _tmp$1422 = _cursor$146;
                if (_tmp$1422 < _end$145) {
                  int32_t _p$962 = _cursor$146;
                  int32_t next_char$192 = _data$143[_p$962];
                  int32_t _tmp$1423 = _cursor$146;
                  _cursor$146 = _tmp$1423 + 1;
                  if (next_char$192 < 59) {
                    if (next_char$192 < 46) {
                      if (next_char$192 < 45) {
                        goto $join$191;
                      } else {
                        goto $join$184;
                      }
                    } else if (next_char$192 > 47) {
                      if (next_char$192 < 58) {
                        dispatch_19$183 = 6;
                        goto $loop_label_19$186;
                      } else {
                        dispatch_19$183 = 3;
                        goto $loop_label_19$186;
                      }
                    } else {
                      goto $join$191;
                    }
                  } else if (next_char$192 > 55295) {
                    if (next_char$192 < 57344) {
                      if (next_char$192 < 56319) {
                        dispatch_19$183 = 7;
                        goto $loop_label_19$186;
                      } else {
                        dispatch_19$183 = 8;
                        goto $loop_label_19$186;
                      }
                    } else if (next_char$192 > 65535) {
                      goto $join$162;
                    } else {
                      goto $join$191;
                    }
                  } else {
                    goto $join$191;
                  }
                  $join$191:;
                  dispatch_19$183 = 0;
                  goto $loop_label_19$186;
                } else {
                  goto $join$162;
                }
              }
            } else if (next_char$190 > 58) {
              goto $join$189;
            } else {
              dispatch_19$183 = 1;
              goto $loop_label_19$186;
            }
          } else if (next_char$190 > 56318) {
            if (next_char$190 < 57344) {
              dispatch_19$183 = 8;
              goto $loop_label_19$186;
            } else if (next_char$190 > 65535) {
              goto $join$162;
            } else {
              goto $join$189;
            }
          } else {
            dispatch_19$183 = 7;
            goto $loop_label_19$186;
          }
          $join$189:;
          dispatch_19$183 = 0;
          goto $loop_label_19$186;
        } else {
          goto $join$162;
        }
        break;
      }
      
      case 2: {
        int32_t _tmp$1424;
        tag_1$155 = _cursor$146;
        tag_2$159 = _cursor$146;
        _tmp$1424 = _cursor$146;
        if (_tmp$1424 < _end$145) {
          int32_t _p$965 = _cursor$146;
          int32_t next_char$194 = _data$143[_p$965];
          int32_t _tmp$1425 = _cursor$146;
          _cursor$146 = _tmp$1425 + 1;
          if (next_char$194 < 55296) {
            if (next_char$194 < 58) {
              if (next_char$194 < 48) {
                goto $join$193;
              } else {
                dispatch_19$183 = 2;
                goto $loop_label_19$186;
              }
            } else if (next_char$194 > 58) {
              goto $join$193;
            } else {
              dispatch_19$183 = 3;
              goto $loop_label_19$186;
            }
          } else if (next_char$194 > 56318) {
            if (next_char$194 < 57344) {
              dispatch_19$183 = 8;
              goto $loop_label_19$186;
            } else if (next_char$194 > 65535) {
              goto $join$162;
            } else {
              goto $join$193;
            }
          } else {
            dispatch_19$183 = 7;
            goto $loop_label_19$186;
          }
          $join$193:;
          dispatch_19$183 = 0;
          goto $loop_label_19$186;
        } else {
          goto $join$162;
        }
        break;
      }
      
      case 0: {
        int32_t _tmp$1426;
        tag_1$155 = _cursor$146;
        _tmp$1426 = _cursor$146;
        if (_tmp$1426 < _end$145) {
          int32_t _p$968 = _cursor$146;
          int32_t next_char$196 = _data$143[_p$968];
          int32_t _tmp$1427 = _cursor$146;
          _cursor$146 = _tmp$1427 + 1;
          if (next_char$196 < 55296) {
            if (next_char$196 < 58) {
              goto $join$195;
            } else if (next_char$196 > 58) {
              goto $join$195;
            } else {
              dispatch_19$183 = 1;
              goto $loop_label_19$186;
            }
          } else if (next_char$196 > 56318) {
            if (next_char$196 < 57344) {
              dispatch_19$183 = 8;
              goto $loop_label_19$186;
            } else if (next_char$196 > 65535) {
              goto $join$162;
            } else {
              goto $join$195;
            }
          } else {
            dispatch_19$183 = 7;
            goto $loop_label_19$186;
          }
          $join$195:;
          dispatch_19$183 = 0;
          goto $loop_label_19$186;
        } else {
          goto $join$162;
        }
        break;
      }
      
      case 8: {
        int32_t _tmp$1428 = _cursor$146;
        if (_tmp$1428 < _end$145) {
          int32_t _p$971 = _cursor$146;
          int32_t next_char$197 = _data$143[_p$971];
          int32_t _tmp$1429 = _cursor$146;
          _cursor$146 = _tmp$1429 + 1;
          if (next_char$197 < 56320) {
            goto $join$162;
          } else if (next_char$197 > 57343) {
            goto $join$162;
          } else {
            dispatch_19$183 = 0;
            goto $loop_label_19$186;
          }
        } else {
          goto $join$162;
        }
        break;
      }
      
      case 4: {
        int32_t _tmp$1430;
        tag_1$155 = _cursor$146;
        tag_4$161 = _cursor$146;
        _tmp$1430 = _cursor$146;
        if (_tmp$1430 < _end$145) {
          int32_t _p$974 = _cursor$146;
          int32_t next_char$199 = _data$143[_p$974];
          int32_t _tmp$1431 = _cursor$146;
          _cursor$146 = _tmp$1431 + 1;
          if (next_char$199 < 55296) {
            if (next_char$199 < 58) {
              if (next_char$199 < 48) {
                goto $join$198;
              } else {
                dispatch_19$183 = 4;
                goto $loop_label_19$186;
              }
            } else if (next_char$199 > 58) {
              goto $join$198;
            } else {
              int32_t _tmp$1432;
              tag_1_2$157 = tag_1_1$156;
              tag_1_1$156 = tag_1$155;
              tag_1$155 = _cursor$146;
              _tmp$1432 = _cursor$146;
              if (_tmp$1432 < _end$145) {
                int32_t _p$977 = _cursor$146;
                int32_t next_char$201 = _data$143[_p$977];
                int32_t _tmp$1433 = _cursor$146;
                _cursor$146 = _tmp$1433 + 1;
                if (next_char$201 < 55296) {
                  if (next_char$201 < 58) {
                    if (next_char$201 < 48) {
                      goto $join$200;
                    } else {
                      int32_t _tmp$1434;
                      tag_1$155 = _cursor$146;
                      tag_2_1$160 = tag_2$159;
                      tag_2$159 = _cursor$146;
                      _tmp$1434 = _cursor$146;
                      if (_tmp$1434 < _end$145) {
                        int32_t _p$980 = _cursor$146;
                        int32_t next_char$203 = _data$143[_p$980];
                        int32_t _tmp$1435 = _cursor$146;
                        _cursor$146 = _tmp$1435 + 1;
                        if (next_char$203 < 55296) {
                          if (next_char$203 < 58) {
                            if (next_char$203 < 48) {
                              goto $join$202;
                            } else {
                              dispatch_19$183 = 5;
                              goto $loop_label_19$186;
                            }
                          } else if (next_char$203 > 58) {
                            goto $join$202;
                          } else {
                            dispatch_19$183 = 3;
                            goto $loop_label_19$186;
                          }
                        } else if (next_char$203 > 56318) {
                          if (next_char$203 < 57344) {
                            dispatch_19$183 = 8;
                            goto $loop_label_19$186;
                          } else if (next_char$203 > 65535) {
                            goto $join$162;
                          } else {
                            goto $join$202;
                          }
                        } else {
                          dispatch_19$183 = 7;
                          goto $loop_label_19$186;
                        }
                        $join$202:;
                        dispatch_19$183 = 0;
                        goto $loop_label_19$186;
                      } else {
                        goto $join$188;
                      }
                    }
                  } else if (next_char$201 > 58) {
                    goto $join$200;
                  } else {
                    dispatch_19$183 = 1;
                    goto $loop_label_19$186;
                  }
                } else if (next_char$201 > 56318) {
                  if (next_char$201 < 57344) {
                    dispatch_19$183 = 8;
                    goto $loop_label_19$186;
                  } else if (next_char$201 > 65535) {
                    goto $join$162;
                  } else {
                    goto $join$200;
                  }
                } else {
                  dispatch_19$183 = 7;
                  goto $loop_label_19$186;
                }
                $join$200:;
                dispatch_19$183 = 0;
                goto $loop_label_19$186;
              } else {
                goto $join$162;
              }
            }
          } else if (next_char$199 > 56318) {
            if (next_char$199 < 57344) {
              dispatch_19$183 = 8;
              goto $loop_label_19$186;
            } else if (next_char$199 > 65535) {
              goto $join$162;
            } else {
              goto $join$198;
            }
          } else {
            dispatch_19$183 = 7;
            goto $loop_label_19$186;
          }
          $join$198:;
          dispatch_19$183 = 0;
          goto $loop_label_19$186;
        } else {
          goto $join$162;
        }
        break;
      }
      
      case 5: {
        int32_t _tmp$1436;
        tag_1$155 = _cursor$146;
        tag_2$159 = _cursor$146;
        _tmp$1436 = _cursor$146;
        if (_tmp$1436 < _end$145) {
          int32_t _p$983 = _cursor$146;
          int32_t next_char$205 = _data$143[_p$983];
          int32_t _tmp$1437 = _cursor$146;
          _cursor$146 = _tmp$1437 + 1;
          if (next_char$205 < 55296) {
            if (next_char$205 < 58) {
              if (next_char$205 < 48) {
                goto $join$204;
              } else {
                dispatch_19$183 = 5;
                goto $loop_label_19$186;
              }
            } else if (next_char$205 > 58) {
              goto $join$204;
            } else {
              dispatch_19$183 = 3;
              goto $loop_label_19$186;
            }
          } else if (next_char$205 > 56318) {
            if (next_char$205 < 57344) {
              dispatch_19$183 = 8;
              goto $loop_label_19$186;
            } else if (next_char$205 > 65535) {
              goto $join$162;
            } else {
              goto $join$204;
            }
          } else {
            dispatch_19$183 = 7;
            goto $loop_label_19$186;
          }
          $join$204:;
          dispatch_19$183 = 0;
          goto $loop_label_19$186;
        } else {
          goto $join$188;
        }
        break;
      }
      
      case 6: {
        int32_t _tmp$1438;
        tag_1$155 = _cursor$146;
        tag_2$159 = _cursor$146;
        tag_3$158 = _cursor$146;
        _tmp$1438 = _cursor$146;
        if (_tmp$1438 < _end$145) {
          int32_t _p$986 = _cursor$146;
          int32_t next_char$207 = _data$143[_p$986];
          int32_t _tmp$1439 = _cursor$146;
          _cursor$146 = _tmp$1439 + 1;
          if (next_char$207 < 59) {
            if (next_char$207 < 46) {
              if (next_char$207 < 45) {
                goto $join$206;
              } else {
                goto $join$184;
              }
            } else if (next_char$207 > 47) {
              if (next_char$207 < 58) {
                dispatch_19$183 = 6;
                goto $loop_label_19$186;
              } else {
                dispatch_19$183 = 3;
                goto $loop_label_19$186;
              }
            } else {
              goto $join$206;
            }
          } else if (next_char$207 > 55295) {
            if (next_char$207 < 57344) {
              if (next_char$207 < 56319) {
                dispatch_19$183 = 7;
                goto $loop_label_19$186;
              } else {
                dispatch_19$183 = 8;
                goto $loop_label_19$186;
              }
            } else if (next_char$207 > 65535) {
              goto $join$162;
            } else {
              goto $join$206;
            }
          } else {
            goto $join$206;
          }
          $join$206:;
          dispatch_19$183 = 0;
          goto $loop_label_19$186;
        } else {
          goto $join$162;
        }
        break;
      }
      
      case 7: {
        int32_t _tmp$1440 = _cursor$146;
        if (_tmp$1440 < _end$145) {
          int32_t _p$989 = _cursor$146;
          int32_t next_char$208 = _data$143[_p$989];
          int32_t _tmp$1441 = _cursor$146;
          _cursor$146 = _tmp$1441 + 1;
          if (next_char$208 < 56320) {
            goto $join$162;
          } else if (next_char$208 > 65535) {
            goto $join$162;
          } else {
            dispatch_19$183 = 0;
            goto $loop_label_19$186;
          }
        } else {
          goto $join$162;
        }
        break;
      }
      
      case 1: {
        int32_t _tmp$1442;
        tag_1_1$156 = tag_1$155;
        tag_1$155 = _cursor$146;
        _tmp$1442 = _cursor$146;
        if (_tmp$1442 < _end$145) {
          int32_t _p$992 = _cursor$146;
          int32_t next_char$210 = _data$143[_p$992];
          int32_t _tmp$1443 = _cursor$146;
          _cursor$146 = _tmp$1443 + 1;
          if (next_char$210 < 55296) {
            if (next_char$210 < 58) {
              if (next_char$210 < 48) {
                goto $join$209;
              } else {
                dispatch_19$183 = 2;
                goto $loop_label_19$186;
              }
            } else if (next_char$210 > 58) {
              goto $join$209;
            } else {
              dispatch_19$183 = 1;
              goto $loop_label_19$186;
            }
          } else if (next_char$210 > 56318) {
            if (next_char$210 < 57344) {
              dispatch_19$183 = 8;
              goto $loop_label_19$186;
            } else if (next_char$210 > 65535) {
              goto $join$162;
            } else {
              goto $join$209;
            }
          } else {
            dispatch_19$183 = 7;
            goto $loop_label_19$186;
          }
          $join$209:;
          dispatch_19$183 = 0;
          goto $loop_label_19$186;
        } else {
          goto $join$162;
        }
        break;
      }
      default: {
        goto $join$162;
        break;
      }
    }
    $join$188:;
    tag_1$155 = tag_1_2$157;
    tag_2$159 = tag_2_1$160;
    match_tag_saver_0$149 = tag_0$154;
    match_tag_saver_1$150 = tag_1$155;
    match_tag_saver_2$151 = tag_2$159;
    match_tag_saver_3$152 = tag_3$158;
    match_tag_saver_4$153 = tag_4$161;
    accept_state$147 = 0;
    match_end$148 = _cursor$146;
    goto $join$162;
    $join$184:;
    tag_1_1$156 = tag_1_2$157;
    tag_1$155 = _cursor$146;
    tag_2$159 = tag_2_1$160;
    _tmp$1418 = _cursor$146;
    if (_tmp$1418 < _end$145) {
      int32_t _p$995 = _cursor$146;
      int32_t next_char$187 = _data$143[_p$995];
      int32_t _tmp$1419 = _cursor$146;
      _cursor$146 = _tmp$1419 + 1;
      if (next_char$187 < 55296) {
        if (next_char$187 < 58) {
          if (next_char$187 < 48) {
            goto $join$185;
          } else {
            dispatch_19$183 = 4;
            continue;
          }
        } else if (next_char$187 > 58) {
          goto $join$185;
        } else {
          dispatch_19$183 = 1;
          continue;
        }
      } else if (next_char$187 > 56318) {
        if (next_char$187 < 57344) {
          dispatch_19$183 = 8;
          continue;
        } else if (next_char$187 > 65535) {
          goto $join$162;
        } else {
          goto $join$185;
        }
      } else {
        dispatch_19$183 = 7;
        continue;
      }
      $join$185:;
      dispatch_19$183 = 0;
      continue;
    } else {
      goto $join$162;
    }
    break;
  }
  $join$162:;
  switch (accept_state$147) {
    case 0: {
      void* _try_err$165;
      struct $StringView start_line$163;
      int32_t _tmp$1415 = match_tag_saver_1$150;
      int32_t _tmp$1414 = _tmp$1415 + 1;
      int64_t _tmp$1411 = (int64_t)_tmp$1414;
      int32_t _tmp$1413 = match_tag_saver_2$151;
      int64_t _tmp$1412 = (int64_t)_tmp$1413;
      struct moonbit_result_2 _tmp$2924;
      void* _try_err$168;
      struct $StringView start_column$166;
      int32_t _tmp$1408;
      int32_t _tmp$1407;
      int64_t _tmp$1404;
      int32_t _tmp$1406;
      int64_t _tmp$1405;
      struct moonbit_result_2 _tmp$2926;
      void* _try_err$171;
      struct $StringView pkg$169;
      int32_t _tmp$1401;
      int64_t _tmp$1398;
      int32_t _tmp$1400;
      int64_t _tmp$1399;
      struct moonbit_result_2 _tmp$2928;
      void* _try_err$174;
      struct $StringView filename$172;
      int32_t _tmp$1395;
      int32_t _tmp$1394;
      int64_t _tmp$1391;
      int32_t _tmp$1393;
      int64_t _tmp$1392;
      struct moonbit_result_2 _tmp$2930;
      void* _try_err$177;
      struct $StringView end_line$175;
      int32_t _tmp$1388;
      int32_t _tmp$1387;
      int64_t _tmp$1384;
      int32_t _tmp$1386;
      int64_t _tmp$1385;
      struct moonbit_result_2 _tmp$2932;
      void* _try_err$180;
      struct $StringView end_column$178;
      int32_t _tmp$1381;
      int32_t _tmp$1380;
      int64_t _tmp$1377;
      int32_t _tmp$1379;
      int64_t _tmp$1378;
      struct moonbit_result_2 _tmp$2934;
      struct $$moonbitlang$core$builtin$SourceLocRepr* _block$2935;
      moonbit_incref(_data$143);
      _tmp$2924 = $String$$sub(_data$143, _tmp$1411, _tmp$1412);
      if (_tmp$2924.tag) {
        struct $StringView const _ok$1416 = _tmp$2924.data.ok;
        start_line$163 = _ok$1416;
      } else {
        void* const _err$1417 = _tmp$2924.data.err;
        _try_err$165 = _err$1417;
        goto $join$164;
      }
      goto $joinlet$2923;
      $join$164:;
      moonbit_decref(_try_err$165);
      moonbit_panic();
      $joinlet$2923:;
      _tmp$1408 = match_tag_saver_2$151;
      _tmp$1407 = _tmp$1408 + 1;
      _tmp$1404 = (int64_t)_tmp$1407;
      _tmp$1406 = match_tag_saver_3$152;
      _tmp$1405 = (int64_t)_tmp$1406;
      moonbit_incref(_data$143);
      _tmp$2926 = $String$$sub(_data$143, _tmp$1404, _tmp$1405);
      if (_tmp$2926.tag) {
        struct $StringView const _ok$1409 = _tmp$2926.data.ok;
        start_column$166 = _ok$1409;
      } else {
        void* const _err$1410 = _tmp$2926.data.err;
        _try_err$168 = _err$1410;
        goto $join$167;
      }
      goto $joinlet$2925;
      $join$167:;
      moonbit_decref(_try_err$168);
      moonbit_panic();
      $joinlet$2925:;
      _tmp$1401 = _start$144 + 1;
      _tmp$1398 = (int64_t)_tmp$1401;
      _tmp$1400 = match_tag_saver_0$149;
      _tmp$1399 = (int64_t)_tmp$1400;
      moonbit_incref(_data$143);
      _tmp$2928 = $String$$sub(_data$143, _tmp$1398, _tmp$1399);
      if (_tmp$2928.tag) {
        struct $StringView const _ok$1402 = _tmp$2928.data.ok;
        pkg$169 = _ok$1402;
      } else {
        void* const _err$1403 = _tmp$2928.data.err;
        _try_err$171 = _err$1403;
        goto $join$170;
      }
      goto $joinlet$2927;
      $join$170:;
      moonbit_decref(_try_err$171);
      moonbit_panic();
      $joinlet$2927:;
      _tmp$1395 = match_tag_saver_0$149;
      _tmp$1394 = _tmp$1395 + 1;
      _tmp$1391 = (int64_t)_tmp$1394;
      _tmp$1393 = match_tag_saver_1$150;
      _tmp$1392 = (int64_t)_tmp$1393;
      moonbit_incref(_data$143);
      _tmp$2930 = $String$$sub(_data$143, _tmp$1391, _tmp$1392);
      if (_tmp$2930.tag) {
        struct $StringView const _ok$1396 = _tmp$2930.data.ok;
        filename$172 = _ok$1396;
      } else {
        void* const _err$1397 = _tmp$2930.data.err;
        _try_err$174 = _err$1397;
        goto $join$173;
      }
      goto $joinlet$2929;
      $join$173:;
      moonbit_decref(_try_err$174);
      moonbit_panic();
      $joinlet$2929:;
      _tmp$1388 = match_tag_saver_3$152;
      _tmp$1387 = _tmp$1388 + 1;
      _tmp$1384 = (int64_t)_tmp$1387;
      _tmp$1386 = match_tag_saver_4$153;
      _tmp$1385 = (int64_t)_tmp$1386;
      moonbit_incref(_data$143);
      _tmp$2932 = $String$$sub(_data$143, _tmp$1384, _tmp$1385);
      if (_tmp$2932.tag) {
        struct $StringView const _ok$1389 = _tmp$2932.data.ok;
        end_line$175 = _ok$1389;
      } else {
        void* const _err$1390 = _tmp$2932.data.err;
        _try_err$177 = _err$1390;
        goto $join$176;
      }
      goto $joinlet$2931;
      $join$176:;
      moonbit_decref(_try_err$177);
      moonbit_panic();
      $joinlet$2931:;
      _tmp$1381 = match_tag_saver_4$153;
      _tmp$1380 = _tmp$1381 + 1;
      _tmp$1377 = (int64_t)_tmp$1380;
      _tmp$1379 = match_end$148;
      _tmp$1378 = (int64_t)_tmp$1379;
      _tmp$2934 = $String$$sub(_data$143, _tmp$1377, _tmp$1378);
      if (_tmp$2934.tag) {
        struct $StringView const _ok$1382 = _tmp$2934.data.ok;
        end_column$178 = _ok$1382;
      } else {
        void* const _err$1383 = _tmp$2934.data.err;
        _try_err$180 = _err$1383;
        goto $join$179;
      }
      goto $joinlet$2933;
      $join$179:;
      moonbit_decref(_try_err$180);
      moonbit_panic();
      $joinlet$2933:;
      _block$2935
      = (struct $$moonbitlang$core$builtin$SourceLocRepr*)moonbit_malloc(
          sizeof(struct $$moonbitlang$core$builtin$SourceLocRepr)
        );
      Moonbit_object_header(_block$2935)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $$moonbitlang$core$builtin$SourceLocRepr, $0_0) >> 2,
          6,
          0
      );
      _block$2935->$0_0 = pkg$169.$0;
      _block$2935->$0_1 = pkg$169.$1;
      _block$2935->$0_2 = pkg$169.$2;
      _block$2935->$1_0 = filename$172.$0;
      _block$2935->$1_1 = filename$172.$1;
      _block$2935->$1_2 = filename$172.$2;
      _block$2935->$2_0 = start_line$163.$0;
      _block$2935->$2_1 = start_line$163.$1;
      _block$2935->$2_2 = start_line$163.$2;
      _block$2935->$3_0 = start_column$166.$0;
      _block$2935->$3_1 = start_column$166.$1;
      _block$2935->$3_2 = start_column$166.$2;
      _block$2935->$4_0 = end_line$175.$0;
      _block$2935->$4_1 = end_line$175.$1;
      _block$2935->$4_2 = end_line$175.$2;
      _block$2935->$5_0 = end_column$178.$0;
      _block$2935->$5_1 = end_column$178.$1;
      _block$2935->$5_2 = end_column$178.$2;
      return _block$2935;
      break;
    }
    default: {
      moonbit_decref(_data$143);
      moonbit_panic();
      break;
    }
  }
}

moonbit_bytes_t $$moonbitlang$core$builtin$Array$$buffer$4(
  struct $$moonbitlang$core$builtin$Array$3c$Byte$3e$* self$140
) {
  moonbit_bytes_t _field$2669 = self$140->$0;
  int32_t _cnt$2765 = Moonbit_object_header(self$140)->rc;
  if (_cnt$2765 > 1) {
    int32_t _new_cnt$2766 = _cnt$2765 - 1;
    Moonbit_object_header(self$140)->rc = _new_cnt$2766;
    moonbit_incref(_field$2669);
  } else if (_cnt$2765 == 1) {
    moonbit_free(self$140);
  }
  return _field$2669;
}

int32_t* $$moonbitlang$core$builtin$Array$$buffer$3(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$139
) {
  int32_t* _field$2670 = self$139->$0;
  int32_t _cnt$2767 = Moonbit_object_header(self$139)->rc;
  if (_cnt$2767 > 1) {
    int32_t _new_cnt$2768 = _cnt$2767 - 1;
    Moonbit_object_header(self$139)->rc = _new_cnt$2768;
    moonbit_incref(_field$2670);
  } else if (_cnt$2767 == 1) {
    moonbit_free(self$139);
  }
  return _field$2670;
}

struct $QueryDef** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$QueryDef$3e$* self$138
) {
  struct $QueryDef** _field$2671 = self$138->$0;
  int32_t _cnt$2769 = Moonbit_object_header(self$138)->rc;
  if (_cnt$2769 > 1) {
    int32_t _new_cnt$2770 = _cnt$2769 - 1;
    Moonbit_object_header(self$138)->rc = _new_cnt$2770;
    moonbit_incref(_field$2671);
  } else if (_cnt$2769 == 1) {
    moonbit_free(self$138);
  }
  return _field$2671;
}

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$137
) {
  moonbit_string_t* _field$2672 = self$137->$0;
  int32_t _cnt$2771 = Moonbit_object_header(self$137)->rc;
  if (_cnt$2771 > 1) {
    int32_t _new_cnt$2772 = _cnt$2771 - 1;
    Moonbit_object_header(self$137)->rc = _new_cnt$2772;
    moonbit_incref(_field$2672);
  } else if (_cnt$2771 == 1) {
    moonbit_free(self$137);
  }
  return _field$2672;
}

struct $$3c$String$2a$String$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* self$136
) {
  struct $$3c$String$2a$String$3e$** _field$2673 = self$136->$0;
  int32_t _cnt$2773 = Moonbit_object_header(self$136)->rc;
  if (_cnt$2773 > 1) {
    int32_t _new_cnt$2774 = _cnt$2773 - 1;
    Moonbit_object_header(self$136)->rc = _new_cnt$2774;
    moonbit_incref(_field$2673);
  } else if (_cnt$2773 == 1) {
    moonbit_free(self$136);
  }
  return _field$2673;
}

int32_t $String$$unsafe_char_at(moonbit_string_t self$133, int32_t index$134) {
  int32_t c1$132 = self$133[index$134];
  if (55296 <= c1$132 && c1$132 <= 56319) {
    int32_t _tmp$1376 = index$134 + 1;
    int32_t _tmp$2674 = self$133[_tmp$1376];
    int32_t c2$135;
    moonbit_decref(self$133);
    c2$135 = _tmp$2674;
    return $moonbitlang$core$builtin$code_point_of_surrogate_pair(
             c1$132, c2$135
           );
  } else {
    moonbit_decref(self$133);
    return c1$132;
  }
}

int32_t $moonbitlang$core$builtin$code_point_of_surrogate_pair(
  int32_t leading$130,
  int32_t trailing$131
) {
  int32_t _tmp$1375 = leading$130 - 55296;
  int32_t _tmp$1374 = _tmp$1375 * 1024;
  int32_t _tmp$1373 = _tmp$1374 + trailing$131;
  int32_t _tmp$1372 = _tmp$1373 - 56320;
  int32_t _tmp$1371 = _tmp$1372 + 65536;
  return _tmp$1371;
}

int32_t $String$$unsafe_charcode_at(
  moonbit_string_t self$128,
  int32_t idx$129
) {
  int32_t _tmp$2675 = self$128[idx$129];
  moonbit_decref(self$128);
  return _tmp$2675;
}

int32_t $Int$$is_trailing_surrogate(int32_t self$127) {
  return 56320 <= self$127 && self$127 <= 57343;
}

int32_t $Int$$is_leading_surrogate(int32_t self$126) {
  return 55296 <= self$126 && self$126 <= 56319;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
  struct $$moonbitlang$core$builtin$StringBuilder* self$123,
  int32_t ch$125
) {
  int32_t len$1366 = self$123->$1;
  int32_t _tmp$1365 = len$1366 + 4;
  moonbit_bytes_t _field$2676;
  moonbit_bytes_t data$1369;
  int32_t len$1370;
  int32_t inc$124;
  int32_t len$1368;
  int32_t _tmp$1367;
  moonbit_incref(self$123);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$123, _tmp$1365
  );
  _field$2676 = self$123->$0;
  data$1369 = _field$2676;
  len$1370 = self$123->$1;
  moonbit_incref(data$1369);
  inc$124 = $FixedArray$$set_utf16le_char(data$1369, len$1370, ch$125);
  len$1368 = self$123->$1;
  _tmp$1367 = len$1368 + inc$124;
  self$123->$1 = _tmp$1367;
  moonbit_decref(self$123);
  return 0;
}

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$118,
  int32_t required$119
) {
  moonbit_bytes_t _field$2680 = self$118->$0;
  moonbit_bytes_t data$1364 = _field$2680;
  int32_t _tmp$2679 = Moonbit_array_length(data$1364);
  int32_t current_len$117 = _tmp$2679;
  int32_t enough_space$120;
  int32_t _tmp$1363;
  moonbit_bytes_t new_data$122;
  moonbit_bytes_t _field$2678;
  moonbit_bytes_t data$1361;
  int32_t len$1362;
  moonbit_bytes_t _old$2677;
  if (required$119 <= current_len$117) {
    moonbit_decref(self$118);
    return 0;
  }
  enough_space$120 = current_len$117;
  while (1) {
    int32_t _tmp$1359 = enough_space$120;
    if (_tmp$1359 < required$119) {
      int32_t _tmp$1360 = enough_space$120;
      enough_space$120 = _tmp$1360 * 2;
      continue;
    }
    break;
  }
  _tmp$1363 = enough_space$120;
  new_data$122 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1363, 0);
  _field$2678 = self$118->$0;
  data$1361 = _field$2678;
  len$1362 = self$118->$1;
  moonbit_incref(data$1361);
  moonbit_incref(new_data$122);
  $FixedArray$$unsafe_blit$0(new_data$122, 0, data$1361, 0, len$1362);
  _old$2677 = self$118->$0;
  moonbit_decref(_old$2677);
  self$118->$0 = new_data$122;
  moonbit_decref(self$118);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Default$$Byte$$default() {
  return 0;
}

int32_t $FixedArray$$set_utf16le_char(
  moonbit_bytes_t self$112,
  int32_t offset$113,
  int32_t value$111
) {
  int32_t _tmp$1358 = value$111;
  uint32_t code$110 = *(uint32_t*)&_tmp$1358;
  if (code$110 < 65536u) {
    uint32_t _p$916 = code$110 & 255u;
    int32_t _tmp$1341 = *(int32_t*)&_p$916;
    int32_t _tmp$1340 = _tmp$1341 & 0xff;
    int32_t _tmp$1342;
    uint32_t _p$919;
    int32_t _tmp$1344;
    int32_t _tmp$1343;
    if (offset$113 < 0 || offset$113 >= Moonbit_array_length(self$112)) {
      moonbit_panic();
    }
    self$112[offset$113] = _tmp$1340;
    _tmp$1342 = offset$113 + 1;
    _p$919 = code$110 >> 8;
    _tmp$1344 = *(int32_t*)&_p$919;
    _tmp$1343 = _tmp$1344 & 0xff;
    if (_tmp$1342 < 0 || _tmp$1342 >= Moonbit_array_length(self$112)) {
      moonbit_panic();
    }
    self$112[_tmp$1342] = _tmp$1343;
    moonbit_decref(self$112);
    return 2;
  } else if (code$110 < 1114112u) {
    uint32_t hi$114 = code$110 - 65536u;
    uint32_t _tmp$1357 = hi$114 >> 10;
    uint32_t lo$115 = _tmp$1357 | 55296u;
    uint32_t _tmp$1356 = hi$114 & 1023u;
    uint32_t hi$116 = _tmp$1356 | 56320u;
    uint32_t _p$922 = lo$115 & 255u;
    int32_t _tmp$1346 = *(int32_t*)&_p$922;
    int32_t _tmp$1345 = _tmp$1346 & 0xff;
    int32_t _tmp$1347;
    uint32_t _p$925;
    int32_t _tmp$1349;
    int32_t _tmp$1348;
    int32_t _tmp$1350;
    uint32_t _p$928;
    int32_t _tmp$1352;
    int32_t _tmp$1351;
    int32_t _tmp$1353;
    uint32_t _p$931;
    int32_t _tmp$1355;
    int32_t _tmp$1354;
    if (offset$113 < 0 || offset$113 >= Moonbit_array_length(self$112)) {
      moonbit_panic();
    }
    self$112[offset$113] = _tmp$1345;
    _tmp$1347 = offset$113 + 1;
    _p$925 = lo$115 >> 8;
    _tmp$1349 = *(int32_t*)&_p$925;
    _tmp$1348 = _tmp$1349 & 0xff;
    if (_tmp$1347 < 0 || _tmp$1347 >= Moonbit_array_length(self$112)) {
      moonbit_panic();
    }
    self$112[_tmp$1347] = _tmp$1348;
    _tmp$1350 = offset$113 + 2;
    _p$928 = hi$116 & 255u;
    _tmp$1352 = *(int32_t*)&_p$928;
    _tmp$1351 = _tmp$1352 & 0xff;
    if (_tmp$1350 < 0 || _tmp$1350 >= Moonbit_array_length(self$112)) {
      moonbit_panic();
    }
    self$112[_tmp$1350] = _tmp$1351;
    _tmp$1353 = offset$113 + 3;
    _p$931 = hi$116 >> 8;
    _tmp$1355 = *(int32_t*)&_p$931;
    _tmp$1354 = _tmp$1355 & 0xff;
    if (_tmp$1353 < 0 || _tmp$1353 >= Moonbit_array_length(self$112)) {
      moonbit_panic();
    }
    self$112[_tmp$1353] = _tmp$1354;
    moonbit_decref(self$112);
    return 4;
  } else {
    moonbit_decref(self$112);
    return $moonbitlang$core$builtin$abort$0(
             (moonbit_string_t)moonbit_string_literal_130.data,
               (moonbit_string_t)moonbit_string_literal_131.data
           );
  }
}

int32_t $UInt$$to_byte(uint32_t self$109) {
  int32_t _tmp$1339 = *(int32_t*)&self$109;
  return _tmp$1339 & 0xff;
}

uint32_t $Char$$to_uint(int32_t self$108) {
  int32_t _tmp$1338 = self$108;
  return *(uint32_t*)&_tmp$1338;
}

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$107
) {
  moonbit_bytes_t _field$2682 = self$107->$0;
  moonbit_bytes_t data$1337 = _field$2682;
  moonbit_bytes_t _tmp$1334;
  int32_t _field$2681;
  int32_t len$1336;
  int64_t _tmp$1335;
  moonbit_incref(data$1337);
  _tmp$1334 = data$1337;
  _field$2681 = self$107->$1;
  moonbit_decref(self$107);
  len$1336 = _field$2681;
  _tmp$1335 = (int64_t)len$1336;
  return $Bytes$$to_unchecked_string$inner(_tmp$1334, 0, _tmp$1335);
}

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$102,
  int32_t offset$106,
  int64_t length$104
) {
  int32_t len$101 = Moonbit_array_length(self$102);
  int32_t length$103;
  int32_t _if_result$2937;
  if (length$104 == 4294967296ll) {
    length$103 = len$101 - offset$106;
  } else {
    int64_t _Some$105 = length$104;
    length$103 = (int32_t)_Some$105;
  }
  if (offset$106 >= 0) {
    if (length$103 >= 0) {
      int32_t _tmp$1333 = offset$106 + length$103;
      _if_result$2937 = _tmp$1333 <= len$101;
    } else {
      _if_result$2937 = 0;
    }
  } else {
    _if_result$2937 = 0;
  }
  if (_if_result$2937) {
    return $moonbitlang$core$builtin$unsafe_sub_string(
             self$102, offset$106, length$103
           );
  } else {
    moonbit_decref(self$102);
    moonbit_panic();
  }
}

struct $$moonbitlang$core$builtin$StringBuilder* $$moonbitlang$core$builtin$StringBuilder$$new$inner(
  int32_t size_hint$99
) {
  int32_t initial$98;
  moonbit_bytes_t data$100;
  struct $$moonbitlang$core$builtin$StringBuilder* _block$2938;
  if (size_hint$99 < 1) {
    initial$98 = 1;
  } else {
    initial$98 = size_hint$99;
  }
  data$100 = (moonbit_bytes_t)moonbit_make_bytes(initial$98, 0);
  _block$2938
  = (struct $$moonbitlang$core$builtin$StringBuilder*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$StringBuilder)
    );
  Moonbit_object_header(_block$2938)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$StringBuilder, $0) >> 2, 1, 0
  );
  _block$2938->$0 = data$100;
  _block$2938->$1 = 0;
  return _block$2938;
}

int32_t $Byte$$to_char(int32_t self$97) {
  int32_t _tmp$1332 = (int32_t)self$97;
  return _tmp$1332;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$4(
  moonbit_bytes_t dst$92,
  int32_t dst_offset$93,
  moonbit_bytes_t src$94,
  int32_t src_offset$95,
  int32_t len$96
) {
  $FixedArray$$unsafe_blit$5(
    dst$92, dst_offset$93, src$94, src_offset$95, len$96
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$3(
  int32_t* dst$87,
  int32_t dst_offset$88,
  int32_t* src$89,
  int32_t src_offset$90,
  int32_t len$91
) {
  $FixedArray$$unsafe_blit$4(
    dst$87, dst_offset$88, src$89, src_offset$90, len$91
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
  struct $QueryDef** dst$82,
  int32_t dst_offset$83,
  struct $QueryDef** src$84,
  int32_t src_offset$85,
  int32_t len$86
) {
  $FixedArray$$unsafe_blit$3(
    dst$82, dst_offset$83, src$84, src_offset$85, len$86
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
  moonbit_string_t* dst$77,
  int32_t dst_offset$78,
  moonbit_string_t* src$79,
  int32_t src_offset$80,
  int32_t len$81
) {
  $FixedArray$$unsafe_blit$2(
    dst$77, dst_offset$78, src$79, src_offset$80, len$81
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
  struct $$3c$String$2a$String$3e$** dst$72,
  int32_t dst_offset$73,
  struct $$3c$String$2a$String$3e$** src$74,
  int32_t src_offset$75,
  int32_t len$76
) {
  $FixedArray$$unsafe_blit$1(
    dst$72, dst_offset$73, src$74, src_offset$75, len$76
  );
  return 0;
}

int32_t $FixedArray$$unsafe_blit$5(
  moonbit_bytes_t dst$63,
  int32_t dst_offset$65,
  moonbit_bytes_t src$64,
  int32_t src_offset$66,
  int32_t len$68
) {
  if (dst$63 == src$64 && dst_offset$65 < src_offset$66) {
    int32_t i$67 = 0;
    while (1) {
      if (i$67 < len$68) {
        int32_t _tmp$1323 = dst_offset$65 + i$67;
        int32_t _tmp$1325 = src_offset$66 + i$67;
        int32_t _tmp$1324;
        int32_t _tmp$1326;
        if (_tmp$1325 < 0 || _tmp$1325 >= Moonbit_array_length(src$64)) {
          moonbit_panic();
        }
        _tmp$1324 = (int32_t)src$64[_tmp$1325];
        if (_tmp$1323 < 0 || _tmp$1323 >= Moonbit_array_length(dst$63)) {
          moonbit_panic();
        }
        dst$63[_tmp$1323] = _tmp$1324;
        _tmp$1326 = i$67 + 1;
        i$67 = _tmp$1326;
        continue;
      } else {
        moonbit_decref(src$64);
        moonbit_decref(dst$63);
      }
      break;
    }
  } else {
    int32_t _tmp$1331 = len$68 - 1;
    int32_t i$70 = _tmp$1331;
    while (1) {
      if (i$70 >= 0) {
        int32_t _tmp$1327 = dst_offset$65 + i$70;
        int32_t _tmp$1329 = src_offset$66 + i$70;
        int32_t _tmp$1328;
        int32_t _tmp$1330;
        if (_tmp$1329 < 0 || _tmp$1329 >= Moonbit_array_length(src$64)) {
          moonbit_panic();
        }
        _tmp$1328 = (int32_t)src$64[_tmp$1329];
        if (_tmp$1327 < 0 || _tmp$1327 >= Moonbit_array_length(dst$63)) {
          moonbit_panic();
        }
        dst$63[_tmp$1327] = _tmp$1328;
        _tmp$1330 = i$70 - 1;
        i$70 = _tmp$1330;
        continue;
      } else {
        moonbit_decref(src$64);
        moonbit_decref(dst$63);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$4(
  int32_t* dst$54,
  int32_t dst_offset$56,
  int32_t* src$55,
  int32_t src_offset$57,
  int32_t len$59
) {
  if (dst$54 == src$55 && dst_offset$56 < src_offset$57) {
    int32_t i$58 = 0;
    while (1) {
      if (i$58 < len$59) {
        int32_t _tmp$1314 = dst_offset$56 + i$58;
        int32_t _tmp$1316 = src_offset$57 + i$58;
        int32_t _tmp$1315;
        int32_t _tmp$1317;
        if (_tmp$1316 < 0 || _tmp$1316 >= Moonbit_array_length(src$55)) {
          moonbit_panic();
        }
        _tmp$1315 = (int32_t)src$55[_tmp$1316];
        if (_tmp$1314 < 0 || _tmp$1314 >= Moonbit_array_length(dst$54)) {
          moonbit_panic();
        }
        dst$54[_tmp$1314] = _tmp$1315;
        _tmp$1317 = i$58 + 1;
        i$58 = _tmp$1317;
        continue;
      } else {
        moonbit_decref(src$55);
        moonbit_decref(dst$54);
      }
      break;
    }
  } else {
    int32_t _tmp$1322 = len$59 - 1;
    int32_t i$61 = _tmp$1322;
    while (1) {
      if (i$61 >= 0) {
        int32_t _tmp$1318 = dst_offset$56 + i$61;
        int32_t _tmp$1320 = src_offset$57 + i$61;
        int32_t _tmp$1319;
        int32_t _tmp$1321;
        if (_tmp$1320 < 0 || _tmp$1320 >= Moonbit_array_length(src$55)) {
          moonbit_panic();
        }
        _tmp$1319 = (int32_t)src$55[_tmp$1320];
        if (_tmp$1318 < 0 || _tmp$1318 >= Moonbit_array_length(dst$54)) {
          moonbit_panic();
        }
        dst$54[_tmp$1318] = _tmp$1319;
        _tmp$1321 = i$61 - 1;
        i$61 = _tmp$1321;
        continue;
      } else {
        moonbit_decref(src$55);
        moonbit_decref(dst$54);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$3(
  struct $QueryDef** dst$45,
  int32_t dst_offset$47,
  struct $QueryDef** src$46,
  int32_t src_offset$48,
  int32_t len$50
) {
  if (dst$45 == src$46 && dst_offset$47 < src_offset$48) {
    int32_t i$49 = 0;
    while (1) {
      if (i$49 < len$50) {
        int32_t _tmp$1305 = dst_offset$47 + i$49;
        int32_t _tmp$1307 = src_offset$48 + i$49;
        struct $QueryDef* _tmp$2684;
        struct $QueryDef* _tmp$1306;
        struct $QueryDef* _old$2683;
        int32_t _tmp$1308;
        if (_tmp$1307 < 0 || _tmp$1307 >= Moonbit_array_length(src$46)) {
          moonbit_panic();
        }
        _tmp$2684 = (struct $QueryDef*)src$46[_tmp$1307];
        _tmp$1306 = _tmp$2684;
        if (_tmp$1305 < 0 || _tmp$1305 >= Moonbit_array_length(dst$45)) {
          moonbit_panic();
        }
        _old$2683 = (struct $QueryDef*)dst$45[_tmp$1305];
        if (_tmp$1306) {
          moonbit_incref(_tmp$1306);
        }
        if (_old$2683) {
          moonbit_decref(_old$2683);
        }
        dst$45[_tmp$1305] = _tmp$1306;
        _tmp$1308 = i$49 + 1;
        i$49 = _tmp$1308;
        continue;
      } else {
        moonbit_decref(src$46);
        moonbit_decref(dst$45);
      }
      break;
    }
  } else {
    int32_t _tmp$1313 = len$50 - 1;
    int32_t i$52 = _tmp$1313;
    while (1) {
      if (i$52 >= 0) {
        int32_t _tmp$1309 = dst_offset$47 + i$52;
        int32_t _tmp$1311 = src_offset$48 + i$52;
        struct $QueryDef* _tmp$2686;
        struct $QueryDef* _tmp$1310;
        struct $QueryDef* _old$2685;
        int32_t _tmp$1312;
        if (_tmp$1311 < 0 || _tmp$1311 >= Moonbit_array_length(src$46)) {
          moonbit_panic();
        }
        _tmp$2686 = (struct $QueryDef*)src$46[_tmp$1311];
        _tmp$1310 = _tmp$2686;
        if (_tmp$1309 < 0 || _tmp$1309 >= Moonbit_array_length(dst$45)) {
          moonbit_panic();
        }
        _old$2685 = (struct $QueryDef*)dst$45[_tmp$1309];
        if (_tmp$1310) {
          moonbit_incref(_tmp$1310);
        }
        if (_old$2685) {
          moonbit_decref(_old$2685);
        }
        dst$45[_tmp$1309] = _tmp$1310;
        _tmp$1312 = i$52 - 1;
        i$52 = _tmp$1312;
        continue;
      } else {
        moonbit_decref(src$46);
        moonbit_decref(dst$45);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$2(
  moonbit_string_t* dst$36,
  int32_t dst_offset$38,
  moonbit_string_t* src$37,
  int32_t src_offset$39,
  int32_t len$41
) {
  if (dst$36 == src$37 && dst_offset$38 < src_offset$39) {
    int32_t i$40 = 0;
    while (1) {
      if (i$40 < len$41) {
        int32_t _tmp$1296 = dst_offset$38 + i$40;
        int32_t _tmp$1298 = src_offset$39 + i$40;
        moonbit_string_t _tmp$2688;
        moonbit_string_t _tmp$1297;
        moonbit_string_t _old$2687;
        int32_t _tmp$1299;
        if (_tmp$1298 < 0 || _tmp$1298 >= Moonbit_array_length(src$37)) {
          moonbit_panic();
        }
        _tmp$2688 = (moonbit_string_t)src$37[_tmp$1298];
        _tmp$1297 = _tmp$2688;
        if (_tmp$1296 < 0 || _tmp$1296 >= Moonbit_array_length(dst$36)) {
          moonbit_panic();
        }
        _old$2687 = (moonbit_string_t)dst$36[_tmp$1296];
        moonbit_incref(_tmp$1297);
        moonbit_decref(_old$2687);
        dst$36[_tmp$1296] = _tmp$1297;
        _tmp$1299 = i$40 + 1;
        i$40 = _tmp$1299;
        continue;
      } else {
        moonbit_decref(src$37);
        moonbit_decref(dst$36);
      }
      break;
    }
  } else {
    int32_t _tmp$1304 = len$41 - 1;
    int32_t i$43 = _tmp$1304;
    while (1) {
      if (i$43 >= 0) {
        int32_t _tmp$1300 = dst_offset$38 + i$43;
        int32_t _tmp$1302 = src_offset$39 + i$43;
        moonbit_string_t _tmp$2690;
        moonbit_string_t _tmp$1301;
        moonbit_string_t _old$2689;
        int32_t _tmp$1303;
        if (_tmp$1302 < 0 || _tmp$1302 >= Moonbit_array_length(src$37)) {
          moonbit_panic();
        }
        _tmp$2690 = (moonbit_string_t)src$37[_tmp$1302];
        _tmp$1301 = _tmp$2690;
        if (_tmp$1300 < 0 || _tmp$1300 >= Moonbit_array_length(dst$36)) {
          moonbit_panic();
        }
        _old$2689 = (moonbit_string_t)dst$36[_tmp$1300];
        moonbit_incref(_tmp$1301);
        moonbit_decref(_old$2689);
        dst$36[_tmp$1300] = _tmp$1301;
        _tmp$1303 = i$43 - 1;
        i$43 = _tmp$1303;
        continue;
      } else {
        moonbit_decref(src$37);
        moonbit_decref(dst$36);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$1(
  struct $$3c$String$2a$String$3e$** dst$27,
  int32_t dst_offset$29,
  struct $$3c$String$2a$String$3e$** src$28,
  int32_t src_offset$30,
  int32_t len$32
) {
  if (dst$27 == src$28 && dst_offset$29 < src_offset$30) {
    int32_t i$31 = 0;
    while (1) {
      if (i$31 < len$32) {
        int32_t _tmp$1287 = dst_offset$29 + i$31;
        int32_t _tmp$1289 = src_offset$30 + i$31;
        struct $$3c$String$2a$String$3e$* _tmp$2692;
        struct $$3c$String$2a$String$3e$* _tmp$1288;
        struct $$3c$String$2a$String$3e$* _old$2691;
        int32_t _tmp$1290;
        if (_tmp$1289 < 0 || _tmp$1289 >= Moonbit_array_length(src$28)) {
          moonbit_panic();
        }
        _tmp$2692 = (struct $$3c$String$2a$String$3e$*)src$28[_tmp$1289];
        _tmp$1288 = _tmp$2692;
        if (_tmp$1287 < 0 || _tmp$1287 >= Moonbit_array_length(dst$27)) {
          moonbit_panic();
        }
        _old$2691 = (struct $$3c$String$2a$String$3e$*)dst$27[_tmp$1287];
        if (_tmp$1288) {
          moonbit_incref(_tmp$1288);
        }
        if (_old$2691) {
          moonbit_decref(_old$2691);
        }
        dst$27[_tmp$1287] = _tmp$1288;
        _tmp$1290 = i$31 + 1;
        i$31 = _tmp$1290;
        continue;
      } else {
        moonbit_decref(src$28);
        moonbit_decref(dst$27);
      }
      break;
    }
  } else {
    int32_t _tmp$1295 = len$32 - 1;
    int32_t i$34 = _tmp$1295;
    while (1) {
      if (i$34 >= 0) {
        int32_t _tmp$1291 = dst_offset$29 + i$34;
        int32_t _tmp$1293 = src_offset$30 + i$34;
        struct $$3c$String$2a$String$3e$* _tmp$2694;
        struct $$3c$String$2a$String$3e$* _tmp$1292;
        struct $$3c$String$2a$String$3e$* _old$2693;
        int32_t _tmp$1294;
        if (_tmp$1293 < 0 || _tmp$1293 >= Moonbit_array_length(src$28)) {
          moonbit_panic();
        }
        _tmp$2694 = (struct $$3c$String$2a$String$3e$*)src$28[_tmp$1293];
        _tmp$1292 = _tmp$2694;
        if (_tmp$1291 < 0 || _tmp$1291 >= Moonbit_array_length(dst$27)) {
          moonbit_panic();
        }
        _old$2693 = (struct $$3c$String$2a$String$3e$*)dst$27[_tmp$1291];
        if (_tmp$1292) {
          moonbit_incref(_tmp$1292);
        }
        if (_old$2693) {
          moonbit_decref(_old$2693);
        }
        dst$27[_tmp$1291] = _tmp$1292;
        _tmp$1294 = i$34 - 1;
        i$34 = _tmp$1294;
        continue;
      } else {
        moonbit_decref(src$28);
        moonbit_decref(dst$27);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$0(
  moonbit_bytes_t dst$18,
  int32_t dst_offset$20,
  moonbit_bytes_t src$19,
  int32_t src_offset$21,
  int32_t len$23
) {
  if (dst$18 == src$19 && dst_offset$20 < src_offset$21) {
    int32_t i$22 = 0;
    while (1) {
      if (i$22 < len$23) {
        int32_t _tmp$1278 = dst_offset$20 + i$22;
        int32_t _tmp$1280 = src_offset$21 + i$22;
        int32_t _tmp$1279;
        int32_t _tmp$1281;
        if (_tmp$1280 < 0 || _tmp$1280 >= Moonbit_array_length(src$19)) {
          moonbit_panic();
        }
        _tmp$1279 = (int32_t)src$19[_tmp$1280];
        if (_tmp$1278 < 0 || _tmp$1278 >= Moonbit_array_length(dst$18)) {
          moonbit_panic();
        }
        dst$18[_tmp$1278] = _tmp$1279;
        _tmp$1281 = i$22 + 1;
        i$22 = _tmp$1281;
        continue;
      } else {
        moonbit_decref(src$19);
        moonbit_decref(dst$18);
      }
      break;
    }
  } else {
    int32_t _tmp$1286 = len$23 - 1;
    int32_t i$25 = _tmp$1286;
    while (1) {
      if (i$25 >= 0) {
        int32_t _tmp$1282 = dst_offset$20 + i$25;
        int32_t _tmp$1284 = src_offset$21 + i$25;
        int32_t _tmp$1283;
        int32_t _tmp$1285;
        if (_tmp$1284 < 0 || _tmp$1284 >= Moonbit_array_length(src$19)) {
          moonbit_panic();
        }
        _tmp$1283 = (int32_t)src$19[_tmp$1284];
        if (_tmp$1282 < 0 || _tmp$1282 >= Moonbit_array_length(dst$18)) {
          moonbit_panic();
        }
        dst$18[_tmp$1282] = _tmp$1283;
        _tmp$1285 = i$25 - 1;
        i$25 = _tmp$1285;
        continue;
      } else {
        moonbit_decref(src$19);
        moonbit_decref(dst$18);
      }
      break;
    }
  }
  return 0;
}

int64_t $moonbitlang$core$builtin$abort$4(
  moonbit_string_t string$16,
  moonbit_string_t loc$17
) {
  moonbit_string_t _tmp$1276 =
    moonbit_add_string(
      string$16, (moonbit_string_t)moonbit_string_literal_132.data
    );
  moonbit_string_t _tmp$1277 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$17);
  moonbit_string_t _tmp$1275 = moonbit_add_string(_tmp$1276, _tmp$1277);
  moonbit_string_t _tmp$1274 =
    moonbit_add_string(
      _tmp$1275, (moonbit_string_t)moonbit_string_literal_22.data
    );
  return $moonbitlang$core$abort$abort$4(_tmp$1274);
}

int32_t $moonbitlang$core$builtin$abort$3(
  moonbit_string_t string$14,
  moonbit_string_t loc$15
) {
  moonbit_string_t _tmp$1272 =
    moonbit_add_string(
      string$14, (moonbit_string_t)moonbit_string_literal_132.data
    );
  moonbit_string_t _tmp$1273 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$15);
  moonbit_string_t _tmp$1271 = moonbit_add_string(_tmp$1272, _tmp$1273);
  moonbit_string_t _tmp$1270 =
    moonbit_add_string(
      _tmp$1271, (moonbit_string_t)moonbit_string_literal_22.data
    );
  return $moonbitlang$core$abort$abort$3(_tmp$1270);
}

struct $StringView $moonbitlang$core$builtin$abort$2(
  moonbit_string_t string$12,
  moonbit_string_t loc$13
) {
  moonbit_string_t _tmp$1268 =
    moonbit_add_string(
      string$12, (moonbit_string_t)moonbit_string_literal_132.data
    );
  moonbit_string_t _tmp$1269 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$13);
  moonbit_string_t _tmp$1267 = moonbit_add_string(_tmp$1268, _tmp$1269);
  moonbit_string_t _tmp$1266 =
    moonbit_add_string(
      _tmp$1267, (moonbit_string_t)moonbit_string_literal_22.data
    );
  return $moonbitlang$core$abort$abort$2(_tmp$1266);
}

int32_t $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$10,
  moonbit_string_t loc$11
) {
  moonbit_string_t _tmp$1264 =
    moonbit_add_string(
      string$10, (moonbit_string_t)moonbit_string_literal_132.data
    );
  moonbit_string_t _tmp$1265 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$11);
  moonbit_string_t _tmp$1263 = moonbit_add_string(_tmp$1264, _tmp$1265);
  moonbit_string_t _tmp$1262 =
    moonbit_add_string(
      _tmp$1263, (moonbit_string_t)moonbit_string_literal_22.data
    );
  $moonbitlang$core$abort$abort$1(_tmp$1262);
  return 0;
}

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$8,
  moonbit_string_t loc$9
) {
  moonbit_string_t _tmp$1260 =
    moonbit_add_string(
      string$8, (moonbit_string_t)moonbit_string_literal_132.data
    );
  moonbit_string_t _tmp$1261 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$9);
  moonbit_string_t _tmp$1259 = moonbit_add_string(_tmp$1260, _tmp$1261);
  moonbit_string_t _tmp$1258 =
    moonbit_add_string(
      _tmp$1259, (moonbit_string_t)moonbit_string_literal_22.data
    );
  return $moonbitlang$core$abort$abort$0(_tmp$1258);
}

int32_t $$moonbitlang$core$builtin$Logger$$write_object$0(
  struct $$moonbitlang$core$builtin$Logger self$7,
  moonbit_string_t obj$6
) {
  $$moonbitlang$core$builtin$Show$$String$$output(obj$6, self$7);
  return 0;
}

int64_t $moonbitlang$core$abort$abort$4(moonbit_string_t msg$5) {
  moonbit_println(msg$5);
  moonbit_decref(msg$5);
  moonbit_panic();
}

int32_t $moonbitlang$core$abort$abort$3(moonbit_string_t msg$4) {
  moonbit_println(msg$4);
  moonbit_decref(msg$4);
  moonbit_panic();
}

struct $StringView $moonbitlang$core$abort$abort$2(moonbit_string_t msg$3) {
  moonbit_println(msg$3);
  moonbit_decref(msg$3);
  moonbit_panic();
}

int32_t $moonbitlang$core$abort$abort$1(moonbit_string_t msg$2) {
  moonbit_println(msg$2);
  moonbit_decref(msg$2);
  moonbit_panic();
  return 0;
}

int32_t $moonbitlang$core$abort$abort$0(moonbit_string_t msg$1) {
  moonbit_println(msg$1);
  moonbit_decref(msg$1);
  moonbit_panic();
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1248,
  int32_t _param$1247
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1246 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1248;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$1246, _param$1247
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1245,
  struct $StringView _param$1244
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1243 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1245;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    _self$1243, _param$1244
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1242,
  moonbit_string_t _param$1239,
  int32_t _param$1240,
  int32_t _param$1241
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1238 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1242;
  $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
    _self$1238, _param$1239, _param$1240, _param$1241
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1237,
  moonbit_string_t _param$1236
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1235 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1237;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    _self$1235, _param$1236
  );
  return 0;
}

void moonbit_init() {
  $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$439 = (int64_t)0;
  $moonbitlang$core$builtin$brute_force_find$constr$453 = (int64_t)0;
}

int main(int argc, char** argv) {
  int32_t _return_value$895;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* args$896;
  int32_t len$1249;
  int32_t len$1256;
  moonbit_string_t* _field$2696;
  int32_t _cnt$2775;
  moonbit_string_t* buf$1257;
  moonbit_string_t _tmp$2695;
  moonbit_string_t filename$897;
  void* _try_err$900;
  moonbit_string_t content$898;
  struct moonbit_result_0 _tmp$2953;
  moonbit_string_t _tmp$1252;
  moonbit_string_t _tmp$1251;
  int32_t _tmp$1253;
  struct $$moonbitlang$core$builtin$Array$3c$QueryDef$3e$* queries$901;
  moonbit_string_t code$902;
  moonbit_runtime_init(argc, argv);
  moonbit_init();
  args$896 = $moonbitlang$x$sys$get_cli_args();
  len$1249 = args$896->$1;
  if (len$1249 < 2) {
    int32_t _tmp$1250;
    moonbit_decref(args$896);
    $moonbitlang$core$builtin$println$0(
      (moonbit_string_t)moonbit_string_literal_133.data
    );
    $moonbitlang$core$builtin$println$0(
      (moonbit_string_t)moonbit_string_literal_111.data
    );
    $moonbitlang$core$builtin$println$0(
      (moonbit_string_t)moonbit_string_literal_134.data
    );
    $moonbitlang$core$builtin$println$0(
      (moonbit_string_t)moonbit_string_literal_135.data
    );
    $moonbitlang$core$builtin$println$0(
      (moonbit_string_t)moonbit_string_literal_136.data
    );
    $moonbitlang$core$builtin$println$0(
      (moonbit_string_t)moonbit_string_literal_137.data
    );
    $moonbitlang$core$builtin$println$0(
      (moonbit_string_t)moonbit_string_literal_138.data
    );
    _tmp$1250 = 0;
    _return_value$895 = _tmp$1250;
    goto $join$894;
  }
  len$1256 = args$896->$1;
  if (1 >= len$1256) {
    moonbit_panic();
  }
  _field$2696 = args$896->$0;
  _cnt$2775 = Moonbit_object_header(args$896)->rc;
  if (_cnt$2775 > 1) {
    int32_t _new_cnt$2776 = _cnt$2775 - 1;
    Moonbit_object_header(args$896)->rc = _new_cnt$2776;
    moonbit_incref(_field$2696);
  } else if (_cnt$2775 == 1) {
    moonbit_free(args$896);
  }
  buf$1257 = _field$2696;
  _tmp$2695 = (moonbit_string_t)buf$1257[1];
  moonbit_incref(_tmp$2695);
  moonbit_decref(buf$1257);
  filename$897 = _tmp$2695;
  _tmp$2953
  = $moonbitlang$x$fs$read_file_to_string$inner(
    filename$897, (moonbit_string_t)moonbit_string_literal_107.data
  );
  if (_tmp$2953.tag) {
    moonbit_string_t const _ok$1254 = _tmp$2953.data.ok;
    content$898 = _ok$1254;
  } else {
    void* const _err$1255 = _tmp$2953.data.err;
    _try_err$900 = _err$1255;
    goto $join$899;
  }
  goto $joinlet$2952;
  $join$899:;
  _tmp$1252
  = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
    _try_err$900
  );
  _tmp$1251
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_139.data, _tmp$1252
  );
  $moonbitlang$core$builtin$println$0(_tmp$1251);
  _tmp$1253 = 0;
  _return_value$895 = _tmp$1253;
  goto $join$894;
  $joinlet$2952:;
  queries$901 = $sqlc_gen_moonbit$tools$codegen$parse_queries(content$898);
  code$902 = $sqlc_gen_moonbit$tools$codegen$generate_code(queries$901);
  $moonbitlang$core$builtin$println$0(code$902);
  goto $joinlet$2951;
  $join$894:;
  $joinlet$2951:;
  return 0;
}