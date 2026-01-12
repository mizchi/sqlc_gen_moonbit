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
struct $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger;

struct $$moonbitlang$x$encoding$UTF16Decode$UTF16Malformed;

struct $String$$iter$$2a$p$fn$1$2d$cap;

struct $$moonbitlang$x$encoding$Decoder;

struct $$moonbitlang$x$encoding$UTF16Decode$Hi;

struct $$moonbitlang$x$encoding$UTF16Decode$UTF16Uchar;

struct $Ref$3c$Int$3e$;

struct $$moonbitlang$x$encoding$Decode$Refill;

struct $StringView;

struct $$moonbitlang$core$builtin$ArrayView$3c$Char$3e$;

struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode;

struct $$3c$$moonbitlang$core$buffer$Buffer$2a$Char$3e$$3d$$3e$Unit;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Err;

struct $Result$3c$Byte$2a$$moonbitlang$core$builtin$NoError$3e$$Err;

struct $$moonbitlang$core$buffer$Buffer;

struct $$3c$Int$3e$$3d$$3e$Byte;

struct $$moonbitlang$x$encoding$Decode$Uchar;

struct $$example$gen$CreateUserParams;

struct $$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$4$2d$cap;

struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$;

struct $$example$gen$ListUsersRow;

struct $$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$6$2d$cap;

struct $$moonbitlang$core$builtin$Array$3c$Char$3e$;

struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$;

struct $Bytes$$from_array$fn$2$2d$cap;

struct $$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$5$2d$cap;

struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode;

struct $$example$gen$GetUserRow;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Ok;

struct $$3c$Int$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode;

struct $BytesView;

struct $$moonbitlang$core$builtin$SourceLocRepr;

struct $$moonbitlang$core$builtin$Logger;

struct $$moonbitlang$x$encoding$Decode$Malformed;

struct $Result$3c$Bytes$2a$$moonbitlang$core$builtin$NoError$3e$$Ok;

struct $Result$3c$Bytes$2a$$moonbitlang$core$builtin$NoError$3e$$Err;

struct $$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$3$2d$cap;

struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$;

struct $Option$3c$StringView$3e$$Some;

struct $Result$3c$Byte$2a$$moonbitlang$core$builtin$NoError$3e$$Ok;

struct $$example$gen$GetUserParams;

struct $$moonbitlang$core$builtin$Logger$static_method_table;

struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode;

struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode;

struct $$moonbitlang$core$builtin$StringBuilder;

struct $$moonbitlang$core$builtin$Array$3c$$example$gen$ListUsersRow$3e$;

struct $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger {
  struct $$moonbitlang$core$builtin$Logger$static_method_table* $0;
  void* $1;
  
};

struct $$moonbitlang$x$encoding$UTF16Decode$UTF16Malformed {
  moonbit_bytes_t $0;
  
};

struct $String$$iter$$2a$p$fn$1$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$*);
  int32_t $2;
  struct $Ref$3c$Int$3e$* $0;
  moonbit_string_t $1;
  
};

struct $$moonbitlang$x$encoding$Decoder {
  int32_t $1;
  int32_t $3;
  int32_t $4;
  moonbit_bytes_t $0;
  moonbit_bytes_t $2;
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $5;
  
};

struct $$moonbitlang$x$encoding$UTF16Decode$Hi {
  int32_t $0;
  
};

struct $$moonbitlang$x$encoding$UTF16Decode$UTF16Uchar {
  int32_t $0;
  
};

struct $Ref$3c$Int$3e$ {
  int32_t $0;
  
};

struct $$moonbitlang$x$encoding$Decode$Refill {
  moonbit_bytes_t $0;
  
};

struct $StringView {
  int32_t $1;
  int32_t $2;
  moonbit_string_t $0;
  
};

struct $$moonbitlang$core$builtin$ArrayView$3c$Char$3e$ {
  int32_t $1;
  int32_t $2;
  int32_t* $0;
  
};

struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode {
  void*(* code)(
    struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*,
    struct $$moonbitlang$x$encoding$Decoder*
  );
  
};

struct $$3c$$moonbitlang$core$buffer$Buffer$2a$Char$3e$$3d$$3e$Unit {
  int32_t(* code)(
    struct $$3c$$moonbitlang$core$buffer$Buffer$2a$Char$3e$$3d$$3e$Unit*,
    struct $$moonbitlang$core$buffer$Buffer*,
    int32_t
  );
  
};

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Err {
  void* $0;
  
};

struct $Result$3c$Byte$2a$$moonbitlang$core$builtin$NoError$3e$$Err {
  int32_t $0;
  
};

struct $$moonbitlang$core$buffer$Buffer {
  int32_t $1;
  moonbit_bytes_t $0;
  
};

struct $$3c$Int$3e$$3d$$3e$Byte {
  int32_t(* code)(struct $$3c$Int$3e$$3d$$3e$Byte*, int32_t);
  
};

struct $$moonbitlang$x$encoding$Decode$Uchar {
  int32_t $0;
  
};

struct $$example$gen$CreateUserParams {
  moonbit_string_t $0;
  moonbit_string_t $1;
  
};

struct $$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$4$2d$cap {
  void*(* code)(
    struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*,
    struct $$moonbitlang$x$encoding$Decoder*
  );
  struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $0;
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $1;
  
};

struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ {
  int32_t $1;
  int32_t $2;
  moonbit_bytes_t $0;
  
};

struct $$example$gen$ListUsersRow {
  int64_t $0;
  moonbit_string_t $1;
  moonbit_string_t $2;
  
};

struct $$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$6$2d$cap {
  void*(* code)(
    struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*,
    struct $$moonbitlang$x$encoding$Decoder*
  );
  int32_t $1;
  struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $0;
  
};

struct $$moonbitlang$core$builtin$Array$3c$Char$3e$ {
  int32_t $1;
  int32_t* $0;
  
};

struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$ {
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t $0_0;
  void* $1;
  
};

struct $Bytes$$from_array$fn$2$2d$cap {
  int32_t(* code)(struct $$3c$Int$3e$$3d$$3e$Byte*, int32_t);
  int32_t $0_1;
  int32_t $0_2;
  moonbit_bytes_t $0_0;
  
};

struct $$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$5$2d$cap {
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*(* code)(
    struct $$3c$Int$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*,
    int32_t
  );
  struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $0;
  
};

struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode {
  void*(* code)(
    struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*,
    struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*,
    struct $$moonbitlang$x$encoding$Decoder*
  );
  
};

struct $$example$gen$GetUserRow {
  int64_t $0;
  moonbit_string_t $1;
  moonbit_string_t $2;
  
};

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Ok {
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t $0_0;
  
};

struct $$3c$Int$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode {
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*(* code)(
    struct $$3c$Int$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*,
    int32_t
  );
  
};

struct $BytesView {
  int32_t $1;
  int32_t $2;
  moonbit_bytes_t $0;
  
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

struct $$moonbitlang$core$builtin$Logger {
  struct $$moonbitlang$core$builtin$Logger$static_method_table* $0;
  void* $1;
  
};

struct $$moonbitlang$x$encoding$Decode$Malformed {
  moonbit_bytes_t $0;
  
};

struct $Result$3c$Bytes$2a$$moonbitlang$core$builtin$NoError$3e$$Ok {
  moonbit_bytes_t $0;
  
};

struct $Result$3c$Bytes$2a$$moonbitlang$core$builtin$NoError$3e$$Err {
  int32_t $0;
  
};

struct $$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$3$2d$cap {
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*(* code)(
    struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*,
    struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*
  );
  struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $0;
  
};

struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$ {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$*);
  
};

struct $Option$3c$StringView$3e$$Some {
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t $0_0;
  
};

struct $Result$3c$Byte$2a$$moonbitlang$core$builtin$NoError$3e$$Ok {
  int32_t $0;
  
};

struct $$example$gen$GetUserParams {
  int64_t $0;
  
};

struct $$moonbitlang$core$builtin$Logger$static_method_table {
  int32_t(* $method_0)(void*, moonbit_string_t);
  int32_t(* $method_1)(void*, moonbit_string_t, int32_t, int32_t);
  int32_t(* $method_2)(void*, struct $StringView);
  int32_t(* $method_3)(void*, int32_t);
  
};

struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode {
  void*(* code)(
    struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*,
    int32_t,
    struct $$moonbitlang$x$encoding$Decoder*
  );
  
};

struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode {
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*(* code)(
    struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*,
    struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*
  );
  
};

struct $$moonbitlang$core$builtin$StringBuilder {
  int32_t $1;
  moonbit_bytes_t $0;
  
};

struct $$moonbitlang$core$builtin$Array$3c$$example$gen$ListUsersRow$3e$ {
  int32_t $1;
  struct $$example$gen$ListUsersRow** $0;
  
};

struct moonbit_result_0 {
  int tag;
  union { struct $StringView ok; void* err;  } data;
  
};

int32_t $moonbitlang$x$encoding$write_utf16be_char$dyncall(
  struct $$3c$$moonbitlang$core$buffer$Buffer$2a$Char$3e$$3d$$3e$Unit* _env$1965,
  struct $$moonbitlang$core$buffer$Buffer* buf$548,
  int32_t value$545
);

void* $$moonbitlang$x$encoding$Decoder$$t_decode_utf_8$dyncall(
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1964,
  struct $$moonbitlang$x$encoding$Decoder* self$675
);

void* $moonbitlang$x$encoding$t_decode_utf_16be_lo$dyncall(
  struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1963,
  int32_t hi$634,
  struct $$moonbitlang$x$encoding$Decoder* self$633
);

void* $$moonbitlang$x$encoding$Decoder$$decode_utf_8$dyncall(
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1962,
  struct $$moonbitlang$x$encoding$Decoder* self$671
);

void* $$moonbitlang$x$encoding$Decoder$$decode_utf_16be$dyncall(
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1961,
  struct $$moonbitlang$x$encoding$Decoder* self$619
);

void* $moonbitlang$x$encoding$t_fill$dyncall(
  struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1960,
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* k$605,
  struct $$moonbitlang$x$encoding$Decoder* decoder$603
);

void* $$moonbitlang$x$encoding$Decoder$$t_decode_utf_16le$dyncall(
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1959,
  struct $$moonbitlang$x$encoding$Decoder* self$653
);

void* $$moonbitlang$x$encoding$Decoder$$t_decode_utf_16be$dyncall(
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1958,
  struct $$moonbitlang$x$encoding$Decoder* self$635
);

int32_t $moonbitlang$x$encoding$write_utf8_char$dyncall(
  struct $$3c$$moonbitlang$core$buffer$Buffer$2a$Char$3e$$3d$$3e$Unit* _env$1957,
  struct $$moonbitlang$core$buffer$Buffer* buf$559,
  int32_t value$557
);

void* $$moonbitlang$x$encoding$Decoder$$decode_utf_16le$dyncall(
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1956,
  struct $$moonbitlang$x$encoding$Decoder* self$637
);

void* $moonbitlang$x$encoding$t_decode_utf_16le_lo$dyncall(
  struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1955,
  int32_t hi$652,
  struct $$moonbitlang$x$encoding$Decoder* decoder$651
);

moonbit_bytes_t $example$src$main$cstring(moonbit_string_t s$713);

int32_t $example$gen$create_user(
  void* db$711,
  struct $$example$gen$CreateUserParams* params$712
);

struct $$moonbitlang$core$builtin$Array$3c$$example$gen$ListUsersRow$3e$* $example$gen$list_users(
  void* db$706
);

struct $$example$gen$GetUserRow* $example$gen$get_user(
  void* db$702,
  struct $$example$gen$GetUserParams* params$703
);

struct $$example$gen$CreateUserParams* $$example$gen$CreateUserParams$$new(
  moonbit_string_t name$699,
  moonbit_string_t email$700
);

struct $$example$gen$GetUserParams* $$example$gen$GetUserParams$$new(
  int64_t id$698
);

moonbit_string_t $example$gen$bytes_to_string(moonbit_bytes_t b$697);

moonbit_bytes_t $example$gen$cstring(moonbit_string_t s$695);

#define $mizchi$sqlite$sqlite_column_int64 sqlite_column_int64

#define $mizchi$sqlite$sqlite_open_v2 sqlite_open_v2

#define $mizchi$sqlite$sqlite_column_text sqlite_column_text

#define $mizchi$sqlite$sqlite_finalize sqlite_finalize

#define $mizchi$sqlite$sqlite_step sqlite_step

#define $mizchi$sqlite$sqlite_bind_text sqlite_bind_text

#define $mizchi$sqlite$sqlite_bind_int sqlite_bind_int

#define $mizchi$sqlite$sqlite_prepare sqlite_prepare

moonbit_string_t $$moonbitlang$x$encoding$Decoder$$decode_lossy$inner(
  struct $$moonbitlang$x$encoding$Decoder* self$688,
  struct $BytesView input$687,
  int32_t stream$694
);

int32_t $$moonbitlang$x$encoding$Decoder$$i_cont(
  struct $$moonbitlang$x$encoding$Decoder* self$683,
  struct $BytesView input$685
);

void* $$moonbitlang$x$encoding$Decoder$$decode_(
  struct $$moonbitlang$x$encoding$Decoder* self$681
);

struct $$moonbitlang$x$encoding$Decoder* $moonbitlang$x$encoding$decoder(
  int32_t encoding$679
);

void* $$moonbitlang$x$encoding$Decoder$$t_decode_utf_8(
  struct $$moonbitlang$x$encoding$Decoder* self$675
);

void* $$moonbitlang$x$encoding$Decoder$$decode_utf_8(
  struct $$moonbitlang$x$encoding$Decoder* self$671
);

void* $moonbitlang$x$encoding$r_utf_8(
  moonbit_bytes_t bytes$657,
  int32_t offset$658,
  int32_t length$656
);

void* $$moonbitlang$x$encoding$Decoder$$t_decode_utf_16le(
  struct $$moonbitlang$x$encoding$Decoder* self$653
);

void* $moonbitlang$x$encoding$t_decode_utf_16le_lo(
  int32_t hi$652,
  struct $$moonbitlang$x$encoding$Decoder* decoder$651
);

void* $$moonbitlang$x$encoding$Decoder$$decode_utf_16le_lo(
  struct $$moonbitlang$x$encoding$Decoder* self$642,
  void* v$639
);

void* $$moonbitlang$x$encoding$Decoder$$decode_utf_16le(
  struct $$moonbitlang$x$encoding$Decoder* self$637
);

void* $$moonbitlang$x$encoding$Decoder$$t_decode_utf_16be(
  struct $$moonbitlang$x$encoding$Decoder* self$635
);

void* $moonbitlang$x$encoding$t_decode_utf_16be_lo(
  int32_t hi$634,
  struct $$moonbitlang$x$encoding$Decoder* self$633
);

void* $$moonbitlang$x$encoding$Decoder$$decode_utf_16be_lo(
  struct $$moonbitlang$x$encoding$Decoder* self$624,
  void* decode$621
);

void* $$moonbitlang$x$encoding$Decoder$$decode_utf_16be(
  struct $$moonbitlang$x$encoding$Decoder* self$619
);

int32_t $$moonbitlang$x$encoding$Decoder$$t_need(
  struct $$moonbitlang$x$encoding$Decoder* self$616,
  int32_t need$617
);

void* $moonbitlang$x$encoding$r_utf_16_lo(
  int32_t hi$615,
  moonbit_bytes_t bytes$610,
  int32_t offset0$611,
  int32_t offset1$613
);

void* $moonbitlang$x$encoding$t_fill(
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* k$605,
  struct $$moonbitlang$x$encoding$Decoder* decoder$603
);

int32_t $moonbitlang$x$encoding$t_fill$blit$7c$69(
  struct $$moonbitlang$x$encoding$Decoder* decoder$600,
  int32_t l$601
);

void* $$moonbitlang$x$encoding$Decoder$$refill(
  struct $$moonbitlang$x$encoding$Decoder* self$597,
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* k$598
);

void* $$moonbitlang$x$encoding$Decoder$$ret(
  struct $$moonbitlang$x$encoding$Decoder* self$594,
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* k$595,
  void* v$596
);

int32_t $$moonbitlang$x$encoding$Decoder$$eoi(
  struct $$moonbitlang$x$encoding$Decoder* self$593
);

int32_t $$moonbitlang$x$encoding$Decoder$$i_rem(
  struct $$moonbitlang$x$encoding$Decoder* self$592
);

struct $$3c$Int$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $moonbitlang$x$encoding$curry$1(
  struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* f$591
);

struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$5(
  struct $$3c$Int$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1727,
  int32_t x$589
);

void* $$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$6(
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1729,
  struct $$moonbitlang$x$encoding$Decoder* y$590
);

struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $moonbitlang$x$encoding$curry$0(
  struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* f$588
);

struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$3(
  struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1723,
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* x$586
);

void* $$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$4(
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1725,
  struct $$moonbitlang$x$encoding$Decoder* y$587
);

void* $moonbitlang$x$encoding$r_utf_16(
  moonbit_bytes_t bytes$581,
  int32_t offset0$582,
  int32_t offset1$584
);

moonbit_bytes_t $moonbitlang$x$encoding$encode(
  int32_t encoding$572,
  moonbit_string_t src$571
);

int32_t $moonbitlang$x$encoding$write_utf8_char(
  struct $$moonbitlang$core$buffer$Buffer* buf$559,
  int32_t value$557
);

int32_t $moonbitlang$x$encoding$write_utf16be_char(
  struct $$moonbitlang$core$buffer$Buffer* buf$548,
  int32_t value$545
);

void* $moonbitlang$x$encoding$malformed_pair(
  int32_t be$543,
  int32_t hi$541,
  moonbit_bytes_t bytes$535,
  int32_t offset$536,
  int32_t length$534
);

void* $moonbitlang$x$encoding$malformed(
  moonbit_bytes_t bytes$529,
  int32_t offset$530,
  int32_t length$531
);

moonbit_bytes_t $moonbitlang$x$encoding$slice(
  moonbit_bytes_t bytes$527,
  int32_t offset$528,
  int32_t length$526
);

int32_t $moonbitlang$core$cmp$minimum$0(int32_t x$523, int32_t y$524);

int32_t $moonbitlang$core$cmp$maximum$0(int32_t x$521, int32_t y$522);

moonbit_bytes_t $$moonbitlang$core$buffer$Buffer$$to_bytes(
  struct $$moonbitlang$core$buffer$Buffer* self$520
);

int32_t $$moonbitlang$core$buffer$Buffer$$write_bytes(
  struct $$moonbitlang$core$buffer$Buffer* self$519,
  moonbit_bytes_t value$518
);

struct $$moonbitlang$core$buffer$Buffer* $moonbitlang$core$buffer$new$inner(
  int32_t size_hint$515
);

int32_t $$moonbitlang$core$buffer$Buffer$$length(
  struct $$moonbitlang$core$buffer$Buffer* self$513
);

int32_t $$moonbitlang$core$buffer$Buffer$$write_byte(
  struct $$moonbitlang$core$buffer$Buffer* self$511,
  int32_t value$512
);

int32_t $$moonbitlang$core$buffer$Buffer$$grow_if_necessary(
  struct $$moonbitlang$core$buffer$Buffer* self$505,
  int32_t required$508
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$502,
  struct $$moonbitlang$core$builtin$Logger logger$503
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$465,
  struct $$moonbitlang$core$builtin$Logger logger$501
);

moonbit_bytes_t $Bytes$$to_fixedarray(
  moonbit_bytes_t self$460,
  int64_t len$459
);

moonbit_bytes_t $Bytes$$from_array(
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ arr$456
);

int32_t $Bytes$$from_array$fn$2(
  struct $$3c$Int$3e$$3d$$3e$Byte* _env$1582,
  int32_t i$457
);

int32_t $FixedArray$$blit_from_bytesview(
  moonbit_bytes_t self$453,
  int32_t bytes_offset$454,
  struct $BytesView src$455
);

int32_t $FixedArray$$blit_from_bytes(
  moonbit_bytes_t self$450,
  int32_t bytes_offset$445,
  moonbit_bytes_t src$452,
  int32_t src_offset$448,
  int32_t length$446
);

struct $BytesView $Bytes$$sub$inner(
  moonbit_bytes_t self$437,
  int32_t start$443,
  int64_t end$439
);

int32_t $BytesView$$length(struct $BytesView self$435);

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$434);

moonbit_bytes_t $Bytes$$makei$0(
  int32_t length$429,
  struct $$3c$Int$3e$$3d$$3e$Byte* value$431
);

int32_t $FixedArray$$blit_to$inner$0(
  moonbit_bytes_t self$428,
  moonbit_bytes_t dst$427,
  int32_t len$426,
  int32_t src_offset$425,
  int32_t dst_offset$424
);

moonbit_bytes_t $Bytes$$new(int32_t len$423);

moonbit_bytes_t $Bytes$$make(int32_t len$421, int32_t init$422);

moonbit_bytes_t $$moonbitlang$core$builtin$Default$$FixedArray$$default$0();

struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ $FixedArray$$sub$inner$0(
  moonbit_bytes_t self$414,
  int32_t start$420,
  int64_t end$416
);

int32_t $$moonbitlang$core$builtin$ArrayView$$at$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ self$412,
  int32_t index$411
);

int32_t $ReadOnlyArray$$at$0(int32_t* self$409, int32_t index$410);

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$408
);

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$407,
  struct $$moonbitlang$core$builtin$Logger logger$406
);

moonbit_string_t $$moonbitlang$core$builtin$Default$$String$$default();

struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* $String$$iter(
  moonbit_string_t self$401
);

int32_t $String$$iter$$2a$p$fn$1(
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* _env$1517
);

int32_t $$moonbitlang$core$builtin$Iter$$next$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* self$399
);

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$395,
  int32_t value$397
);

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$$example$gen$ListUsersRow$3e$* self$392,
  struct $$example$gen$ListUsersRow* value$394
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$390
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$$example$gen$ListUsersRow$3e$* self$387
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$383,
  int32_t new_capacity$381
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$example$gen$ListUsersRow$3e$* self$377,
  int32_t new_capacity$375
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$372,
  struct $StringView str$373
);

moonbit_string_t $String$$from_array(
  struct $$moonbitlang$core$builtin$ArrayView$3c$Char$3e$ chars$367
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$Char$3e$ self$365
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ self$364
);

struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* $$moonbitlang$core$builtin$Iter$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* f$363
);

moonbit_string_t $Int64$$to_string$inner(int64_t self$347, int32_t radix$346);

int32_t $moonbitlang$core$builtin$int64_to_string_dec(
  uint16_t* buffer$336,
  uint64_t num$324,
  int32_t digit_start$327,
  int32_t total_len$326
);

int32_t $moonbitlang$core$builtin$int64_to_string_generic(
  uint16_t* buffer$318,
  uint64_t num$312,
  int32_t digit_start$310,
  int32_t total_len$309,
  int32_t radix$314
);

int32_t $moonbitlang$core$builtin$int64_to_string_hex(
  uint16_t* buffer$305,
  uint64_t num$301,
  int32_t digit_start$299,
  int32_t total_len$298
);

int32_t $moonbitlang$core$builtin$radix_count64(
  uint64_t value$291,
  int32_t radix$294
);

int32_t $moonbitlang$core$builtin$hex_count64(uint64_t value$289);

int32_t $moonbitlang$core$builtin$dec_count64(uint64_t value$288);

moonbit_string_t $Int$$to_string$inner(int32_t self$272, int32_t radix$271);

int32_t $moonbitlang$core$builtin$radix_count32(
  uint32_t value$265,
  int32_t radix$268
);

int32_t $moonbitlang$core$builtin$hex_count32(uint32_t value$263);

int32_t $moonbitlang$core$builtin$dec_count32(uint32_t value$262);

int32_t $moonbitlang$core$builtin$int_to_string_dec(
  uint16_t* buffer$252,
  uint32_t num$240,
  int32_t digit_start$243,
  int32_t total_len$242
);

int32_t $moonbitlang$core$builtin$int_to_string_generic(
  uint16_t* buffer$234,
  uint32_t num$228,
  int32_t digit_start$226,
  int32_t total_len$225,
  int32_t radix$230
);

int32_t $moonbitlang$core$builtin$int_to_string_hex(
  uint16_t* buffer$221,
  uint32_t num$217,
  int32_t digit_start$215,
  int32_t total_len$214
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  int32_t self$212
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  moonbit_string_t self$210
);

int32_t $StringView$$start_offset(struct $StringView self$208);

int32_t $StringView$$length(struct $StringView self$207);

moonbit_string_t $StringView$$data(struct $StringView self$206);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$200,
  moonbit_string_t value$203,
  int32_t start$204,
  int32_t len$205
);

struct moonbit_result_0 $String$$sub(
  moonbit_string_t self$198,
  int64_t start$opt$196,
  int64_t end$199
);

struct moonbit_result_0 $String$$sub$inner(
  moonbit_string_t self$188,
  int32_t start$194,
  int64_t end$190
);

uint64_t $Int$$to_uint64(int32_t self$186);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$184,
  moonbit_string_t str$185
);

int32_t $FixedArray$$blit_from_string(
  moonbit_bytes_t self$176,
  int32_t bytes_offset$171,
  moonbit_string_t str$178,
  int32_t str_offset$174,
  int32_t length$172
);

struct $$moonbitlang$core$builtin$SourceLocRepr* $$moonbitlang$core$builtin$SourceLocRepr$$parse(
  moonbit_string_t repr$93
);

int32_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$91
);

struct $$example$gen$ListUsersRow** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$example$gen$ListUsersRow$3e$* self$90
);

int32_t $moonbitlang$core$builtin$code_point_of_surrogate_pair(
  int32_t leading$88,
  int32_t trailing$89
);

int32_t $String$$unsafe_charcode_at(moonbit_string_t self$86, int32_t idx$87);

int32_t $Int$$is_trailing_surrogate(int32_t self$85);

int32_t $Int$$is_leading_surrogate(int32_t self$84);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
  struct $$moonbitlang$core$builtin$StringBuilder* self$81,
  int32_t ch$83
);

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$76,
  int32_t required$77
);

int32_t $$moonbitlang$core$builtin$Default$$Byte$$default();

int32_t $FixedArray$$set_utf16le_char(
  moonbit_bytes_t self$70,
  int32_t offset$71,
  int32_t value$69
);

int32_t $UInt$$to_byte(uint32_t self$67);

uint32_t $Char$$to_uint(int32_t self$66);

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$65
);

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$60,
  int32_t offset$64,
  int64_t length$62
);

#define $moonbitlang$core$builtin$unsafe_sub_string moonbit_unsafe_bytes_sub_string

struct $$moonbitlang$core$builtin$StringBuilder* $$moonbitlang$core$builtin$StringBuilder$$new$inner(
  int32_t size_hint$57
);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
  int32_t* dst$51,
  int32_t dst_offset$52,
  int32_t* src$53,
  int32_t src_offset$54,
  int32_t len$55
);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
  struct $$example$gen$ListUsersRow** dst$46,
  int32_t dst_offset$47,
  struct $$example$gen$ListUsersRow** src$48,
  int32_t src_offset$49,
  int32_t len$50
);

int32_t $FixedArray$$unsafe_blit$2(
  int32_t* dst$37,
  int32_t dst_offset$39,
  int32_t* src$38,
  int32_t src_offset$40,
  int32_t len$42
);

int32_t $FixedArray$$unsafe_blit$1(
  struct $$example$gen$ListUsersRow** dst$28,
  int32_t dst_offset$30,
  struct $$example$gen$ListUsersRow** src$29,
  int32_t src_offset$31,
  int32_t len$33
);

int32_t $FixedArray$$unsafe_blit$0(
  moonbit_bytes_t dst$19,
  int32_t dst_offset$21,
  moonbit_bytes_t src$20,
  int32_t src_offset$22,
  int32_t len$24
);

int32_t $moonbitlang$core$builtin$abort$5(
  moonbit_string_t string$17,
  moonbit_string_t loc$18
);

int32_t $moonbitlang$core$builtin$abort$4(
  moonbit_string_t string$15,
  moonbit_string_t loc$16
);

struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ $moonbitlang$core$builtin$abort$3(
  moonbit_string_t string$13,
  moonbit_string_t loc$14
);

struct $BytesView $moonbitlang$core$builtin$abort$2(
  moonbit_string_t string$11,
  moonbit_string_t loc$12
);

struct $$3c$$moonbitlang$core$buffer$Buffer$2a$Char$3e$$3d$$3e$Unit* $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$9,
  moonbit_string_t loc$10
);

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$7,
  moonbit_string_t loc$8
);

int32_t $moonbitlang$core$abort$abort$5(moonbit_string_t msg$6);

int32_t $moonbitlang$core$abort$abort$4(moonbit_string_t msg$5);

struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ $moonbitlang$core$abort$abort$3(
  moonbit_string_t msg$4
);

struct $BytesView $moonbitlang$core$abort$abort$2(moonbit_string_t msg$3);

struct $$3c$$moonbitlang$core$buffer$Buffer$2a$Char$3e$$3d$$3e$Unit* $moonbitlang$core$abort$abort$1(
  moonbit_string_t msg$2
);

int32_t $moonbitlang$core$abort$abort$0(moonbit_string_t msg$1);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1028,
  int32_t _param$1027
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1025,
  struct $StringView _param$1024
);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1022,
  moonbit_string_t _param$1019,
  int32_t _param$1020,
  int32_t _param$1021
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1017,
  moonbit_string_t _param$1016
);

int32_t sqlite_bind_int(void* $0, int32_t $1, int32_t $2);

int32_t sqlite_bind_text(void* $0, int32_t $1, moonbit_bytes_t $2);

int64_t sqlite_column_int64(void* $0, int32_t $1);

int32_t sqlite_step(void* $0);

void* sqlite_prepare(void* $0, moonbit_bytes_t $1);

int32_t sqlite_finalize(void* $0);

void* sqlite_open_v2(moonbit_bytes_t $0, int32_t $1, moonbit_bytes_t $2);

moonbit_bytes_t sqlite_column_text(void* $0, int32_t $1);

struct { int32_t rc; uint32_t meta; uint16_t const data[14]; 
} const moonbit_string_literal_39 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 13), 
    85, 115, 101, 114, 115, 32, 99, 111, 117, 110, 116, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[52]; 
} const moonbit_string_literal_22 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 51), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 97, 114, 
    114, 97, 121, 118, 105, 101, 119, 46, 109, 98, 116, 58, 49, 50, 52, 
    58, 53, 45, 49, 50, 54, 58, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[25]; 
} const moonbit_string_literal_18 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 24), 
    86, 105, 101, 119, 32, 105, 110, 100, 101, 120, 32, 111, 117, 116, 
    32, 111, 102, 32, 98, 111, 117, 110, 100, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[1]; 
} const moonbit_string_literal_4 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 0), 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[49]; 
} const moonbit_string_literal_6 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 48), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 120, 
    47, 101, 110, 99, 111, 100, 105, 110, 103, 58, 101, 110, 99, 111, 
    100, 105, 110, 103, 46, 109, 98, 116, 58, 52, 56, 58, 49, 48, 45, 
    52, 56, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_31 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    58, 109, 101, 109, 111, 114, 121, 58, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_11 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 98, 121, 
    116, 101, 115, 118, 105, 101, 119, 46, 109, 98, 116, 58, 49, 54, 
    50, 58, 53, 45, 49, 54, 50, 58, 51, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_40 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    32, 32, 105, 100, 61, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_29 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    10, 32, 32, 97, 116, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_24 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 116, 111, 
    95, 115, 116, 114, 105, 110, 103, 46, 109, 98, 116, 58, 54, 48, 52, 
    58, 53, 45, 54, 48, 52, 58, 52, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[31]; 
} const moonbit_string_literal_23 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 30), 
    114, 97, 100, 105, 120, 32, 109, 117, 115, 116, 32, 98, 101, 32, 
    98, 101, 116, 119, 101, 101, 110, 32, 50, 32, 97, 110, 100, 32, 51, 
    54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_10 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    73, 110, 118, 97, 108, 105, 100, 32, 105, 110, 100, 101, 120, 32, 
    102, 111, 114, 32, 86, 105, 101, 119, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_45 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    68, 111, 110, 101, 33, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[59]; 
} const moonbit_string_literal_17 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 58), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 102, 105, 
    120, 101, 100, 97, 114, 114, 97, 121, 95, 98, 108, 111, 99, 107, 
    46, 109, 98, 116, 58, 49, 49, 53, 58, 53, 45, 49, 49, 55, 58, 54, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_25 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 48, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[48]; 
} const moonbit_string_literal_1 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 47), 
    83, 69, 76, 69, 67, 84, 32, 105, 100, 44, 32, 110, 97, 109, 101, 
    44, 32, 101, 109, 97, 105, 108, 32, 70, 82, 79, 77, 32, 117, 115, 
    101, 114, 115, 32, 79, 82, 68, 69, 82, 32, 66, 89, 32, 110, 97, 109, 
    101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_14 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    44, 32, 108, 101, 110, 32, 61, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[18]; 
} const moonbit_string_literal_7 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 17), 
    67, 104, 97, 114, 32, 111, 117, 116, 32, 111, 102, 32, 114, 97, 110, 
    103, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[20]; 
} const moonbit_string_literal_35 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 19), 
    67, 114, 101, 97, 116, 101, 100, 32, 117, 115, 101, 114, 58, 32, 
    65, 108, 105, 99, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[15]; 
} const moonbit_string_literal_43 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 14), 
    85, 115, 101, 114, 32, 110, 111, 116, 32, 102, 111, 117, 110, 100, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_41 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    32, 110, 97, 109, 101, 61, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[85]; 
} const moonbit_string_literal_3 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 84), 
    67, 82, 69, 65, 84, 69, 32, 84, 65, 66, 76, 69, 32, 117, 115, 101, 
    114, 115, 32, 40, 105, 100, 32, 73, 78, 84, 69, 71, 69, 82, 32, 80, 
    82, 73, 77, 65, 82, 89, 32, 75, 69, 89, 44, 32, 110, 97, 109, 101, 
    32, 84, 69, 88, 84, 32, 78, 79, 84, 32, 78, 85, 76, 76, 44, 32, 101, 
    109, 97, 105, 108, 32, 84, 69, 88, 84, 32, 78, 79, 84, 32, 78, 85, 
    76, 76, 41, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_42 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    32, 101, 109, 97, 105, 108, 61, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[35]; 
} const moonbit_string_literal_12 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 34), 
    98, 111, 117, 110, 100, 115, 32, 99, 104, 101, 99, 107, 32, 102, 
    97, 105, 108, 101, 100, 58, 32, 100, 115, 116, 95, 111, 102, 102, 
    115, 101, 116, 32, 61, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_44 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    70, 111, 117, 110, 100, 32, 117, 115, 101, 114, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[49]; 
} const moonbit_string_literal_28 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 48), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 98, 121, 
    116, 101, 115, 46, 109, 98, 116, 58, 50, 57, 56, 58, 53, 45, 50, 
    57, 56, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_27 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 116, 111, 
    95, 115, 116, 114, 105, 110, 103, 46, 109, 98, 116, 58, 50, 51, 57, 
    58, 53, 45, 50, 51, 57, 58, 52, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[37]; 
} const moonbit_string_literal_26 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 36), 
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 97, 98, 99, 100, 101, 102, 
    103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 
    116, 117, 118, 119, 120, 121, 122, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[50]; 
} const moonbit_string_literal_9 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 49), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 120, 
    47, 101, 110, 99, 111, 100, 105, 110, 103, 58, 101, 110, 99, 111, 
    100, 105, 110, 103, 46, 109, 98, 116, 58, 50, 50, 57, 58, 53, 45, 
    50, 50, 57, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[43]; 
} const moonbit_string_literal_20 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 42), 
    105, 110, 100, 101, 120, 32, 111, 117, 116, 32, 111, 102, 32, 98, 
    111, 117, 110, 100, 115, 58, 32, 116, 104, 101, 32, 108, 101, 110, 
    32, 105, 115, 32, 102, 114, 111, 109, 32, 48, 32, 116, 111, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_16 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    44, 32, 115, 101, 108, 102, 46, 108, 101, 110, 103, 116, 104, 32, 
    61, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_5 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    117, 110, 114, 101, 97, 99, 104, 97, 98, 108, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[16]; 
} const moonbit_string_literal_37 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 15), 
    98, 111, 98, 64, 101, 120, 97, 109, 112, 108, 101, 46, 99, 111, 109, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_30 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 10, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[51]; 
} const moonbit_string_literal_8 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 50), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 120, 
    47, 101, 110, 99, 111, 100, 105, 110, 103, 58, 101, 110, 99, 111, 
    100, 105, 110, 103, 46, 109, 98, 116, 58, 49, 55, 48, 58, 49, 48, 
    45, 49, 55, 48, 58, 51, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[18]; 
} const moonbit_string_literal_38 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 17), 
    67, 114, 101, 97, 116, 101, 100, 32, 117, 115, 101, 114, 58, 32, 
    66, 111, 98, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_36 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    66, 111, 98, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[46]; 
} const moonbit_string_literal_2 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 45), 
    73, 78, 83, 69, 82, 84, 32, 73, 78, 84, 79, 32, 117, 115, 101, 114, 
    115, 32, 40, 110, 97, 109, 101, 44, 32, 101, 109, 97, 105, 108, 41, 
    32, 86, 65, 76, 85, 69, 83, 32, 40, 63, 44, 32, 63, 41, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[16]; 
} const moonbit_string_literal_13 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 15), 
    44, 32, 115, 114, 99, 95, 111, 102, 102, 115, 101, 116, 32, 61, 32, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[47]; 
} const moonbit_string_literal_0 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 46), 
    83, 69, 76, 69, 67, 84, 32, 105, 100, 44, 32, 110, 97, 109, 101, 
    44, 32, 101, 109, 97, 105, 108, 32, 70, 82, 79, 77, 32, 117, 115, 
    101, 114, 115, 32, 87, 72, 69, 82, 69, 32, 105, 100, 32, 61, 32, 
    63, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_33 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    65, 108, 105, 99, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[16]; 
} const moonbit_string_literal_15 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 15), 
    44, 32, 100, 115, 116, 46, 108, 101, 110, 103, 116, 104, 32, 61, 
    32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[19]; 
} const moonbit_string_literal_21 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 18), 
    32, 98, 117, 116, 32, 116, 104, 101, 32, 105, 110, 100, 101, 120, 
    32, 105, 115, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_19 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 97, 114, 
    114, 97, 121, 118, 105, 101, 119, 46, 109, 98, 116, 58, 51, 53, 49, 
    58, 53, 45, 51, 53, 49, 58, 51, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[18]; 
} const moonbit_string_literal_34 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 17), 
    97, 108, 105, 99, 101, 64, 101, 120, 97, 109, 112, 108, 101, 46, 
    99, 111, 109, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[20]; 
} const moonbit_string_literal_32 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 19), 
    67, 114, 101, 97, 116, 101, 100, 32, 117, 115, 101, 114, 115, 32, 
    116, 97, 98, 108, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint8_t const data[1]; 
} const moonbit_bytes_literal_0 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 0, 0), 0};

struct moonbit_object const moonbit_constant_constructor_0 =
  { -1, Moonbit_make_regular_object_header(2, 0, 0)};

struct moonbit_object const moonbit_constant_constructor_1 =
  { -1, Moonbit_make_regular_object_header(2, 0, 1)};

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode data;
  
} const $moonbitlang$x$encoding$t_fill$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $moonbitlang$x$encoding$t_fill$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode data;
  
} const $$moonbitlang$x$encoding$Decoder$$decode_utf_16le$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $$moonbitlang$x$encoding$Decoder$$decode_utf_16le$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode data;
  
} const $$moonbitlang$x$encoding$Decoder$$t_decode_utf_8$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $$moonbitlang$x$encoding$Decoder$$t_decode_utf_8$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode data;
  
} const $$moonbitlang$x$encoding$Decoder$$decode_utf_16be$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $$moonbitlang$x$encoding$Decoder$$decode_utf_16be$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$moonbitlang$core$buffer$Buffer$2a$Char$3e$$3d$$3e$Unit data;
  
} const $moonbitlang$x$encoding$write_utf16be_char$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $moonbitlang$x$encoding$write_utf16be_char$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode data;
  
} const $moonbitlang$x$encoding$t_decode_utf_16be_lo$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $moonbitlang$x$encoding$t_decode_utf_16be_lo$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode data;
  
} const $$moonbitlang$x$encoding$Decoder$$decode_utf_8$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $$moonbitlang$x$encoding$Decoder$$decode_utf_8$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode data;
  
} const $$moonbitlang$x$encoding$Decoder$$t_decode_utf_16le$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $$moonbitlang$x$encoding$Decoder$$t_decode_utf_16le$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode data;
  
} const $$moonbitlang$x$encoding$Decoder$$t_decode_utf_16be$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $$moonbitlang$x$encoding$Decoder$$t_decode_utf_16be$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode data;
  
} const $moonbitlang$x$encoding$t_decode_utf_16le_lo$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $moonbitlang$x$encoding$t_decode_utf_16le_lo$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$moonbitlang$core$buffer$Buffer$2a$Char$3e$$3d$$3e$Unit data;
  
} const $moonbitlang$x$encoding$write_utf8_char$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $moonbitlang$x$encoding$write_utf8_char$dyncall
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

struct { int32_t rc; uint32_t meta; int32_t data[256]; 
} $moonbitlang$x$encoding$utf_8_len$object =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 0, 256), 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
    2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 
    4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  };

int32_t* $moonbitlang$x$encoding$utf_8_len =
  $moonbitlang$x$encoding$utf_8_len$object.data;

int64_t $moonbitlang$x$encoding$malformed_pair$constr$532;

moonbit_string_t $example$gen$get_user_sql =
  (moonbit_string_t)moonbit_string_literal_0.data;

moonbit_string_t $example$gen$list_users_sql =
  (moonbit_string_t)moonbit_string_literal_1.data;

moonbit_string_t $example$gen$create_user_sql =
  (moonbit_string_t)moonbit_string_literal_2.data;

moonbit_string_t $example$src$main$_init$2a$$create_table_sql$7c$2 =
  (moonbit_string_t)moonbit_string_literal_3.data;

struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $moonbitlang$x$encoding$t_decode_utf_16le_lo$clo;

struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $$moonbitlang$x$encoding$Decoder$$decode_utf_16le$clo;

struct $$3c$$moonbitlang$core$buffer$Buffer$2a$Char$3e$$3d$$3e$Unit* $moonbitlang$x$encoding$write_utf8_char$clo;

struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $$moonbitlang$x$encoding$Decoder$$t_decode_utf_16be$clo;

struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $$moonbitlang$x$encoding$Decoder$$t_decode_utf_16le$clo;

struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $moonbitlang$x$encoding$t_fill$clo;

struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $$moonbitlang$x$encoding$Decoder$$decode_utf_16be$clo;

struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $$moonbitlang$x$encoding$Decoder$$decode_utf_8$clo;

struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $moonbitlang$x$encoding$t_decode_utf_16be_lo$clo;

struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $$moonbitlang$x$encoding$Decoder$$t_decode_utf_8$clo;

struct $$3c$$moonbitlang$core$buffer$Buffer$2a$Char$3e$$3d$$3e$Unit* $moonbitlang$x$encoding$write_utf16be_char$clo;

int32_t $moonbitlang$x$encoding$write_utf16be_char$dyncall(
  struct $$3c$$moonbitlang$core$buffer$Buffer$2a$Char$3e$$3d$$3e$Unit* _env$1965,
  struct $$moonbitlang$core$buffer$Buffer* buf$548,
  int32_t value$545
) {
  $moonbitlang$x$encoding$write_utf16be_char(buf$548, value$545);
  return 0;
}

void* $$moonbitlang$x$encoding$Decoder$$t_decode_utf_8$dyncall(
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1964,
  struct $$moonbitlang$x$encoding$Decoder* self$675
) {
  return $$moonbitlang$x$encoding$Decoder$$t_decode_utf_8(self$675);
}

void* $moonbitlang$x$encoding$t_decode_utf_16be_lo$dyncall(
  struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1963,
  int32_t hi$634,
  struct $$moonbitlang$x$encoding$Decoder* self$633
) {
  return $moonbitlang$x$encoding$t_decode_utf_16be_lo(hi$634, self$633);
}

void* $$moonbitlang$x$encoding$Decoder$$decode_utf_8$dyncall(
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1962,
  struct $$moonbitlang$x$encoding$Decoder* self$671
) {
  return $$moonbitlang$x$encoding$Decoder$$decode_utf_8(self$671);
}

void* $$moonbitlang$x$encoding$Decoder$$decode_utf_16be$dyncall(
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1961,
  struct $$moonbitlang$x$encoding$Decoder* self$619
) {
  return $$moonbitlang$x$encoding$Decoder$$decode_utf_16be(self$619);
}

void* $moonbitlang$x$encoding$t_fill$dyncall(
  struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1960,
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* k$605,
  struct $$moonbitlang$x$encoding$Decoder* decoder$603
) {
  return $moonbitlang$x$encoding$t_fill(k$605, decoder$603);
}

void* $$moonbitlang$x$encoding$Decoder$$t_decode_utf_16le$dyncall(
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1959,
  struct $$moonbitlang$x$encoding$Decoder* self$653
) {
  return $$moonbitlang$x$encoding$Decoder$$t_decode_utf_16le(self$653);
}

void* $$moonbitlang$x$encoding$Decoder$$t_decode_utf_16be$dyncall(
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1958,
  struct $$moonbitlang$x$encoding$Decoder* self$635
) {
  return $$moonbitlang$x$encoding$Decoder$$t_decode_utf_16be(self$635);
}

int32_t $moonbitlang$x$encoding$write_utf8_char$dyncall(
  struct $$3c$$moonbitlang$core$buffer$Buffer$2a$Char$3e$$3d$$3e$Unit* _env$1957,
  struct $$moonbitlang$core$buffer$Buffer* buf$559,
  int32_t value$557
) {
  $moonbitlang$x$encoding$write_utf8_char(buf$559, value$557);
  return 0;
}

void* $$moonbitlang$x$encoding$Decoder$$decode_utf_16le$dyncall(
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1956,
  struct $$moonbitlang$x$encoding$Decoder* self$637
) {
  return $$moonbitlang$x$encoding$Decoder$$decode_utf_16le(self$637);
}

void* $moonbitlang$x$encoding$t_decode_utf_16le_lo$dyncall(
  struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1955,
  int32_t hi$652,
  struct $$moonbitlang$x$encoding$Decoder* decoder$651
) {
  return $moonbitlang$x$encoding$t_decode_utf_16le_lo(hi$652, decoder$651);
}

moonbit_bytes_t $example$src$main$cstring(moonbit_string_t s$713) {
  return $moonbitlang$x$encoding$encode(0, s$713);
}

int32_t $example$gen$create_user(
  void* db$711,
  struct $$example$gen$CreateUserParams* params$712
) {
  moonbit_bytes_t _tmp$1954;
  void* _tmp$1970;
  void* stmt$710;
  moonbit_string_t _field$1969;
  moonbit_string_t name$1949;
  moonbit_bytes_t _tmp$1948;
  int32_t _tmp$1968;
  int32_t _tmp$1947;
  moonbit_string_t _field$1967;
  int32_t _cnt$2126;
  moonbit_string_t email$1952;
  moonbit_bytes_t _tmp$1951;
  int32_t _tmp$1966;
  int32_t _tmp$1950;
  int32_t _tmp$1953;
  moonbit_incref($example$gen$create_user_sql);
  _tmp$1954 = $example$gen$cstring($example$gen$create_user_sql);
  _tmp$1970 = $mizchi$sqlite$sqlite_prepare(db$711, _tmp$1954);
  if (db$711) {
    moonbit_decref(db$711);
  }
  moonbit_decref(_tmp$1954);
  stmt$710 = _tmp$1970;
  _field$1969 = params$712->$0;
  name$1949 = _field$1969;
  moonbit_incref(name$1949);
  _tmp$1948 = $example$gen$cstring(name$1949);
  _tmp$1968 = $mizchi$sqlite$sqlite_bind_text(stmt$710, 1, _tmp$1948);
  moonbit_decref(_tmp$1948);
  _tmp$1947 = _tmp$1968;
  _field$1967 = params$712->$1;
  _cnt$2126 = Moonbit_object_header(params$712)->rc;
  if (_cnt$2126 > 1) {
    int32_t _new_cnt$2128 = _cnt$2126 - 1;
    Moonbit_object_header(params$712)->rc = _new_cnt$2128;
    moonbit_incref(_field$1967);
  } else if (_cnt$2126 == 1) {
    moonbit_string_t _field$2127 = params$712->$0;
    moonbit_decref(_field$2127);
    moonbit_free(params$712);
  }
  email$1952 = _field$1967;
  _tmp$1951 = $example$gen$cstring(email$1952);
  _tmp$1966 = $mizchi$sqlite$sqlite_bind_text(stmt$710, 2, _tmp$1951);
  moonbit_decref(_tmp$1951);
  _tmp$1950 = _tmp$1966;
  _tmp$1953 = $mizchi$sqlite$sqlite_step(stmt$710);
  $mizchi$sqlite$sqlite_finalize(stmt$710);
  if (stmt$710) {
    moonbit_decref(stmt$710);
  }
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$$example$gen$ListUsersRow$3e$* $example$gen$list_users(
  void* db$706
) {
  moonbit_bytes_t _tmp$1946;
  void* _tmp$1971;
  void* stmt$705;
  struct $$example$gen$ListUsersRow** _tmp$1945;
  struct $$moonbitlang$core$builtin$Array$3c$$example$gen$ListUsersRow$3e$* results$707;
  moonbit_incref($example$gen$list_users_sql);
  _tmp$1946 = $example$gen$cstring($example$gen$list_users_sql);
  _tmp$1971 = $mizchi$sqlite$sqlite_prepare(db$706, _tmp$1946);
  if (db$706) {
    moonbit_decref(db$706);
  }
  moonbit_decref(_tmp$1946);
  stmt$705 = _tmp$1971;
  _tmp$1945 = (struct $$example$gen$ListUsersRow**)moonbit_empty_ref_array;
  results$707
  = (struct $$moonbitlang$core$builtin$Array$3c$$example$gen$ListUsersRow$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$example$gen$ListUsersRow$3e$
      )
    );
  Moonbit_object_header(results$707)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$example$gen$ListUsersRow$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  results$707->$0 = _tmp$1945;
  results$707->$1 = 0;
  while (1) {
    int32_t _tmp$1939 = $mizchi$sqlite$sqlite_step(stmt$705);
    if (_tmp$1939 == 100) {
      int64_t _tmp$1940 = $mizchi$sqlite$sqlite_column_int64(stmt$705, 0);
      moonbit_bytes_t _tmp$1944 =
        $mizchi$sqlite$sqlite_column_text(stmt$705, 1);
      moonbit_string_t _tmp$1941 = $example$gen$bytes_to_string(_tmp$1944);
      moonbit_bytes_t _tmp$1943 =
        $mizchi$sqlite$sqlite_column_text(stmt$705, 2);
      moonbit_string_t _tmp$1942 = $example$gen$bytes_to_string(_tmp$1943);
      struct $$example$gen$ListUsersRow* row$708 =
        (struct $$example$gen$ListUsersRow*)moonbit_malloc(
          sizeof(struct $$example$gen$ListUsersRow)
        );
      Moonbit_object_header(row$708)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $$example$gen$ListUsersRow, $1) >> 2, 2, 0
      );
      row$708->$0 = _tmp$1940;
      row$708->$1 = _tmp$1941;
      row$708->$2 = _tmp$1942;
      moonbit_incref(results$707);
      $$moonbitlang$core$builtin$Array$$push$0(results$707, row$708);
      continue;
    }
    break;
  }
  $mizchi$sqlite$sqlite_finalize(stmt$705);
  if (stmt$705) {
    moonbit_decref(stmt$705);
  }
  return results$707;
}

struct $$example$gen$GetUserRow* $example$gen$get_user(
  void* db$702,
  struct $$example$gen$GetUserParams* params$703
) {
  moonbit_bytes_t _tmp$1938;
  void* _tmp$1973;
  void* stmt$701;
  int64_t _field$1972;
  int64_t id$1931;
  int32_t _tmp$1930;
  int32_t _tmp$1929;
  int32_t _tmp$1932;
  moonbit_incref($example$gen$get_user_sql);
  _tmp$1938 = $example$gen$cstring($example$gen$get_user_sql);
  _tmp$1973 = $mizchi$sqlite$sqlite_prepare(db$702, _tmp$1938);
  if (db$702) {
    moonbit_decref(db$702);
  }
  moonbit_decref(_tmp$1938);
  stmt$701 = _tmp$1973;
  _field$1972 = params$703->$0;
  moonbit_decref(params$703);
  id$1931 = _field$1972;
  _tmp$1930 = (int32_t)id$1931;
  _tmp$1929 = $mizchi$sqlite$sqlite_bind_int(stmt$701, 1, _tmp$1930);
  _tmp$1932 = $mizchi$sqlite$sqlite_step(stmt$701);
  if (_tmp$1932 == 100) {
    int64_t _tmp$1933 = $mizchi$sqlite$sqlite_column_int64(stmt$701, 0);
    moonbit_bytes_t _tmp$1937 =
      $mizchi$sqlite$sqlite_column_text(stmt$701, 1);
    moonbit_string_t _tmp$1934 = $example$gen$bytes_to_string(_tmp$1937);
    moonbit_bytes_t _tmp$1936 =
      $mizchi$sqlite$sqlite_column_text(stmt$701, 2);
    moonbit_string_t _tmp$1935 = $example$gen$bytes_to_string(_tmp$1936);
    struct $$example$gen$GetUserRow* row$704 =
      (struct $$example$gen$GetUserRow*)moonbit_malloc(
        sizeof(struct $$example$gen$GetUserRow)
      );
    Moonbit_object_header(row$704)->meta
    = Moonbit_make_regular_object_header(
      offsetof(struct $$example$gen$GetUserRow, $1) >> 2, 2, 0
    );
    row$704->$0 = _tmp$1933;
    row$704->$1 = _tmp$1934;
    row$704->$2 = _tmp$1935;
    $mizchi$sqlite$sqlite_finalize(stmt$701);
    if (stmt$701) {
      moonbit_decref(stmt$701);
    }
    return row$704;
  } else {
    $mizchi$sqlite$sqlite_finalize(stmt$701);
    if (stmt$701) {
      moonbit_decref(stmt$701);
    }
    return 0;
  }
}

struct $$example$gen$CreateUserParams* $$example$gen$CreateUserParams$$new(
  moonbit_string_t name$699,
  moonbit_string_t email$700
) {
  struct $$example$gen$CreateUserParams* _block$2175 =
    (struct $$example$gen$CreateUserParams*)moonbit_malloc(
      sizeof(struct $$example$gen$CreateUserParams)
    );
  Moonbit_object_header(_block$2175)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$example$gen$CreateUserParams, $0) >> 2, 2, 0
  );
  _block$2175->$0 = name$699;
  _block$2175->$1 = email$700;
  return _block$2175;
}

struct $$example$gen$GetUserParams* $$example$gen$GetUserParams$$new(
  int64_t id$698
) {
  struct $$example$gen$GetUserParams* _block$2176 =
    (struct $$example$gen$GetUserParams*)moonbit_malloc(
      sizeof(struct $$example$gen$GetUserParams)
    );
  Moonbit_object_header(_block$2176)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $$example$gen$GetUserParams) >> 2, 0, 0
  );
  _block$2176->$0 = id$698;
  return _block$2176;
}

moonbit_string_t $example$gen$bytes_to_string(moonbit_bytes_t b$697) {
  struct $$moonbitlang$x$encoding$Decoder* decoder$696 =
    $moonbitlang$x$encoding$decoder(0);
  int32_t _tmp$1928 = Moonbit_array_length(b$697);
  int64_t _tmp$1927 = (int64_t)_tmp$1928;
  struct $BytesView _tmp$1926 = $Bytes$$sub$inner(b$697, 0, _tmp$1927);
  return $$moonbitlang$x$encoding$Decoder$$decode_lossy$inner(
           decoder$696, _tmp$1926, 0
         );
}

moonbit_bytes_t $example$gen$cstring(moonbit_string_t s$695) {
  return $moonbitlang$x$encoding$encode(0, s$695);
}

moonbit_string_t $$moonbitlang$x$encoding$Decoder$$decode_lossy$inner(
  struct $$moonbitlang$x$encoding$Decoder* self$688,
  struct $BytesView input$687,
  int32_t stream$694
) {
  int32_t end$1905 = input$687.$2;
  int32_t start$1906 = input$687.$1;
  int32_t _tmp$1904 = end$1905 - start$1906;
  moonbit_bytes_t _field$1982;
  moonbit_bytes_t i$1910;
  int32_t _tmp$1981;
  int32_t _tmp$1908;
  int32_t i_pos$1909;
  int32_t _tmp$1907;
  int32_t* _tmp$1925;
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* chars$689;
  void* _tmp$1924;
  void* _param$690;
  if (_tmp$1904 > 0) {
    moonbit_incref(self$688);
    $$moonbitlang$x$encoding$Decoder$$i_cont(self$688, input$687);
  } else {
    moonbit_decref(input$687.$0);
  }
  _field$1982 = self$688->$0;
  i$1910 = _field$1982;
  _tmp$1981 = Moonbit_array_length(i$1910);
  _tmp$1908 = _tmp$1981;
  i_pos$1909 = self$688->$1;
  _tmp$1907 = _tmp$1908 - i_pos$1909;
  if (_tmp$1907 == 0) {
    moonbit_decref(self$688);
    return (moonbit_string_t)moonbit_string_literal_4.data;
  }
  _tmp$1925 = (int32_t*)moonbit_empty_int32_array;
  chars$689
  = (struct $$moonbitlang$core$builtin$Array$3c$Char$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Char$3e$)
    );
  Moonbit_object_header(chars$689)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Char$3e$, $0) >> 2,
      1,
      0
  );
  chars$689->$0 = _tmp$1925;
  chars$689->$1 = 0;
  moonbit_incref(self$688);
  _tmp$1924 = $$moonbitlang$x$encoding$Decoder$$decode_(self$688);
  _param$690 = _tmp$1924;
  $$2a$loop$693:;
  while (1) {
    switch (Moonbit_object_tag(_param$690)) {
      case 3: {
        struct $$moonbitlang$x$encoding$Decode$Uchar* _Uchar$691 =
          (struct $$moonbitlang$x$encoding$Decode$Uchar*)_param$690;
        int32_t _field$1974 = _Uchar$691->$0;
        int32_t _u$692;
        void* _tmp$1923;
        moonbit_decref(_Uchar$691);
        _u$692 = _field$1974;
        moonbit_incref(chars$689);
        $$moonbitlang$core$builtin$Array$$push$1(chars$689, _u$692);
        moonbit_incref(self$688);
        _tmp$1923 = $$moonbitlang$x$encoding$Decoder$$decode_(self$688);
        _param$690 = _tmp$1923;
        goto $$2a$loop$693;
        break;
      }
      
      case 2: {
        int32_t _if_result$2178;
        moonbit_decref(_param$690);
        if (stream$694) {
          int32_t t_need$1918 = self$688->$4;
          _if_result$2178 = t_need$1918 > 0;
        } else {
          _if_result$2178 = 0;
        }
        if (_if_result$2178) {
          int32_t* _field$1976;
          int32_t* buf$1920;
          int32_t _field$1975;
          int32_t _cnt$2129;
          int32_t len$1921;
          struct $$moonbitlang$core$builtin$ArrayView$3c$Char$3e$ _tmp$1919;
          moonbit_decref(self$688);
          _field$1976 = chars$689->$0;
          buf$1920 = _field$1976;
          _field$1975 = chars$689->$1;
          _cnt$2129 = Moonbit_object_header(chars$689)->rc;
          if (_cnt$2129 > 1) {
            int32_t _new_cnt$2130 = _cnt$2129 - 1;
            Moonbit_object_header(chars$689)->rc = _new_cnt$2130;
            moonbit_incref(buf$1920);
          } else if (_cnt$2129 == 1) {
            moonbit_free(chars$689);
          }
          len$1921 = _field$1975;
          _tmp$1919
          = (struct $$moonbitlang$core$builtin$ArrayView$3c$Char$3e$){
            0, len$1921, buf$1920
          };
          return $String$$from_array(_tmp$1919);
        } else {
          void* _tmp$1922;
          moonbit_incref(chars$689);
          $$moonbitlang$core$builtin$Array$$push$1(chars$689, 65533);
          moonbit_incref(self$688);
          _tmp$1922 = $$moonbitlang$x$encoding$Decoder$$decode_(self$688);
          _param$690 = _tmp$1922;
          goto $$2a$loop$693;
        }
        break;
      }
      
      case 0: {
        int32_t* _field$1978;
        int32_t* buf$1916;
        int32_t _field$1977;
        int32_t _cnt$2131;
        int32_t len$1917;
        struct $$moonbitlang$core$builtin$ArrayView$3c$Char$3e$ _tmp$1915;
        moonbit_decref(self$688);
        _field$1978 = chars$689->$0;
        buf$1916 = _field$1978;
        _field$1977 = chars$689->$1;
        _cnt$2131 = Moonbit_object_header(chars$689)->rc;
        if (_cnt$2131 > 1) {
          int32_t _new_cnt$2132 = _cnt$2131 - 1;
          Moonbit_object_header(chars$689)->rc = _new_cnt$2132;
          moonbit_incref(buf$1916);
        } else if (_cnt$2131 == 1) {
          moonbit_free(chars$689);
        }
        len$1917 = _field$1977;
        _tmp$1915
        = (struct $$moonbitlang$core$builtin$ArrayView$3c$Char$3e$){
          0, len$1917, buf$1916
        };
        return $String$$from_array(_tmp$1915);
        break;
      }
      default: {
        moonbit_decref(_param$690);
        if (stream$694) {
          int32_t* _field$1980;
          int32_t* buf$1912;
          int32_t _field$1979;
          int32_t _cnt$2133;
          int32_t len$1913;
          struct $$moonbitlang$core$builtin$ArrayView$3c$Char$3e$ _tmp$1911;
          moonbit_decref(self$688);
          _field$1980 = chars$689->$0;
          buf$1912 = _field$1980;
          _field$1979 = chars$689->$1;
          _cnt$2133 = Moonbit_object_header(chars$689)->rc;
          if (_cnt$2133 > 1) {
            int32_t _new_cnt$2134 = _cnt$2133 - 1;
            Moonbit_object_header(chars$689)->rc = _new_cnt$2134;
            moonbit_incref(buf$1912);
          } else if (_cnt$2133 == 1) {
            moonbit_free(chars$689);
          }
          len$1913 = _field$1979;
          _tmp$1911
          = (struct $$moonbitlang$core$builtin$ArrayView$3c$Char$3e$){
            0, len$1913, buf$1912
          };
          return $String$$from_array(_tmp$1911);
        } else {
          void* _tmp$1914;
          moonbit_incref(self$688);
          _tmp$1914 = $$moonbitlang$x$encoding$Decoder$$decode_(self$688);
          _param$690 = _tmp$1914;
          goto $$2a$loop$693;
        }
        break;
      }
    }
    break;
  }
}

int32_t $$moonbitlang$x$encoding$Decoder$$i_cont(
  struct $$moonbitlang$x$encoding$Decoder* self$683,
  struct $BytesView input$685
) {
  moonbit_bytes_t _field$1986 = self$683->$0;
  moonbit_bytes_t i$1903 = _field$1986;
  int32_t _tmp$1985 = Moonbit_array_length(i$1903);
  int32_t _tmp$1901 = _tmp$1985;
  int32_t i_pos$1902 = self$683->$1;
  int32_t _p$981 = _tmp$1901 - i_pos$1902;
  int32_t _p$982 = 0;
  int32_t i_rem$682;
  int32_t end$1899;
  int32_t start$1900;
  int32_t _tmp$1898;
  int32_t new_len$684;
  moonbit_bytes_t new_i$686;
  moonbit_bytes_t _old$1983;
  if (_p$981 > _p$982) {
    i_rem$682 = _p$981;
  } else {
    i_rem$682 = _p$982;
  }
  end$1899 = input$685.$2;
  start$1900 = input$685.$1;
  _tmp$1898 = end$1899 - start$1900;
  new_len$684 = i_rem$682 + _tmp$1898;
  new_i$686 = (moonbit_bytes_t)moonbit_make_bytes(new_len$684, 0);
  if (i_rem$682 > 0) {
    moonbit_bytes_t _field$1984 = self$683->$0;
    moonbit_bytes_t i$1896 = _field$1984;
    int32_t i_pos$1897 = self$683->$1;
    moonbit_incref(i$1896);
    moonbit_incref(new_i$686);
    $FixedArray$$blit_to$inner$0(i$1896, new_i$686, i_rem$682, i_pos$1897, 0);
  }
  moonbit_incref(new_i$686);
  $FixedArray$$blit_from_bytesview(new_i$686, i_rem$682, input$685);
  _old$1983 = self$683->$0;
  moonbit_decref(_old$1983);
  self$683->$0 = new_i$686;
  self$683->$1 = 0;
  moonbit_decref(self$683);
  return 0;
}

void* $$moonbitlang$x$encoding$Decoder$$decode_(
  struct $$moonbitlang$x$encoding$Decoder* self$681
) {
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _field$1987 =
    self$681->$5;
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _func$680 =
    _field$1987;
  moonbit_incref(_func$680);
  return _func$680->code(_func$680, self$681);
}

struct $$moonbitlang$x$encoding$Decoder* $moonbitlang$x$encoding$decoder(
  int32_t encoding$679
) {
  moonbit_bytes_t i$676 =
    $$moonbitlang$core$builtin$Default$$FixedArray$$default$0();
  moonbit_bytes_t t$677 = (moonbit_bytes_t)moonbit_make_bytes(4, 0);
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* k$678;
  struct $$moonbitlang$x$encoding$Decoder* _block$2179;
  switch (encoding$679) {
    case 0: {
      moonbit_incref($$moonbitlang$x$encoding$Decoder$$decode_utf_8$clo);
      k$678 = $$moonbitlang$x$encoding$Decoder$$decode_utf_8$clo;
      break;
    }
    
    case 1: {
      moonbit_incref($$moonbitlang$x$encoding$Decoder$$decode_utf_16le$clo);
      k$678 = $$moonbitlang$x$encoding$Decoder$$decode_utf_16le$clo;
      break;
    }
    
    case 2: {
      moonbit_incref($$moonbitlang$x$encoding$Decoder$$decode_utf_16le$clo);
      k$678 = $$moonbitlang$x$encoding$Decoder$$decode_utf_16le$clo;
      break;
    }
    default: {
      moonbit_incref($$moonbitlang$x$encoding$Decoder$$decode_utf_16be$clo);
      k$678 = $$moonbitlang$x$encoding$Decoder$$decode_utf_16be$clo;
      break;
    }
  }
  _block$2179
  = (struct $$moonbitlang$x$encoding$Decoder*)moonbit_malloc(
      sizeof(struct $$moonbitlang$x$encoding$Decoder)
    );
  Moonbit_object_header(_block$2179)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$x$encoding$Decoder, $0) >> 2, 3, 0
  );
  _block$2179->$0 = i$676;
  _block$2179->$1 = 0;
  _block$2179->$2 = t$677;
  _block$2179->$3 = 0;
  _block$2179->$4 = 0;
  _block$2179->$5 = k$678;
  return _block$2179;
}

void* $$moonbitlang$x$encoding$Decoder$$t_decode_utf_8(
  struct $$moonbitlang$x$encoding$Decoder* self$675
) {
  int32_t t_len$1888 = self$675->$3;
  int32_t t_need$1889 = self$675->$4;
  if (t_len$1888 < t_need$1889) {
    moonbit_bytes_t _field$1988 = self$675->$2;
    moonbit_bytes_t t$1891 = _field$1988;
    int32_t t_len$1892 = self$675->$3;
    void* _tmp$1890;
    moonbit_incref(t$1891);
    _tmp$1890 = $moonbitlang$x$encoding$malformed(t$1891, 0, t_len$1892);
    moonbit_incref($$moonbitlang$x$encoding$Decoder$$decode_utf_8$clo);
    return $$moonbitlang$x$encoding$Decoder$$ret(
             self$675,
               $$moonbitlang$x$encoding$Decoder$$decode_utf_8$clo,
               _tmp$1890
           );
  } else {
    moonbit_bytes_t _field$1989 = self$675->$2;
    moonbit_bytes_t t$1894 = _field$1989;
    int32_t t_len$1895 = self$675->$3;
    void* _tmp$1893;
    moonbit_incref(t$1894);
    _tmp$1893 = $moonbitlang$x$encoding$r_utf_8(t$1894, 0, t_len$1895);
    moonbit_incref($$moonbitlang$x$encoding$Decoder$$decode_utf_8$clo);
    return $$moonbitlang$x$encoding$Decoder$$ret(
             self$675,
               $$moonbitlang$x$encoding$Decoder$$decode_utf_8$clo,
               _tmp$1893
           );
  }
}

void* $$moonbitlang$x$encoding$Decoder$$decode_utf_8(
  struct $$moonbitlang$x$encoding$Decoder* self$671
) {
  moonbit_bytes_t _field$1996 = self$671->$0;
  moonbit_bytes_t i$1887 = _field$1996;
  int32_t _tmp$1995 = Moonbit_array_length(i$1887);
  int32_t _tmp$1885 = _tmp$1995;
  int32_t i_pos$1886 = self$671->$1;
  int32_t rem$670 = _tmp$1885 - i_pos$1886;
  if (rem$670 <= 0) {
    moonbit_decref(self$671);
    return (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  } else {
    moonbit_bytes_t _field$1994 = self$671->$0;
    moonbit_bytes_t i$1883 = _field$1994;
    int32_t i_pos$1884 = self$671->$1;
    int32_t _tmp$1993;
    int32_t _tmp$1882;
    int32_t idx$672;
    int32_t* _tmp$1881;
    int32_t _tmp$1992;
    int32_t need$673;
    if (i_pos$1884 < 0 || i_pos$1884 >= Moonbit_array_length(i$1883)) {
      moonbit_panic();
    }
    _tmp$1993 = (int32_t)i$1883[i_pos$1884];
    _tmp$1882 = _tmp$1993;
    idx$672 = (int32_t)_tmp$1882;
    moonbit_incref($moonbitlang$x$encoding$utf_8_len);
    _tmp$1881 = $moonbitlang$x$encoding$utf_8_len;
    if (idx$672 < 0 || idx$672 >= Moonbit_array_length(_tmp$1881)) {
      moonbit_panic();
    }
    _tmp$1992 = (int32_t)_tmp$1881[idx$672];
    moonbit_decref(_tmp$1881);
    need$673 = _tmp$1992;
    if (rem$670 < need$673) {
      moonbit_incref(self$671);
      $$moonbitlang$x$encoding$Decoder$$t_need(self$671, need$673);
      moonbit_incref($$moonbitlang$x$encoding$Decoder$$t_decode_utf_8$clo);
      return $moonbitlang$x$encoding$t_fill(
               $$moonbitlang$x$encoding$Decoder$$t_decode_utf_8$clo, self$671
             );
    } else {
      int32_t j$674 = self$671->$1;
      if (need$673 == 0) {
        int32_t i_pos$1874 = self$671->$1;
        int32_t _tmp$1873 = i_pos$1874 + 1;
        moonbit_bytes_t _field$1990;
        moonbit_bytes_t i$1876;
        void* _tmp$1875;
        self$671->$1 = _tmp$1873;
        _field$1990 = self$671->$0;
        i$1876 = _field$1990;
        moonbit_incref(i$1876);
        _tmp$1875 = $moonbitlang$x$encoding$malformed(i$1876, j$674, 1);
        moonbit_incref($$moonbitlang$x$encoding$Decoder$$decode_utf_8$clo);
        return $$moonbitlang$x$encoding$Decoder$$ret(
                 self$671,
                   $$moonbitlang$x$encoding$Decoder$$decode_utf_8$clo,
                   _tmp$1875
               );
      } else {
        int32_t i_pos$1878 = self$671->$1;
        int32_t _tmp$1877 = i_pos$1878 + need$673;
        moonbit_bytes_t _field$1991;
        moonbit_bytes_t i$1880;
        void* _tmp$1879;
        self$671->$1 = _tmp$1877;
        _field$1991 = self$671->$0;
        i$1880 = _field$1991;
        moonbit_incref(i$1880);
        _tmp$1879 = $moonbitlang$x$encoding$r_utf_8(i$1880, j$674, need$673);
        moonbit_incref($$moonbitlang$x$encoding$Decoder$$decode_utf_8$clo);
        return $$moonbitlang$x$encoding$Decoder$$ret(
                 self$671,
                   $$moonbitlang$x$encoding$Decoder$$decode_utf_8$clo,
                   _tmp$1879
               );
      }
    }
  }
}

void* $moonbitlang$x$encoding$r_utf_8(
  moonbit_bytes_t bytes$657,
  int32_t offset$658,
  int32_t length$656
) {
  int32_t c$655;
  int32_t _tmp$1830;
  void* _block$2182;
  switch (length$656) {
    case 1: {
      int32_t _tmp$1997;
      int32_t _tmp$1832;
      int32_t _tmp$1831;
      if (offset$658 < 0 || offset$658 >= Moonbit_array_length(bytes$657)) {
        moonbit_panic();
      }
      _tmp$1997 = (int32_t)bytes$657[offset$658];
      moonbit_decref(bytes$657);
      _tmp$1832 = _tmp$1997;
      _tmp$1831 = (int32_t)_tmp$1832;
      c$655 = _tmp$1831;
      goto $join$654;
      break;
    }
    
    case 2: {
      int32_t _tmp$1840;
      int32_t b0$659;
      int32_t _tmp$1839;
      int32_t _tmp$1838;
      int32_t b1$660;
      int32_t _tmp$1833;
      if (offset$658 < 0 || offset$658 >= Moonbit_array_length(bytes$657)) {
        moonbit_panic();
      }
      _tmp$1840 = (int32_t)bytes$657[offset$658];
      b0$659 = (int32_t)_tmp$1840;
      _tmp$1839 = offset$658 + 1;
      if (_tmp$1839 < 0 || _tmp$1839 >= Moonbit_array_length(bytes$657)) {
        moonbit_panic();
      }
      _tmp$1838 = (int32_t)bytes$657[_tmp$1839];
      b1$660 = (int32_t)_tmp$1838;
      _tmp$1833 = b1$660 >> 6;
      if (_tmp$1833 != 2) {
        return $moonbitlang$x$encoding$malformed(
                 bytes$657, offset$658, length$656
               );
      } else {
        int32_t _tmp$1837;
        int32_t _tmp$1835;
        int32_t _tmp$1836;
        int32_t _tmp$1834;
        moonbit_decref(bytes$657);
        _tmp$1837 = b0$659 & 31;
        _tmp$1835 = _tmp$1837 << 6;
        _tmp$1836 = b1$660 & 63;
        _tmp$1834 = _tmp$1835 | _tmp$1836;
        c$655 = _tmp$1834;
        goto $join$654;
      }
      break;
    }
    
    case 3: {
      int32_t _tmp$1853;
      int32_t b0$661;
      int32_t _tmp$1852;
      int32_t _tmp$1851;
      int32_t b1$662;
      int32_t _tmp$1850;
      int32_t _tmp$1849;
      int32_t b2$663;
      int32_t _tmp$1848;
      int32_t _tmp$1843;
      int32_t _tmp$1847;
      int32_t _tmp$1845;
      int32_t _tmp$1846;
      int32_t _tmp$1844;
      int32_t c$664;
      int32_t _tmp$1841;
      if (offset$658 < 0 || offset$658 >= Moonbit_array_length(bytes$657)) {
        moonbit_panic();
      }
      _tmp$1853 = (int32_t)bytes$657[offset$658];
      b0$661 = (int32_t)_tmp$1853;
      _tmp$1852 = offset$658 + 1;
      if (_tmp$1852 < 0 || _tmp$1852 >= Moonbit_array_length(bytes$657)) {
        moonbit_panic();
      }
      _tmp$1851 = (int32_t)bytes$657[_tmp$1852];
      b1$662 = (int32_t)_tmp$1851;
      _tmp$1850 = offset$658 + 2;
      if (_tmp$1850 < 0 || _tmp$1850 >= Moonbit_array_length(bytes$657)) {
        moonbit_panic();
      }
      _tmp$1849 = (int32_t)bytes$657[_tmp$1850];
      b2$663 = (int32_t)_tmp$1849;
      _tmp$1848 = b0$661 & 15;
      _tmp$1843 = _tmp$1848 << 12;
      _tmp$1847 = b1$662 & 63;
      _tmp$1845 = _tmp$1847 << 6;
      _tmp$1846 = b2$663 & 63;
      _tmp$1844 = _tmp$1845 | _tmp$1846;
      c$664 = _tmp$1843 | _tmp$1844;
      _tmp$1841 = b2$663 >> 6;
      if (_tmp$1841 != 2) {
        return $moonbitlang$x$encoding$malformed(
                 bytes$657, offset$658, length$656
               );
      } else {
        switch (b0$661) {
          case 224: {
            if (b1$662 < 160 || 191 < b1$662) {
              return $moonbitlang$x$encoding$malformed(
                       bytes$657, offset$658, length$656
                     );
            } else {
              moonbit_decref(bytes$657);
              c$655 = c$664;
              goto $join$654;
            }
            break;
          }
          
          case 237: {
            if (b1$662 < 128 || 159 < b1$662) {
              return $moonbitlang$x$encoding$malformed(
                       bytes$657, offset$658, length$656
                     );
            } else {
              moonbit_decref(bytes$657);
              c$655 = c$664;
              goto $join$654;
            }
            break;
          }
          default: {
            int32_t _tmp$1842 = b1$662 >> 6;
            if (_tmp$1842 != 2) {
              return $moonbitlang$x$encoding$malformed(
                       bytes$657, offset$658, length$656
                     );
            } else {
              moonbit_decref(bytes$657);
              c$655 = c$664;
              goto $join$654;
            }
            break;
          }
        }
      }
      break;
    }
    
    case 4: {
      int32_t _tmp$1872;
      int32_t b0$665;
      int32_t _tmp$1871;
      int32_t _tmp$1870;
      int32_t b1$666;
      int32_t _tmp$1869;
      int32_t _tmp$1868;
      int32_t b2$667;
      int32_t _tmp$1867;
      int32_t _tmp$1866;
      int32_t b3$668;
      int32_t _tmp$1865;
      int32_t _tmp$1862;
      int32_t _tmp$1864;
      int32_t _tmp$1863;
      int32_t _tmp$1859;
      int32_t _tmp$1861;
      int32_t _tmp$1860;
      int32_t _tmp$1857;
      int32_t _tmp$1858;
      int32_t c$669;
      int32_t _tmp$1855;
      int32_t _if_result$2181;
      if (offset$658 < 0 || offset$658 >= Moonbit_array_length(bytes$657)) {
        moonbit_panic();
      }
      _tmp$1872 = (int32_t)bytes$657[offset$658];
      b0$665 = (int32_t)_tmp$1872;
      _tmp$1871 = offset$658 + 1;
      if (_tmp$1871 < 0 || _tmp$1871 >= Moonbit_array_length(bytes$657)) {
        moonbit_panic();
      }
      _tmp$1870 = (int32_t)bytes$657[_tmp$1871];
      b1$666 = (int32_t)_tmp$1870;
      _tmp$1869 = offset$658 + 2;
      if (_tmp$1869 < 0 || _tmp$1869 >= Moonbit_array_length(bytes$657)) {
        moonbit_panic();
      }
      _tmp$1868 = (int32_t)bytes$657[_tmp$1869];
      b2$667 = (int32_t)_tmp$1868;
      _tmp$1867 = offset$658 + 3;
      if (_tmp$1867 < 0 || _tmp$1867 >= Moonbit_array_length(bytes$657)) {
        moonbit_panic();
      }
      _tmp$1866 = (int32_t)bytes$657[_tmp$1867];
      b3$668 = (int32_t)_tmp$1866;
      _tmp$1865 = b0$665 & 7;
      _tmp$1862 = _tmp$1865 << 18;
      _tmp$1864 = b1$666 & 63;
      _tmp$1863 = _tmp$1864 << 12;
      _tmp$1859 = _tmp$1862 | _tmp$1863;
      _tmp$1861 = b2$667 & 63;
      _tmp$1860 = _tmp$1861 << 6;
      _tmp$1857 = _tmp$1859 | _tmp$1860;
      _tmp$1858 = b3$668 & 63;
      c$669 = _tmp$1857 | _tmp$1858;
      _tmp$1855 = b3$668 >> 6;
      if (_tmp$1855 != 2) {
        _if_result$2181 = 1;
      } else {
        int32_t _tmp$1854 = b2$667 >> 6;
        _if_result$2181 = _tmp$1854 != 2;
      }
      if (_if_result$2181) {
        return $moonbitlang$x$encoding$malformed(
                 bytes$657, offset$658, length$656
               );
      } else {
        switch (b0$665) {
          case 240: {
            if (b1$666 < 144 || 191 < b1$666) {
              return $moonbitlang$x$encoding$malformed(
                       bytes$657, offset$658, length$656
                     );
            } else {
              moonbit_decref(bytes$657);
              c$655 = c$669;
              goto $join$654;
            }
            break;
          }
          
          case 244: {
            if (b1$666 < 128 || 143 < b1$666) {
              return $moonbitlang$x$encoding$malformed(
                       bytes$657, offset$658, length$656
                     );
            } else {
              moonbit_decref(bytes$657);
              c$655 = c$669;
              goto $join$654;
            }
            break;
          }
          default: {
            int32_t _tmp$1856 = b1$666 >> 6;
            if (_tmp$1856 != 2) {
              return $moonbitlang$x$encoding$malformed(
                       bytes$657, offset$658, length$656
                     );
            } else {
              moonbit_decref(bytes$657);
              c$655 = c$669;
              goto $join$654;
            }
            break;
          }
        }
      }
      break;
    }
    default: {
      moonbit_decref(bytes$657);
      moonbit_panic();
      break;
    }
  }
  $join$654:;
  _tmp$1830 = c$655;
  _block$2182
  = (void*)moonbit_malloc(
      sizeof(struct $$moonbitlang$x$encoding$Decode$Uchar)
    );
  Moonbit_object_header(_block$2182)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $$moonbitlang$x$encoding$Decode$Uchar) >> 2, 0, 3
  );
  ((struct $$moonbitlang$x$encoding$Decode$Uchar*)_block$2182)->$0
  = _tmp$1830;
  return _block$2182;
}

void* $$moonbitlang$x$encoding$Decoder$$t_decode_utf_16le(
  struct $$moonbitlang$x$encoding$Decoder* self$653
) {
  int32_t t_len$1823 = self$653->$3;
  int32_t t_need$1824 = self$653->$4;
  if (t_len$1823 < t_need$1824) {
    moonbit_bytes_t _field$1998 = self$653->$2;
    moonbit_bytes_t t$1826 = _field$1998;
    int32_t t_len$1827 = self$653->$3;
    void* _tmp$1825;
    moonbit_incref(t$1826);
    _tmp$1825 = $moonbitlang$x$encoding$malformed(t$1826, 0, t_len$1827);
    moonbit_incref($$moonbitlang$x$encoding$Decoder$$decode_utf_16le$clo);
    return $$moonbitlang$x$encoding$Decoder$$ret(
             self$653,
               $$moonbitlang$x$encoding$Decoder$$decode_utf_16le$clo,
               _tmp$1825
           );
  } else {
    moonbit_bytes_t _field$1999 = self$653->$2;
    moonbit_bytes_t t$1829 = _field$1999;
    void* _tmp$1828;
    moonbit_incref(t$1829);
    _tmp$1828 = $moonbitlang$x$encoding$r_utf_16(t$1829, 1, 0);
    return $$moonbitlang$x$encoding$Decoder$$decode_utf_16le_lo(
             self$653, _tmp$1828
           );
  }
}

void* $moonbitlang$x$encoding$t_decode_utf_16le_lo(
  int32_t hi$652,
  struct $$moonbitlang$x$encoding$Decoder* decoder$651
) {
  int32_t t_len$1816 = decoder$651->$3;
  int32_t t_need$1817 = decoder$651->$4;
  if (t_len$1816 < t_need$1817) {
    moonbit_bytes_t _field$2000 = decoder$651->$2;
    moonbit_bytes_t t$1819 = _field$2000;
    int32_t t_len$1820 = decoder$651->$3;
    void* _tmp$1818;
    moonbit_incref(t$1819);
    _tmp$1818
    = $moonbitlang$x$encoding$malformed_pair(
      0, hi$652, t$1819, 0, t_len$1820
    );
    moonbit_incref($$moonbitlang$x$encoding$Decoder$$decode_utf_16le$clo);
    return $$moonbitlang$x$encoding$Decoder$$ret(
             decoder$651,
               $$moonbitlang$x$encoding$Decoder$$decode_utf_16le$clo,
               _tmp$1818
           );
  } else {
    moonbit_bytes_t _field$2001 = decoder$651->$2;
    moonbit_bytes_t t$1822 = _field$2001;
    void* _tmp$1821;
    moonbit_incref(t$1822);
    _tmp$1821 = $moonbitlang$x$encoding$r_utf_16_lo(hi$652, t$1822, 1, 0);
    moonbit_incref($$moonbitlang$x$encoding$Decoder$$decode_utf_16le$clo);
    return $$moonbitlang$x$encoding$Decoder$$ret(
             decoder$651,
               $$moonbitlang$x$encoding$Decoder$$decode_utf_16le$clo,
               _tmp$1821
           );
  }
}

void* $$moonbitlang$x$encoding$Decoder$$decode_utf_16le_lo(
  struct $$moonbitlang$x$encoding$Decoder* self$642,
  void* v$639
) {
  switch (Moonbit_object_tag(v$639)) {
    case 2: {
      struct $$moonbitlang$x$encoding$UTF16Decode$UTF16Uchar* _UTF16Uchar$640 =
        (struct $$moonbitlang$x$encoding$UTF16Decode$UTF16Uchar*)v$639;
      int32_t _field$2002 = _UTF16Uchar$640->$0;
      int32_t _u$641;
      void* Uchar$1815;
      moonbit_decref(_UTF16Uchar$640);
      _u$641 = _field$2002;
      Uchar$1815
      = (void*)moonbit_malloc(
          sizeof(struct $$moonbitlang$x$encoding$Decode$Uchar)
        );
      Moonbit_object_header(Uchar$1815)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$moonbitlang$x$encoding$Decode$Uchar) >> 2, 0, 3
      );
      ((struct $$moonbitlang$x$encoding$Decode$Uchar*)Uchar$1815)->$0
      = _u$641;
      moonbit_incref($$moonbitlang$x$encoding$Decoder$$decode_utf_16le$clo);
      return $$moonbitlang$x$encoding$Decoder$$ret(
               self$642,
                 $$moonbitlang$x$encoding$Decoder$$decode_utf_16le$clo,
                 Uchar$1815
             );
      break;
    }
    
    case 1: {
      struct $$moonbitlang$x$encoding$UTF16Decode$UTF16Malformed* _UTF16Malformed$643 =
        (struct $$moonbitlang$x$encoding$UTF16Decode$UTF16Malformed*)v$639;
      moonbit_bytes_t _field$2003 = _UTF16Malformed$643->$0;
      int32_t _cnt$2135 = Moonbit_object_header(_UTF16Malformed$643)->rc;
      moonbit_bytes_t _s$644;
      void* Malformed$1814;
      if (_cnt$2135 > 1) {
        int32_t _new_cnt$2136 = _cnt$2135 - 1;
        Moonbit_object_header(_UTF16Malformed$643)->rc = _new_cnt$2136;
        moonbit_incref(_field$2003);
      } else if (_cnt$2135 == 1) {
        moonbit_free(_UTF16Malformed$643);
      }
      _s$644 = _field$2003;
      Malformed$1814
      = (void*)moonbit_malloc(
          sizeof(struct $$moonbitlang$x$encoding$Decode$Malformed)
        );
      Moonbit_object_header(Malformed$1814)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $$moonbitlang$x$encoding$Decode$Malformed, $0) >> 2,
          1,
          2
      );
      ((struct $$moonbitlang$x$encoding$Decode$Malformed*)Malformed$1814)->$0
      = _s$644;
      moonbit_incref($$moonbitlang$x$encoding$Decoder$$decode_utf_16le$clo);
      return $$moonbitlang$x$encoding$Decoder$$ret(
               self$642,
                 $$moonbitlang$x$encoding$Decoder$$decode_utf_16le$clo,
                 Malformed$1814
             );
      break;
    }
    default: {
      struct $$moonbitlang$x$encoding$UTF16Decode$Hi* _Hi$645 =
        (struct $$moonbitlang$x$encoding$UTF16Decode$Hi*)v$639;
      int32_t _field$2007 = _Hi$645->$0;
      int32_t _hi$646;
      moonbit_bytes_t _field$2006;
      moonbit_bytes_t i$1813;
      int32_t _tmp$2005;
      int32_t _tmp$1811;
      int32_t i_pos$1812;
      int32_t rem$647;
      moonbit_decref(_Hi$645);
      _hi$646 = _field$2007;
      _field$2006 = self$642->$0;
      i$1813 = _field$2006;
      _tmp$2005 = Moonbit_array_length(i$1813);
      _tmp$1811 = _tmp$2005;
      i_pos$1812 = self$642->$1;
      rem$647 = _tmp$1811 - i_pos$1812;
      if (rem$647 < 2) {
        struct $$3c$Int$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _func$648;
        struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _tmp$1806;
        moonbit_incref(self$642);
        $$moonbitlang$x$encoding$Decoder$$t_need(self$642, 2);
        moonbit_incref($moonbitlang$x$encoding$t_decode_utf_16le_lo$clo);
        _func$648
        = $moonbitlang$x$encoding$curry$1(
          $moonbitlang$x$encoding$t_decode_utf_16le_lo$clo
        );
        _tmp$1806 = _func$648->code(_func$648, _hi$646);
        return $moonbitlang$x$encoding$t_fill(_tmp$1806, self$642);
      } else {
        int32_t j$649 = self$642->$1;
        moonbit_bytes_t _field$2004 = self$642->$0;
        moonbit_bytes_t i$1809 = _field$2004;
        int32_t _tmp$1810 = j$649 + 1;
        void* dcd$650;
        moonbit_incref(i$1809);
        dcd$650
        = $moonbitlang$x$encoding$r_utf_16_lo(
          _hi$646, i$1809, _tmp$1810, j$649
        );
        switch (Moonbit_object_tag(dcd$650)) {
          case 3: {
            int32_t i_pos$1808 = self$642->$1;
            int32_t _tmp$1807 = i_pos$1808 + 2;
            self$642->$1 = _tmp$1807;
            break;
          }
          default:
            break;
        }
        moonbit_incref($$moonbitlang$x$encoding$Decoder$$decode_utf_16le$clo);
        return $$moonbitlang$x$encoding$Decoder$$ret(
                 self$642,
                   $$moonbitlang$x$encoding$Decoder$$decode_utf_16le$clo,
                   dcd$650
               );
      }
      break;
    }
  }
}

void* $$moonbitlang$x$encoding$Decoder$$decode_utf_16le(
  struct $$moonbitlang$x$encoding$Decoder* self$637
) {
  moonbit_bytes_t _field$2010 = self$637->$0;
  moonbit_bytes_t i$1805 = _field$2010;
  int32_t _tmp$2009 = Moonbit_array_length(i$1805);
  int32_t _tmp$1803 = _tmp$2009;
  int32_t i_pos$1804 = self$637->$1;
  int32_t rem$636 = _tmp$1803 - i_pos$1804;
  if (rem$636 <= 0) {
    moonbit_decref(self$637);
    return (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  } else if (rem$636 < 2) {
    moonbit_incref(self$637);
    $$moonbitlang$x$encoding$Decoder$$t_need(self$637, 2);
    moonbit_incref($$moonbitlang$x$encoding$Decoder$$t_decode_utf_16le$clo);
    return $moonbitlang$x$encoding$t_fill(
             $$moonbitlang$x$encoding$Decoder$$t_decode_utf_16le$clo,
               self$637
           );
  } else {
    int32_t j$638 = self$637->$1;
    int32_t i_pos$1799 = self$637->$1;
    int32_t _tmp$1798 = i_pos$1799 + 2;
    moonbit_bytes_t _field$2008;
    moonbit_bytes_t i$1801;
    int32_t _tmp$1802;
    void* _tmp$1800;
    self$637->$1 = _tmp$1798;
    _field$2008 = self$637->$0;
    i$1801 = _field$2008;
    _tmp$1802 = j$638 + 1;
    moonbit_incref(i$1801);
    _tmp$1800 = $moonbitlang$x$encoding$r_utf_16(i$1801, _tmp$1802, j$638);
    return $$moonbitlang$x$encoding$Decoder$$decode_utf_16le_lo(
             self$637, _tmp$1800
           );
  }
}

void* $$moonbitlang$x$encoding$Decoder$$t_decode_utf_16be(
  struct $$moonbitlang$x$encoding$Decoder* self$635
) {
  int32_t t_len$1791 = self$635->$3;
  int32_t t_need$1792 = self$635->$4;
  if (t_len$1791 < t_need$1792) {
    moonbit_bytes_t _field$2011 = self$635->$2;
    moonbit_bytes_t t$1794 = _field$2011;
    int32_t t_len$1795 = self$635->$3;
    void* _tmp$1793;
    moonbit_incref(t$1794);
    _tmp$1793 = $moonbitlang$x$encoding$malformed(t$1794, 0, t_len$1795);
    moonbit_incref($$moonbitlang$x$encoding$Decoder$$decode_utf_16be$clo);
    return $$moonbitlang$x$encoding$Decoder$$ret(
             self$635,
               $$moonbitlang$x$encoding$Decoder$$decode_utf_16be$clo,
               _tmp$1793
           );
  } else {
    moonbit_bytes_t _field$2012 = self$635->$2;
    moonbit_bytes_t t$1797 = _field$2012;
    void* _tmp$1796;
    moonbit_incref(t$1797);
    _tmp$1796 = $moonbitlang$x$encoding$r_utf_16(t$1797, 0, 1);
    return $$moonbitlang$x$encoding$Decoder$$decode_utf_16be_lo(
             self$635, _tmp$1796
           );
  }
}

void* $moonbitlang$x$encoding$t_decode_utf_16be_lo(
  int32_t hi$634,
  struct $$moonbitlang$x$encoding$Decoder* self$633
) {
  int32_t t_len$1784 = self$633->$3;
  int32_t t_need$1785 = self$633->$4;
  if (t_len$1784 < t_need$1785) {
    moonbit_bytes_t _field$2013 = self$633->$2;
    moonbit_bytes_t t$1787 = _field$2013;
    int32_t t_len$1788 = self$633->$3;
    void* _tmp$1786;
    moonbit_incref(t$1787);
    _tmp$1786
    = $moonbitlang$x$encoding$malformed_pair(
      1, hi$634, t$1787, 0, t_len$1788
    );
    moonbit_incref($$moonbitlang$x$encoding$Decoder$$decode_utf_16be$clo);
    return $$moonbitlang$x$encoding$Decoder$$ret(
             self$633,
               $$moonbitlang$x$encoding$Decoder$$decode_utf_16be$clo,
               _tmp$1786
           );
  } else {
    moonbit_bytes_t _field$2014 = self$633->$2;
    moonbit_bytes_t t$1790 = _field$2014;
    void* _tmp$1789;
    moonbit_incref(t$1790);
    _tmp$1789 = $moonbitlang$x$encoding$r_utf_16_lo(hi$634, t$1790, 0, 1);
    moonbit_incref($$moonbitlang$x$encoding$Decoder$$decode_utf_16be$clo);
    return $$moonbitlang$x$encoding$Decoder$$ret(
             self$633,
               $$moonbitlang$x$encoding$Decoder$$decode_utf_16be$clo,
               _tmp$1789
           );
  }
}

void* $$moonbitlang$x$encoding$Decoder$$decode_utf_16be_lo(
  struct $$moonbitlang$x$encoding$Decoder* self$624,
  void* decode$621
) {
  switch (Moonbit_object_tag(decode$621)) {
    case 2: {
      struct $$moonbitlang$x$encoding$UTF16Decode$UTF16Uchar* _UTF16Uchar$622 =
        (struct $$moonbitlang$x$encoding$UTF16Decode$UTF16Uchar*)decode$621;
      int32_t _field$2015 = _UTF16Uchar$622->$0;
      int32_t _x$623;
      void* Uchar$1783;
      moonbit_decref(_UTF16Uchar$622);
      _x$623 = _field$2015;
      Uchar$1783
      = (void*)moonbit_malloc(
          sizeof(struct $$moonbitlang$x$encoding$Decode$Uchar)
        );
      Moonbit_object_header(Uchar$1783)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$moonbitlang$x$encoding$Decode$Uchar) >> 2, 0, 3
      );
      ((struct $$moonbitlang$x$encoding$Decode$Uchar*)Uchar$1783)->$0
      = _x$623;
      moonbit_incref($$moonbitlang$x$encoding$Decoder$$decode_utf_16be$clo);
      return $$moonbitlang$x$encoding$Decoder$$ret(
               self$624,
                 $$moonbitlang$x$encoding$Decoder$$decode_utf_16be$clo,
                 Uchar$1783
             );
      break;
    }
    
    case 1: {
      struct $$moonbitlang$x$encoding$UTF16Decode$UTF16Malformed* _UTF16Malformed$625 =
        (struct $$moonbitlang$x$encoding$UTF16Decode$UTF16Malformed*)decode$621;
      moonbit_bytes_t _field$2016 = _UTF16Malformed$625->$0;
      int32_t _cnt$2137 = Moonbit_object_header(_UTF16Malformed$625)->rc;
      moonbit_bytes_t _x$626;
      void* Malformed$1782;
      if (_cnt$2137 > 1) {
        int32_t _new_cnt$2138 = _cnt$2137 - 1;
        Moonbit_object_header(_UTF16Malformed$625)->rc = _new_cnt$2138;
        moonbit_incref(_field$2016);
      } else if (_cnt$2137 == 1) {
        moonbit_free(_UTF16Malformed$625);
      }
      _x$626 = _field$2016;
      Malformed$1782
      = (void*)moonbit_malloc(
          sizeof(struct $$moonbitlang$x$encoding$Decode$Malformed)
        );
      Moonbit_object_header(Malformed$1782)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $$moonbitlang$x$encoding$Decode$Malformed, $0) >> 2,
          1,
          2
      );
      ((struct $$moonbitlang$x$encoding$Decode$Malformed*)Malformed$1782)->$0
      = _x$626;
      moonbit_incref($$moonbitlang$x$encoding$Decoder$$decode_utf_16be$clo);
      return $$moonbitlang$x$encoding$Decoder$$ret(
               self$624,
                 $$moonbitlang$x$encoding$Decoder$$decode_utf_16be$clo,
                 Malformed$1782
             );
      break;
    }
    default: {
      struct $$moonbitlang$x$encoding$UTF16Decode$Hi* _Hi$627 =
        (struct $$moonbitlang$x$encoding$UTF16Decode$Hi*)decode$621;
      int32_t _field$2020 = _Hi$627->$0;
      int32_t _hi$628;
      moonbit_bytes_t _field$2019;
      moonbit_bytes_t i$1781;
      int32_t _tmp$2018;
      int32_t _tmp$1779;
      int32_t i_pos$1780;
      int32_t rem$629;
      moonbit_decref(_Hi$627);
      _hi$628 = _field$2020;
      _field$2019 = self$624->$0;
      i$1781 = _field$2019;
      _tmp$2018 = Moonbit_array_length(i$1781);
      _tmp$1779 = _tmp$2018;
      i_pos$1780 = self$624->$1;
      rem$629 = _tmp$1779 - i_pos$1780;
      if (rem$629 < 2) {
        struct $$3c$Int$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _func$630;
        struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _tmp$1774;
        moonbit_incref(self$624);
        $$moonbitlang$x$encoding$Decoder$$t_need(self$624, 2);
        moonbit_incref($moonbitlang$x$encoding$t_decode_utf_16be_lo$clo);
        _func$630
        = $moonbitlang$x$encoding$curry$1(
          $moonbitlang$x$encoding$t_decode_utf_16be_lo$clo
        );
        _tmp$1774 = _func$630->code(_func$630, _hi$628);
        return $moonbitlang$x$encoding$t_fill(_tmp$1774, self$624);
      } else {
        int32_t j$631 = self$624->$1;
        moonbit_bytes_t _field$2017 = self$624->$0;
        moonbit_bytes_t i$1777 = _field$2017;
        int32_t _tmp$1778 = j$631 + 1;
        void* dcd$632;
        moonbit_incref(i$1777);
        dcd$632
        = $moonbitlang$x$encoding$r_utf_16_lo(
          _hi$628, i$1777, j$631, _tmp$1778
        );
        switch (Moonbit_object_tag(dcd$632)) {
          case 3: {
            int32_t i_pos$1776 = self$624->$1;
            int32_t _tmp$1775 = i_pos$1776 + 2;
            self$624->$1 = _tmp$1775;
            break;
          }
          default:
            break;
        }
        moonbit_incref($$moonbitlang$x$encoding$Decoder$$decode_utf_16be$clo);
        return $$moonbitlang$x$encoding$Decoder$$ret(
                 self$624,
                   $$moonbitlang$x$encoding$Decoder$$decode_utf_16be$clo,
                   dcd$632
               );
      }
      break;
    }
  }
}

void* $$moonbitlang$x$encoding$Decoder$$decode_utf_16be(
  struct $$moonbitlang$x$encoding$Decoder* self$619
) {
  moonbit_bytes_t _field$2023 = self$619->$0;
  moonbit_bytes_t i$1773 = _field$2023;
  int32_t _tmp$2022 = Moonbit_array_length(i$1773);
  int32_t _tmp$1771 = _tmp$2022;
  int32_t i_pos$1772 = self$619->$1;
  int32_t rem$618 = _tmp$1771 - i_pos$1772;
  if (rem$618 <= 0) {
    moonbit_decref(self$619);
    return (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  } else if (rem$618 < 2) {
    moonbit_incref(self$619);
    $$moonbitlang$x$encoding$Decoder$$t_need(self$619, 2);
    moonbit_incref($$moonbitlang$x$encoding$Decoder$$t_decode_utf_16be$clo);
    return $moonbitlang$x$encoding$t_fill(
             $$moonbitlang$x$encoding$Decoder$$t_decode_utf_16be$clo,
               self$619
           );
  } else {
    int32_t j$620 = self$619->$1;
    int32_t i_pos$1767 = self$619->$1;
    int32_t _tmp$1766 = i_pos$1767 + 2;
    moonbit_bytes_t _field$2021;
    moonbit_bytes_t i$1769;
    int32_t _tmp$1770;
    void* _tmp$1768;
    self$619->$1 = _tmp$1766;
    _field$2021 = self$619->$0;
    i$1769 = _field$2021;
    _tmp$1770 = j$620 + 1;
    moonbit_incref(i$1769);
    _tmp$1768 = $moonbitlang$x$encoding$r_utf_16(i$1769, j$620, _tmp$1770);
    return $$moonbitlang$x$encoding$Decoder$$decode_utf_16be_lo(
             self$619, _tmp$1768
           );
  }
}

int32_t $$moonbitlang$x$encoding$Decoder$$t_need(
  struct $$moonbitlang$x$encoding$Decoder* self$616,
  int32_t need$617
) {
  self$616->$3 = 0;
  self$616->$4 = need$617;
  moonbit_decref(self$616);
  return 0;
}

void* $moonbitlang$x$encoding$r_utf_16_lo(
  int32_t hi$615,
  moonbit_bytes_t bytes$610,
  int32_t offset0$611,
  int32_t offset1$613
) {
  int32_t _tmp$1765;
  int32_t b0$609;
  int32_t _tmp$1764;
  int32_t b1$612;
  int32_t _tmp$1763;
  int32_t lo$614;
  if (offset0$611 < 0 || offset0$611 >= Moonbit_array_length(bytes$610)) {
    moonbit_panic();
  }
  _tmp$1765 = (int32_t)bytes$610[offset0$611];
  b0$609 = (int32_t)_tmp$1765;
  if (offset1$613 < 0 || offset1$613 >= Moonbit_array_length(bytes$610)) {
    moonbit_panic();
  }
  _tmp$1764 = (int32_t)bytes$610[offset1$613];
  b1$612 = (int32_t)_tmp$1764;
  _tmp$1763 = b0$609 << 8;
  lo$614 = _tmp$1763 | b1$612;
  if (lo$614 < 56320 || lo$614 > 57343) {
    int32_t _tmp$1755;
    int32_t _tmp$2024;
    int32_t _tmp$1756;
    moonbit_bytes_t _tmp$1754;
    void* _block$2183;
    if (offset0$611 < 0 || offset0$611 >= Moonbit_array_length(bytes$610)) {
      moonbit_panic();
    }
    _tmp$1755 = (int32_t)bytes$610[offset0$611];
    if (offset1$613 < 0 || offset1$613 >= Moonbit_array_length(bytes$610)) {
      moonbit_panic();
    }
    _tmp$2024 = (int32_t)bytes$610[offset1$613];
    moonbit_decref(bytes$610);
    _tmp$1756 = _tmp$2024;
    _tmp$1754 = (moonbit_bytes_t)moonbit_make_bytes_raw(2);
    _tmp$1754[0] = _tmp$1755;
    _tmp$1754[1] = _tmp$1756;
    _block$2183
    = (void*)moonbit_malloc(
        sizeof(struct $$moonbitlang$x$encoding$Decode$Malformed)
      );
    Moonbit_object_header(_block$2183)->meta
    = Moonbit_make_regular_object_header(
      offsetof(struct $$moonbitlang$x$encoding$Decode$Malformed, $0) >> 2,
        1,
        2
    );
    ((struct $$moonbitlang$x$encoding$Decode$Malformed*)_block$2183)->$0
    = _tmp$1754;
    return _block$2183;
  } else {
    int32_t _tmp$1762;
    int32_t _tmp$1759;
    int32_t _tmp$1761;
    int32_t _tmp$1760;
    int32_t _tmp$1758;
    int32_t _tmp$1757;
    void* _block$2184;
    moonbit_decref(bytes$610);
    _tmp$1762 = hi$615 & 1023;
    _tmp$1759 = _tmp$1762 << 10;
    _tmp$1761 = lo$614 & 1023;
    _tmp$1760 = _tmp$1761 + 65536;
    _tmp$1758 = _tmp$1759 | _tmp$1760;
    _tmp$1757 = _tmp$1758;
    _block$2184
    = (void*)moonbit_malloc(
        sizeof(struct $$moonbitlang$x$encoding$Decode$Uchar)
      );
    Moonbit_object_header(_block$2184)->meta
    = Moonbit_make_regular_object_header(
      sizeof(struct $$moonbitlang$x$encoding$Decode$Uchar) >> 2, 0, 3
    );
    ((struct $$moonbitlang$x$encoding$Decode$Uchar*)_block$2184)->$0
    = _tmp$1757;
    return _block$2184;
  }
}

void* $moonbitlang$x$encoding$t_fill(
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* k$605,
  struct $$moonbitlang$x$encoding$Decoder* decoder$603
) {
  moonbit_bytes_t _field$2026 = decoder$603->$0;
  moonbit_bytes_t i$1753 = _field$2026;
  int32_t _tmp$2025 = Moonbit_array_length(i$1753);
  int32_t _tmp$1751 = _tmp$2025;
  int32_t i_pos$1752 = decoder$603->$1;
  int32_t rem$602 = _tmp$1751 - i_pos$1752;
  if (rem$602 < 0) {
    struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _func$604 =
      k$605;
    return _func$604->code(_func$604, decoder$603);
  } else {
    int32_t t_need$1749 = decoder$603->$4;
    int32_t t_len$1750 = decoder$603->$3;
    int32_t need$606 = t_need$1749 - t_len$1750;
    if (rem$602 < need$606) {
      struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _func$607;
      struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _tmp$1748;
      moonbit_incref(decoder$603);
      $moonbitlang$x$encoding$t_fill$blit$7c$69(decoder$603, rem$602);
      moonbit_incref($moonbitlang$x$encoding$t_fill$clo);
      _func$607
      = $moonbitlang$x$encoding$curry$0(
        $moonbitlang$x$encoding$t_fill$clo
      );
      _tmp$1748 = _func$607->code(_func$607, k$605);
      return $$moonbitlang$x$encoding$Decoder$$refill(decoder$603, _tmp$1748);
    } else {
      struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _func$608;
      moonbit_incref(decoder$603);
      $moonbitlang$x$encoding$t_fill$blit$7c$69(decoder$603, need$606);
      _func$608 = k$605;
      return _func$608->code(_func$608, decoder$603);
    }
  }
}

int32_t $moonbitlang$x$encoding$t_fill$blit$7c$69(
  struct $$moonbitlang$x$encoding$Decoder* decoder$600,
  int32_t l$601
) {
  moonbit_bytes_t _field$2028 = decoder$600->$0;
  moonbit_bytes_t i$1740 = _field$2028;
  moonbit_bytes_t _field$2027 = decoder$600->$2;
  moonbit_bytes_t t$1741 = _field$2027;
  int32_t i_pos$1742 = decoder$600->$1;
  int32_t t_len$1743 = decoder$600->$3;
  int32_t i_pos$1745;
  int32_t _tmp$1744;
  int32_t t_len$1747;
  int32_t _tmp$1746;
  moonbit_incref(t$1741);
  moonbit_incref(i$1740);
  $FixedArray$$blit_to$inner$0(i$1740, t$1741, l$601, i_pos$1742, t_len$1743);
  i_pos$1745 = decoder$600->$1;
  _tmp$1744 = i_pos$1745 + l$601;
  decoder$600->$1 = _tmp$1744;
  t_len$1747 = decoder$600->$3;
  _tmp$1746 = t_len$1747 + l$601;
  decoder$600->$3 = _tmp$1746;
  moonbit_decref(decoder$600);
  return 0;
}

void* $$moonbitlang$x$encoding$Decoder$$refill(
  struct $$moonbitlang$x$encoding$Decoder* self$597,
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* k$598
) {
  moonbit_bytes_t _field$2030;
  moonbit_bytes_t _bind$599;
  moonbit_bytes_t _tmp$1738;
  int32_t _tmp$2029;
  int32_t _tmp$1739;
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ _tmp$1737;
  moonbit_bytes_t _tmp$1736;
  void* Refill$1735;
  moonbit_incref(self$597);
  $$moonbitlang$x$encoding$Decoder$$eoi(self$597);
  _field$2030 = self$597->$2;
  _bind$599 = _field$2030;
  moonbit_incref(_bind$599);
  moonbit_incref(_bind$599);
  _tmp$1738 = _bind$599;
  _tmp$2029 = Moonbit_array_length(_bind$599);
  moonbit_decref(_bind$599);
  _tmp$1739 = _tmp$2029;
  _tmp$1737
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$){
    0, _tmp$1739, _tmp$1738
  };
  _tmp$1736 = $Bytes$$from_array(_tmp$1737);
  Refill$1735
  = (void*)moonbit_malloc(
      sizeof(struct $$moonbitlang$x$encoding$Decode$Refill)
    );
  Moonbit_object_header(Refill$1735)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$x$encoding$Decode$Refill, $0) >> 2, 1, 1
  );
  ((struct $$moonbitlang$x$encoding$Decode$Refill*)Refill$1735)->$0
  = _tmp$1736;
  return $$moonbitlang$x$encoding$Decoder$$ret(self$597, k$598, Refill$1735);
}

void* $$moonbitlang$x$encoding$Decoder$$ret(
  struct $$moonbitlang$x$encoding$Decoder* self$594,
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* k$595,
  void* v$596
) {
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _old$2031 =
    self$594->$5;
  moonbit_decref(_old$2031);
  self$594->$5 = k$595;
  moonbit_decref(self$594);
  return v$596;
}

int32_t $$moonbitlang$x$encoding$Decoder$$eoi(
  struct $$moonbitlang$x$encoding$Decoder* self$593
) {
  moonbit_bytes_t _tmp$1734 =
    $$moonbitlang$core$builtin$Default$$FixedArray$$default$0();
  moonbit_bytes_t _old$2032 = self$593->$0;
  moonbit_decref(_old$2032);
  self$593->$0 = _tmp$1734;
  moonbit_decref(self$593);
  return 0;
}

int32_t $$moonbitlang$x$encoding$Decoder$$i_rem(
  struct $$moonbitlang$x$encoding$Decoder* self$592
) {
  moonbit_bytes_t _field$2035 = self$592->$0;
  moonbit_bytes_t i$1733 = _field$2035;
  int32_t _tmp$2034 = Moonbit_array_length(i$1733);
  int32_t _tmp$1731 = _tmp$2034;
  int32_t _field$2033 = self$592->$1;
  int32_t i_pos$1732;
  moonbit_decref(self$592);
  i_pos$1732 = _field$2033;
  return _tmp$1731 - i_pos$1732;
}

struct $$3c$Int$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $moonbitlang$x$encoding$curry$1(
  struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* f$591
) {
  struct $$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$5$2d$cap* _closure$2185 =
    (struct $$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$5$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$5$2d$cap
      )
    );
  Moonbit_object_header(_closure$2185)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$5$2d$cap,
        $0
    )
    >> 2,
      1,
      0
  );
  _closure$2185->code
  = &$$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$5;
  _closure$2185->$0 = f$591;
  return (struct $$3c$Int$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*)_closure$2185;
}

struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$5(
  struct $$3c$Int$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1727,
  int32_t x$589
) {
  struct $$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$5$2d$cap* _casted_env$1728 =
    (struct $$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$5$2d$cap*)_env$1727;
  struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _field$2036 =
    _casted_env$1728->$0;
  int32_t _cnt$2139 = Moonbit_object_header(_casted_env$1728)->rc;
  struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* f$591;
  struct $$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$6$2d$cap* _closure$2186;
  if (_cnt$2139 > 1) {
    int32_t _new_cnt$2140 = _cnt$2139 - 1;
    Moonbit_object_header(_casted_env$1728)->rc = _new_cnt$2140;
    moonbit_incref(_field$2036);
  } else if (_cnt$2139 == 1) {
    moonbit_free(_casted_env$1728);
  }
  f$591 = _field$2036;
  _closure$2186
  = (struct $$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$6$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$6$2d$cap
      )
    );
  Moonbit_object_header(_closure$2186)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$6$2d$cap,
        $0
    )
    >> 2,
      1,
      0
  );
  _closure$2186->code
  = &$$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$6;
  _closure$2186->$0 = f$591;
  _closure$2186->$1 = x$589;
  return (struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*)_closure$2186;
}

void* $$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$6(
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1729,
  struct $$moonbitlang$x$encoding$Decoder* y$590
) {
  struct $$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$6$2d$cap* _casted_env$1730 =
    (struct $$moonbitlang$x$encoding$curry$7c$Int$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$6$2d$cap*)_env$1729;
  int32_t x$589 = _casted_env$1730->$1;
  struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _field$2037 =
    _casted_env$1730->$0;
  int32_t _cnt$2141 = Moonbit_object_header(_casted_env$1730)->rc;
  struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* f$591;
  if (_cnt$2141 > 1) {
    int32_t _new_cnt$2142 = _cnt$2141 - 1;
    Moonbit_object_header(_casted_env$1730)->rc = _new_cnt$2142;
    moonbit_incref(_field$2037);
  } else if (_cnt$2141 == 1) {
    moonbit_free(_casted_env$1730);
  }
  f$591 = _field$2037;
  return f$591->code(f$591, x$589, y$590);
}

struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $moonbitlang$x$encoding$curry$0(
  struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* f$588
) {
  struct $$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$3$2d$cap* _closure$2187 =
    (struct $$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$3$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$3$2d$cap
      )
    );
  Moonbit_object_header(_closure$2187)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$3$2d$cap,
        $0
    )
    >> 2,
      1,
      0
  );
  _closure$2187->code
  = &$$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$3;
  _closure$2187->$0 = f$588;
  return (struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*)_closure$2187;
}

struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* $$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$3(
  struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$3e$$3d$$3e$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1723,
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* x$586
) {
  struct $$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$3$2d$cap* _casted_env$1724 =
    (struct $$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$3$2d$cap*)_env$1723;
  struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _field$2038 =
    _casted_env$1724->$0;
  int32_t _cnt$2143 = Moonbit_object_header(_casted_env$1724)->rc;
  struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* f$588;
  struct $$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$4$2d$cap* _closure$2188;
  if (_cnt$2143 > 1) {
    int32_t _new_cnt$2144 = _cnt$2143 - 1;
    Moonbit_object_header(_casted_env$1724)->rc = _new_cnt$2144;
    moonbit_incref(_field$2038);
  } else if (_cnt$2143 == 1) {
    moonbit_free(_casted_env$1724);
  }
  f$588 = _field$2038;
  _closure$2188
  = (struct $$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$4$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$4$2d$cap
      )
    );
  Moonbit_object_header(_closure$2188)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$4$2d$cap,
        $0
    )
    >> 2,
      2,
      0
  );
  _closure$2188->code
  = &$$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$4;
  _closure$2188->$0 = f$588;
  _closure$2188->$1 = x$586;
  return (struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*)_closure$2188;
}

void* $$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$4(
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _env$1725,
  struct $$moonbitlang$x$encoding$Decoder* y$587
) {
  struct $$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$4$2d$cap* _casted_env$1726 =
    (struct $$moonbitlang$x$encoding$curry$7c$$moonbitlang$x$encoding$Cont$2b$$moonbitlang$x$encoding$Decoder$2b$$moonbitlang$x$encoding$Decode$7c$$fn$4$2d$cap*)_env$1725;
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _field$2040 =
    _casted_env$1726->$1;
  struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* x$586 =
    _field$2040;
  struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* _field$2039 =
    _casted_env$1726->$0;
  int32_t _cnt$2145 = Moonbit_object_header(_casted_env$1726)->rc;
  struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode* f$588;
  if (_cnt$2145 > 1) {
    int32_t _new_cnt$2146 = _cnt$2145 - 1;
    Moonbit_object_header(_casted_env$1726)->rc = _new_cnt$2146;
    moonbit_incref(x$586);
    moonbit_incref(_field$2039);
  } else if (_cnt$2145 == 1) {
    moonbit_free(_casted_env$1726);
  }
  f$588 = _field$2039;
  return f$588->code(f$588, x$586, y$587);
}

void* $moonbitlang$x$encoding$r_utf_16(
  moonbit_bytes_t bytes$581,
  int32_t offset0$582,
  int32_t offset1$584
) {
  int32_t _tmp$1722;
  int32_t b0$580;
  int32_t _tmp$1721;
  int32_t b1$583;
  int32_t _tmp$1720;
  int32_t u$585;
  if (offset0$582 < 0 || offset0$582 >= Moonbit_array_length(bytes$581)) {
    moonbit_panic();
  }
  _tmp$1722 = (int32_t)bytes$581[offset0$582];
  b0$580 = (int32_t)_tmp$1722;
  if (offset1$584 < 0 || offset1$584 >= Moonbit_array_length(bytes$581)) {
    moonbit_panic();
  }
  _tmp$1721 = (int32_t)bytes$581[offset1$584];
  b1$583 = (int32_t)_tmp$1721;
  _tmp$1720 = b0$580 << 8;
  u$585 = _tmp$1720 | b1$583;
  if (u$585 < 55296 || u$585 > 57343) {
    int32_t _tmp$1717;
    void* _block$2189;
    moonbit_decref(bytes$581);
    _tmp$1717 = u$585;
    _block$2189
    = (void*)moonbit_malloc(
        sizeof(struct $$moonbitlang$x$encoding$UTF16Decode$UTF16Uchar)
      );
    Moonbit_object_header(_block$2189)->meta
    = Moonbit_make_regular_object_header(
      sizeof(struct $$moonbitlang$x$encoding$UTF16Decode$UTF16Uchar) >> 2,
        0,
        2
    );
    ((struct $$moonbitlang$x$encoding$UTF16Decode$UTF16Uchar*)_block$2189)->$0
    = _tmp$1717;
    return _block$2189;
  } else if (u$585 > 56319) {
    int32_t _tmp$1719;
    moonbit_bytes_t _tmp$1718;
    void* _block$2190;
    if (offset0$582 > offset1$584) {
      _tmp$1719 = offset1$584;
    } else {
      _tmp$1719 = offset0$582;
    }
    _tmp$1718 = $moonbitlang$x$encoding$slice(bytes$581, _tmp$1719, 2);
    _block$2190
    = (void*)moonbit_malloc(
        sizeof(struct $$moonbitlang$x$encoding$UTF16Decode$UTF16Malformed)
      );
    Moonbit_object_header(_block$2190)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$x$encoding$UTF16Decode$UTF16Malformed, $0
      )
      >> 2,
        1,
        1
    );
    ((struct $$moonbitlang$x$encoding$UTF16Decode$UTF16Malformed*)_block$2190)->$0
    = _tmp$1718;
    return _block$2190;
  } else {
    void* _block$2191;
    moonbit_decref(bytes$581);
    _block$2191
    = (void*)moonbit_malloc(
        sizeof(struct $$moonbitlang$x$encoding$UTF16Decode$Hi)
      );
    Moonbit_object_header(_block$2191)->meta
    = Moonbit_make_regular_object_header(
      sizeof(struct $$moonbitlang$x$encoding$UTF16Decode$Hi) >> 2, 0, 0
    );
    ((struct $$moonbitlang$x$encoding$UTF16Decode$Hi*)_block$2191)->$0
    = u$585;
    return _block$2191;
  }
}

moonbit_bytes_t $moonbitlang$x$encoding$encode(
  int32_t encoding$572,
  moonbit_string_t src$571
) {
  int32_t _tmp$1714;
  int32_t _tmp$1713;
  moonbit_bytes_t arr$570;
  int32_t _tmp$1712;
  int32_t _tmp$1716;
  int32_t _tmp$1715;
  struct $$moonbitlang$core$buffer$Buffer* new_buf$573;
  struct $$3c$$moonbitlang$core$buffer$Buffer$2a$Char$3e$$3d$$3e$Unit* write$574;
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* _it$575;
  switch (encoding$572) {
    case 1: {
      goto $join$569;
      break;
    }
    
    case 2: {
      goto $join$569;
      break;
    }
    default:
      break;
  }
  goto $joinlet$2192;
  $join$569:;
  _tmp$1714 = Moonbit_array_length(src$571);
  _tmp$1713 = _tmp$1714 * 2;
  arr$570 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1713, 0);
  _tmp$1712 = Moonbit_array_length(src$571);
  moonbit_incref(arr$570);
  $FixedArray$$blit_from_string(arr$570, 0, src$571, 0, _tmp$1712);
  return arr$570;
  $joinlet$2192:;
  _tmp$1716 = Moonbit_array_length(src$571);
  _tmp$1715 = _tmp$1716 * 4;
  new_buf$573 = $moonbitlang$core$buffer$new$inner(_tmp$1715);
  switch (encoding$572) {
    case 0: {
      moonbit_incref($moonbitlang$x$encoding$write_utf8_char$clo);
      write$574 = $moonbitlang$x$encoding$write_utf8_char$clo;
      break;
    }
    
    case 3: {
      moonbit_incref($moonbitlang$x$encoding$write_utf16be_char$clo);
      write$574 = $moonbitlang$x$encoding$write_utf16be_char$clo;
      break;
    }
    default: {
      write$574
      = $moonbitlang$core$builtin$abort$1(
        (moonbit_string_t)moonbit_string_literal_5.data,
          (moonbit_string_t)moonbit_string_literal_6.data
      );
      break;
    }
  }
  _it$575 = $String$$iter(src$571);
  while (1) {
    int32_t _bind$576;
    moonbit_incref(_it$575);
    _bind$576 = $$moonbitlang$core$builtin$Iter$$next$0(_it$575);
    if (_bind$576 == -1) {
      moonbit_decref(_it$575);
      moonbit_decref(write$574);
    } else {
      int32_t _Some$577 = _bind$576;
      int32_t _char$578 = _Some$577;
      moonbit_incref(write$574);
      moonbit_incref(new_buf$573);
      write$574->code(write$574, new_buf$573, _char$578);
      continue;
    }
    break;
  }
  return $$moonbitlang$core$buffer$Buffer$$to_bytes(new_buf$573);
}

int32_t $moonbitlang$x$encoding$write_utf8_char(
  struct $$moonbitlang$core$buffer$Buffer* buf$559,
  int32_t value$557
) {
  int32_t _tmp$1711 = value$557;
  uint32_t code$556 = *(uint32_t*)&_tmp$1711;
  if (code$556 < 128u) {
    uint32_t _tmp$1710 = code$556 & 127u;
    uint32_t _p$931 = _tmp$1710 | 0u;
    int32_t _tmp$1709 = *(int32_t*)&_p$931;
    int32_t b0$558 = _tmp$1709 & 0xff;
    $$moonbitlang$core$buffer$Buffer$$write_byte(buf$559, b0$558);
  } else if (code$556 < 2048u) {
    uint32_t _tmp$1708 = code$556 >> 6;
    uint32_t _tmp$1707 = _tmp$1708 & 31u;
    uint32_t _p$934 = _tmp$1707 | 192u;
    int32_t _tmp$1706 = *(int32_t*)&_p$934;
    int32_t b0$560 = _tmp$1706 & 0xff;
    uint32_t _tmp$1705 = code$556 & 63u;
    uint32_t _p$937 = _tmp$1705 | 128u;
    int32_t _tmp$1704 = *(int32_t*)&_p$937;
    int32_t b1$561 = _tmp$1704 & 0xff;
    moonbit_incref(buf$559);
    $$moonbitlang$core$buffer$Buffer$$write_byte(buf$559, b0$560);
    $$moonbitlang$core$buffer$Buffer$$write_byte(buf$559, b1$561);
  } else if (code$556 < 65536u) {
    uint32_t _tmp$1703 = code$556 >> 12;
    uint32_t _tmp$1702 = _tmp$1703 & 15u;
    uint32_t _p$940 = _tmp$1702 | 224u;
    int32_t _tmp$1701 = *(int32_t*)&_p$940;
    int32_t b0$562 = _tmp$1701 & 0xff;
    uint32_t _tmp$1700 = code$556 >> 6;
    uint32_t _tmp$1699 = _tmp$1700 & 63u;
    uint32_t _p$943 = _tmp$1699 | 128u;
    int32_t _tmp$1698 = *(int32_t*)&_p$943;
    int32_t b1$563 = _tmp$1698 & 0xff;
    uint32_t _tmp$1697 = code$556 & 63u;
    uint32_t _p$946 = _tmp$1697 | 128u;
    int32_t _tmp$1696 = *(int32_t*)&_p$946;
    int32_t b2$564 = _tmp$1696 & 0xff;
    moonbit_incref(buf$559);
    $$moonbitlang$core$buffer$Buffer$$write_byte(buf$559, b0$562);
    moonbit_incref(buf$559);
    $$moonbitlang$core$buffer$Buffer$$write_byte(buf$559, b1$563);
    $$moonbitlang$core$buffer$Buffer$$write_byte(buf$559, b2$564);
  } else if (code$556 < 1114112u) {
    uint32_t _tmp$1695 = code$556 >> 18;
    uint32_t _tmp$1694 = _tmp$1695 & 7u;
    uint32_t _p$949 = _tmp$1694 | 240u;
    int32_t _tmp$1693 = *(int32_t*)&_p$949;
    int32_t b0$565 = _tmp$1693 & 0xff;
    uint32_t _tmp$1692 = code$556 >> 12;
    uint32_t _tmp$1691 = _tmp$1692 & 63u;
    uint32_t _p$952 = _tmp$1691 | 128u;
    int32_t _tmp$1690 = *(int32_t*)&_p$952;
    int32_t b1$566 = _tmp$1690 & 0xff;
    uint32_t _tmp$1689 = code$556 >> 6;
    uint32_t _tmp$1688 = _tmp$1689 & 63u;
    uint32_t _p$955 = _tmp$1688 | 128u;
    int32_t _tmp$1687 = *(int32_t*)&_p$955;
    int32_t b2$567 = _tmp$1687 & 0xff;
    uint32_t _tmp$1686 = code$556 & 63u;
    uint32_t _p$958 = _tmp$1686 | 128u;
    int32_t _tmp$1685 = *(int32_t*)&_p$958;
    int32_t b3$568 = _tmp$1685 & 0xff;
    moonbit_incref(buf$559);
    $$moonbitlang$core$buffer$Buffer$$write_byte(buf$559, b0$565);
    moonbit_incref(buf$559);
    $$moonbitlang$core$buffer$Buffer$$write_byte(buf$559, b1$566);
    moonbit_incref(buf$559);
    $$moonbitlang$core$buffer$Buffer$$write_byte(buf$559, b2$567);
    $$moonbitlang$core$buffer$Buffer$$write_byte(buf$559, b3$568);
  } else {
    moonbit_decref(buf$559);
    $moonbitlang$core$builtin$abort$0(
      (moonbit_string_t)moonbit_string_literal_7.data,
        (moonbit_string_t)moonbit_string_literal_8.data
    );
  }
  return 0;
}

int32_t $moonbitlang$x$encoding$write_utf16be_char(
  struct $$moonbitlang$core$buffer$Buffer* buf$548,
  int32_t value$545
) {
  int32_t _tmp$1684 = value$545;
  uint32_t code$544 = *(uint32_t*)&_tmp$1684;
  if (code$544 < 65536u) {
    uint32_t _p$911 = code$544 >> 8;
    int32_t _tmp$1677 = *(int32_t*)&_p$911;
    int32_t b0$546 = _tmp$1677 & 0xff;
    uint32_t _p$914 = code$544 & 255u;
    int32_t _tmp$1676 = *(int32_t*)&_p$914;
    int32_t b1$547 = _tmp$1676 & 0xff;
    moonbit_incref(buf$548);
    $$moonbitlang$core$buffer$Buffer$$write_byte(buf$548, b0$546);
    $$moonbitlang$core$buffer$Buffer$$write_byte(buf$548, b1$547);
  } else if (code$544 < 1114112u) {
    uint32_t hi$549 = code$544 - 65536u;
    uint32_t _tmp$1683 = hi$549 >> 10;
    uint32_t lo$550 = _tmp$1683 | 55296u;
    uint32_t _tmp$1682 = hi$549 & 1023u;
    uint32_t hi$551 = _tmp$1682 | 56320u;
    uint32_t _p$917 = lo$550 >> 8;
    int32_t _tmp$1681 = *(int32_t*)&_p$917;
    int32_t b0$552 = _tmp$1681 & 0xff;
    uint32_t _p$920 = lo$550 & 255u;
    int32_t _tmp$1680 = *(int32_t*)&_p$920;
    int32_t b1$553 = _tmp$1680 & 0xff;
    uint32_t _p$923 = hi$551 >> 8;
    int32_t _tmp$1679 = *(int32_t*)&_p$923;
    int32_t b2$554 = _tmp$1679 & 0xff;
    uint32_t _p$926 = hi$551 & 255u;
    int32_t _tmp$1678 = *(int32_t*)&_p$926;
    int32_t b3$555 = _tmp$1678 & 0xff;
    moonbit_incref(buf$548);
    $$moonbitlang$core$buffer$Buffer$$write_byte(buf$548, b0$552);
    moonbit_incref(buf$548);
    $$moonbitlang$core$buffer$Buffer$$write_byte(buf$548, b1$553);
    moonbit_incref(buf$548);
    $$moonbitlang$core$buffer$Buffer$$write_byte(buf$548, b2$554);
    $$moonbitlang$core$buffer$Buffer$$write_byte(buf$548, b3$555);
  } else {
    moonbit_decref(buf$548);
    $moonbitlang$core$builtin$abort$0(
      (moonbit_string_t)moonbit_string_literal_7.data,
        (moonbit_string_t)moonbit_string_literal_9.data
    );
  }
  return 0;
}

void* $moonbitlang$x$encoding$malformed_pair(
  int32_t be$543,
  int32_t hi$541,
  moonbit_bytes_t bytes$535,
  int32_t offset$536,
  int32_t length$534
) {
  moonbit_bytes_t bs1$533 =
    (moonbit_bytes_t)moonbit_make_bytes(length$534, 0);
  moonbit_bytes_t bs0$537;
  int32_t j0$539;
  int32_t j1$540;
  int32_t _tmp$1661;
  int32_t _tmp$1660;
  int32_t _tmp$1663;
  int32_t _tmp$1662;
  int32_t _tmp$1674;
  int32_t _tmp$1675;
  int32_t _tmp$1673;
  struct $$moonbitlang$core$buffer$Buffer* bs$542;
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ _tmp$1665;
  moonbit_bytes_t _tmp$1664;
  int64_t _tmp$1668;
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ _tmp$1667;
  moonbit_bytes_t _tmp$1666;
  moonbit_bytes_t _tmp$1672;
  moonbit_bytes_t _tmp$1670;
  int32_t _field$2041;
  int32_t len$1671;
  moonbit_bytes_t _tmp$1669;
  void* _block$2195;
  moonbit_incref(bs1$533);
  $FixedArray$$blit_to$inner$0(bytes$535, bs1$533, length$534, offset$536, 0);
  bs0$537 = (moonbit_bytes_t)moonbit_make_bytes(2, 0);
  if (be$543) {
    j0$539 = 0;
    j1$540 = 1;
    goto $join$538;
  } else {
    j0$539 = 1;
    j1$540 = 0;
    goto $join$538;
  }
  $join$538:;
  _tmp$1661 = hi$541 >> 8;
  _tmp$1660 = _tmp$1661 & 0xff;
  if (j0$539 < 0 || j0$539 >= Moonbit_array_length(bs0$537)) {
    moonbit_panic();
  }
  bs0$537[j0$539] = _tmp$1660;
  _tmp$1663 = hi$541 & 255;
  _tmp$1662 = _tmp$1663 & 0xff;
  if (j1$540 < 0 || j1$540 >= Moonbit_array_length(bs0$537)) {
    moonbit_panic();
  }
  bs0$537[j1$540] = _tmp$1662;
  _tmp$1674 = Moonbit_array_length(bs0$537);
  _tmp$1675 = Moonbit_array_length(bs1$533);
  _tmp$1673 = _tmp$1674 + _tmp$1675;
  bs$542 = $moonbitlang$core$buffer$new$inner(_tmp$1673);
  _tmp$1665
  = $FixedArray$$sub$inner$0(
    bs0$537, 0, $moonbitlang$x$encoding$malformed_pair$constr$532
  );
  _tmp$1664 = $Bytes$$from_array(_tmp$1665);
  moonbit_incref(bs$542);
  $$moonbitlang$core$buffer$Buffer$$write_bytes(bs$542, _tmp$1664);
  _tmp$1668 = (int64_t)length$534;
  _tmp$1667 = $FixedArray$$sub$inner$0(bs1$533, 0, _tmp$1668);
  _tmp$1666 = $Bytes$$from_array(_tmp$1667);
  moonbit_incref(bs$542);
  $$moonbitlang$core$buffer$Buffer$$write_bytes(bs$542, _tmp$1666);
  moonbit_incref(bs$542);
  _tmp$1672 = $$moonbitlang$core$buffer$Buffer$$to_bytes(bs$542);
  _tmp$1670 = $Bytes$$to_fixedarray(_tmp$1672, 4294967296ll);
  _field$2041 = bs$542->$1;
  moonbit_decref(bs$542);
  len$1671 = _field$2041;
  _tmp$1669 = $moonbitlang$x$encoding$slice(_tmp$1670, 0, len$1671);
  _block$2195
  = (void*)moonbit_malloc(
      sizeof(struct $$moonbitlang$x$encoding$Decode$Malformed)
    );
  Moonbit_object_header(_block$2195)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$x$encoding$Decode$Malformed, $0) >> 2, 1, 2
  );
  ((struct $$moonbitlang$x$encoding$Decode$Malformed*)_block$2195)->$0
  = _tmp$1669;
  return _block$2195;
}

void* $moonbitlang$x$encoding$malformed(
  moonbit_bytes_t bytes$529,
  int32_t offset$530,
  int32_t length$531
) {
  moonbit_bytes_t _tmp$1659 =
    $moonbitlang$x$encoding$slice(bytes$529, offset$530, length$531);
  void* _block$2196 =
    (void*)moonbit_malloc(
      sizeof(struct $$moonbitlang$x$encoding$Decode$Malformed)
    );
  Moonbit_object_header(_block$2196)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$x$encoding$Decode$Malformed, $0) >> 2, 1, 2
  );
  ((struct $$moonbitlang$x$encoding$Decode$Malformed*)_block$2196)->$0
  = _tmp$1659;
  return _block$2196;
}

moonbit_bytes_t $moonbitlang$x$encoding$slice(
  moonbit_bytes_t bytes$527,
  int32_t offset$528,
  int32_t length$526
) {
  moonbit_bytes_t new_bytes$525 =
    (moonbit_bytes_t)moonbit_make_bytes(length$526, 48);
  moonbit_bytes_t _tmp$1657;
  int32_t _tmp$2042;
  int32_t _tmp$1658;
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ _tmp$1656;
  moonbit_incref(new_bytes$525);
  $FixedArray$$blit_to$inner$0(
    bytes$527, new_bytes$525, length$526, offset$528, 0
  );
  moonbit_incref(new_bytes$525);
  _tmp$1657 = new_bytes$525;
  _tmp$2042 = Moonbit_array_length(new_bytes$525);
  moonbit_decref(new_bytes$525);
  _tmp$1658 = _tmp$2042;
  _tmp$1656
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$){
    0, _tmp$1658, _tmp$1657
  };
  return $Bytes$$from_array(_tmp$1656);
}

int32_t $moonbitlang$core$cmp$minimum$0(int32_t x$523, int32_t y$524) {
  if (x$523 > y$524) {
    return y$524;
  } else {
    return x$523;
  }
}

int32_t $moonbitlang$core$cmp$maximum$0(int32_t x$521, int32_t y$522) {
  if (x$521 > y$522) {
    return x$521;
  } else {
    return y$522;
  }
}

moonbit_bytes_t $$moonbitlang$core$buffer$Buffer$$to_bytes(
  struct $$moonbitlang$core$buffer$Buffer* self$520
) {
  moonbit_bytes_t _field$2044 = self$520->$0;
  moonbit_bytes_t data$1653 = _field$2044;
  int32_t _field$2043 = self$520->$1;
  int32_t _cnt$2147 = Moonbit_object_header(self$520)->rc;
  int32_t len$1655;
  int64_t _tmp$1654;
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ _tmp$1652;
  if (_cnt$2147 > 1) {
    int32_t _new_cnt$2148 = _cnt$2147 - 1;
    Moonbit_object_header(self$520)->rc = _new_cnt$2148;
    moonbit_incref(data$1653);
  } else if (_cnt$2147 == 1) {
    moonbit_free(self$520);
  }
  len$1655 = _field$2043;
  _tmp$1654 = (int64_t)len$1655;
  _tmp$1652 = $FixedArray$$sub$inner$0(data$1653, 0, _tmp$1654);
  return $Bytes$$from_array(_tmp$1652);
}

int32_t $$moonbitlang$core$buffer$Buffer$$write_bytes(
  struct $$moonbitlang$core$buffer$Buffer* self$519,
  moonbit_bytes_t value$518
) {
  int32_t val_len$517 = Moonbit_array_length(value$518);
  int32_t len$1647 = self$519->$1;
  int32_t _tmp$1646 = len$1647 + val_len$517;
  moonbit_bytes_t _field$2045;
  moonbit_bytes_t data$1648;
  int32_t len$1649;
  int32_t len$1651;
  int32_t _tmp$1650;
  moonbit_incref(self$519);
  $$moonbitlang$core$buffer$Buffer$$grow_if_necessary(self$519, _tmp$1646);
  _field$2045 = self$519->$0;
  data$1648 = _field$2045;
  len$1649 = self$519->$1;
  moonbit_incref(data$1648);
  $FixedArray$$blit_from_bytes(
    data$1648, len$1649, value$518, 0, val_len$517
  );
  len$1651 = self$519->$1;
  _tmp$1650 = len$1651 + val_len$517;
  self$519->$1 = _tmp$1650;
  moonbit_decref(self$519);
  return 0;
}

struct $$moonbitlang$core$buffer$Buffer* $moonbitlang$core$buffer$new$inner(
  int32_t size_hint$515
) {
  int32_t initial$514;
  moonbit_bytes_t data$516;
  struct $$moonbitlang$core$buffer$Buffer* _block$2197;
  if (size_hint$515 < 1) {
    initial$514 = 1;
  } else {
    initial$514 = size_hint$515;
  }
  data$516 = (moonbit_bytes_t)moonbit_make_bytes(initial$514, 0);
  _block$2197
  = (struct $$moonbitlang$core$buffer$Buffer*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$buffer$Buffer)
    );
  Moonbit_object_header(_block$2197)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$buffer$Buffer, $0) >> 2, 1, 0
  );
  _block$2197->$0 = data$516;
  _block$2197->$1 = 0;
  return _block$2197;
}

int32_t $$moonbitlang$core$buffer$Buffer$$length(
  struct $$moonbitlang$core$buffer$Buffer* self$513
) {
  int32_t _field$2046 = self$513->$1;
  moonbit_decref(self$513);
  return _field$2046;
}

int32_t $$moonbitlang$core$buffer$Buffer$$write_byte(
  struct $$moonbitlang$core$buffer$Buffer* self$511,
  int32_t value$512
) {
  int32_t len$1641 = self$511->$1;
  int32_t _tmp$1640 = len$1641 + 1;
  moonbit_bytes_t _field$2047;
  moonbit_bytes_t data$1642;
  int32_t len$1643;
  int32_t len$1645;
  int32_t _tmp$1644;
  moonbit_incref(self$511);
  $$moonbitlang$core$buffer$Buffer$$grow_if_necessary(self$511, _tmp$1640);
  _field$2047 = self$511->$0;
  data$1642 = _field$2047;
  len$1643 = self$511->$1;
  if (len$1643 < 0 || len$1643 >= Moonbit_array_length(data$1642)) {
    moonbit_panic();
  }
  data$1642[len$1643] = value$512;
  len$1645 = self$511->$1;
  _tmp$1644 = len$1645 + 1;
  self$511->$1 = _tmp$1644;
  moonbit_decref(self$511);
  return 0;
}

int32_t $$moonbitlang$core$buffer$Buffer$$grow_if_necessary(
  struct $$moonbitlang$core$buffer$Buffer* self$505,
  int32_t required$508
) {
  moonbit_bytes_t _field$2055 = self$505->$0;
  moonbit_bytes_t data$1638 = _field$2055;
  int32_t _tmp$2054 = Moonbit_array_length(data$1638);
  int32_t _tmp$1637 = _tmp$2054;
  int32_t start$504;
  int32_t enough_space$506;
  int32_t space$507;
  moonbit_bytes_t _field$2051;
  moonbit_bytes_t data$1633;
  int32_t _tmp$2050;
  int32_t _tmp$1632;
  if (_tmp$1637 <= 0) {
    start$504 = 1;
  } else {
    moonbit_bytes_t _field$2053 = self$505->$0;
    moonbit_bytes_t data$1639 = _field$2053;
    int32_t _tmp$2052 = Moonbit_array_length(data$1639);
    start$504 = _tmp$2052;
  }
  space$507 = start$504;
  while (1) {
    int32_t _tmp$1636;
    if (space$507 >= required$508) {
      enough_space$506 = space$507;
      break;
    }
    _tmp$1636 = space$507 * 2;
    space$507 = _tmp$1636;
    continue;
    break;
  }
  _field$2051 = self$505->$0;
  data$1633 = _field$2051;
  _tmp$2050 = Moonbit_array_length(data$1633);
  _tmp$1632 = _tmp$2050;
  if (enough_space$506 != _tmp$1632) {
    moonbit_bytes_t new_data$510 =
      (moonbit_bytes_t)moonbit_make_bytes(enough_space$506, 0);
    moonbit_bytes_t _field$2049 = self$505->$0;
    moonbit_bytes_t data$1634 = _field$2049;
    int32_t len$1635 = self$505->$1;
    moonbit_bytes_t _old$2048;
    moonbit_incref(data$1634);
    moonbit_incref(new_data$510);
    $FixedArray$$unsafe_blit$0(new_data$510, 0, data$1634, 0, len$1635);
    _old$2048 = self$505->$0;
    moonbit_decref(_old$2048);
    self$505->$0 = new_data$510;
    moonbit_decref(self$505);
  } else {
    moonbit_decref(self$505);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$502,
  struct $$moonbitlang$core$builtin$Logger logger$503
) {
  moonbit_string_t _tmp$1631 = self$502;
  struct $$moonbitlang$core$builtin$SourceLocRepr* _tmp$1630 =
    $$moonbitlang$core$builtin$SourceLocRepr$$parse(_tmp$1631);
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
    _tmp$1630, logger$503
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$465,
  struct $$moonbitlang$core$builtin$Logger logger$501
) {
  struct $StringView _field$2065 =
    (struct $StringView){self$465->$0_1, self$465->$0_2, self$465->$0_0};
  struct $StringView pkg$464 = _field$2065;
  moonbit_string_t _field$2064 = pkg$464.$0;
  moonbit_string_t _data$466 = _field$2064;
  int32_t _start$467 = pkg$464.$1;
  int32_t end$1628 = pkg$464.$2;
  int32_t start$1629 = pkg$464.$1;
  int32_t _tmp$1627 = end$1628 - start$1629;
  int32_t _end$468 = _start$467 + _tmp$1627;
  int32_t _cursor$469 = _start$467;
  int32_t accept_state$470 = -1;
  int32_t match_end$471 = -1;
  int32_t match_tag_saver_0$472 = -1;
  int32_t tag_0$473 = -1;
  struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$* _bind$474;
  struct $StringView _field$2063;
  struct $StringView _module_name$497;
  void* _field$2062;
  int32_t _cnt$2149;
  void* _package_name$498;
  struct $StringView _field$2060;
  struct $StringView filename$1590;
  struct $StringView _field$2059;
  struct $StringView start_line$1591;
  struct $StringView _field$2058;
  struct $StringView start_column$1592;
  struct $StringView _field$2057;
  struct $StringView end_line$1593;
  struct $StringView _field$2056;
  int32_t _cnt$2153;
  struct $StringView end_column$1594;
  struct $$moonbitlang$core$builtin$Logger _bind$1589;
  moonbit_incref(_data$466);
  moonbit_incref(pkg$464.$0);
  while (1) {
    int32_t _tmp$1609 = _cursor$469;
    if (_tmp$1609 < _end$468) {
      int32_t _p$872 = _cursor$469;
      int32_t next_char$484 = _data$466[_p$872];
      int32_t _tmp$1610 = _cursor$469;
      _cursor$469 = _tmp$1610 + 1;
      if (next_char$484 < 55296) {
        if (next_char$484 < 47) {
          goto $join$482;
        } else if (next_char$484 > 47) {
          goto $join$482;
        } else {
          while (1) {
            int32_t _tmp$1611;
            tag_0$473 = _cursor$469;
            _tmp$1611 = _cursor$469;
            if (_tmp$1611 < _end$468) {
              int32_t _p$875 = _cursor$469;
              int32_t next_char$487 = _data$466[_p$875];
              int32_t _tmp$1612 = _cursor$469;
              _cursor$469 = _tmp$1612 + 1;
              if (next_char$487 < 55296) {
                if (next_char$487 < 47) {
                  goto $join$485;
                } else if (next_char$487 > 47) {
                  goto $join$485;
                } else {
                  while (1) {
                    int32_t _tmp$1613 = _cursor$469;
                    if (_tmp$1613 < _end$468) {
                      int32_t _p$878 = _cursor$469;
                      int32_t next_char$490 = _data$466[_p$878];
                      int32_t _tmp$1614 = _cursor$469;
                      _cursor$469 = _tmp$1614 + 1;
                      if (next_char$490 < 56319) {
                        if (next_char$490 < 55296) {
                          goto $join$488;
                        } else {
                          int32_t _tmp$1615 = _cursor$469;
                          if (_tmp$1615 < _end$468) {
                            int32_t _p$881 = _cursor$469;
                            int32_t next_char$491 = _data$466[_p$881];
                            int32_t _tmp$1616 = _cursor$469;
                            _cursor$469 = _tmp$1616 + 1;
                            if (next_char$491 < 56320) {
                              goto $join$475;
                            } else if (next_char$491 > 65535) {
                              goto $join$475;
                            } else {
                              continue;
                            }
                          } else {
                            goto $join$475;
                          }
                        }
                      } else if (next_char$490 > 56319) {
                        if (next_char$490 < 65536) {
                          goto $join$488;
                        } else {
                          goto $join$475;
                        }
                      } else {
                        int32_t _tmp$1617 = _cursor$469;
                        if (_tmp$1617 < _end$468) {
                          int32_t _p$884 = _cursor$469;
                          int32_t next_char$492 = _data$466[_p$884];
                          int32_t _tmp$1618 = _cursor$469;
                          _cursor$469 = _tmp$1618 + 1;
                          if (next_char$492 < 56320) {
                            goto $join$475;
                          } else if (next_char$492 > 57343) {
                            goto $join$475;
                          } else {
                            continue;
                          }
                        } else {
                          goto $join$475;
                        }
                      }
                      goto $joinlet$2205;
                      $join$488:;
                      continue;
                      $joinlet$2205:;
                    } else {
                      match_tag_saver_0$472 = tag_0$473;
                      accept_state$470 = 0;
                      match_end$471 = _cursor$469;
                      goto $join$475;
                    }
                    break;
                  }
                }
              } else if (next_char$487 > 56318) {
                if (next_char$487 < 57344) {
                  int32_t _tmp$1619 = _cursor$469;
                  if (_tmp$1619 < _end$468) {
                    int32_t _p$887 = _cursor$469;
                    int32_t next_char$493 = _data$466[_p$887];
                    int32_t _tmp$1620 = _cursor$469;
                    _cursor$469 = _tmp$1620 + 1;
                    if (next_char$493 < 56320) {
                      goto $join$475;
                    } else if (next_char$493 > 57343) {
                      goto $join$475;
                    } else {
                      continue;
                    }
                  } else {
                    goto $join$475;
                  }
                } else if (next_char$487 > 65535) {
                  goto $join$475;
                } else {
                  goto $join$485;
                }
              } else {
                int32_t _tmp$1621 = _cursor$469;
                if (_tmp$1621 < _end$468) {
                  int32_t _p$890 = _cursor$469;
                  int32_t next_char$494 = _data$466[_p$890];
                  int32_t _tmp$1622 = _cursor$469;
                  _cursor$469 = _tmp$1622 + 1;
                  if (next_char$494 < 56320) {
                    goto $join$475;
                  } else if (next_char$494 > 65535) {
                    goto $join$475;
                  } else {
                    continue;
                  }
                } else {
                  goto $join$475;
                }
              }
              goto $joinlet$2203;
              $join$485:;
              continue;
              $joinlet$2203:;
            } else {
              goto $join$475;
            }
            break;
          }
        }
      } else if (next_char$484 > 56318) {
        if (next_char$484 < 57344) {
          int32_t _tmp$1623 = _cursor$469;
          if (_tmp$1623 < _end$468) {
            int32_t _p$893 = _cursor$469;
            int32_t next_char$495 = _data$466[_p$893];
            int32_t _tmp$1624 = _cursor$469;
            _cursor$469 = _tmp$1624 + 1;
            if (next_char$495 < 56320) {
              goto $join$475;
            } else if (next_char$495 > 57343) {
              goto $join$475;
            } else {
              continue;
            }
          } else {
            goto $join$475;
          }
        } else if (next_char$484 > 65535) {
          goto $join$475;
        } else {
          goto $join$482;
        }
      } else {
        int32_t _tmp$1625 = _cursor$469;
        if (_tmp$1625 < _end$468) {
          int32_t _p$896 = _cursor$469;
          int32_t next_char$496 = _data$466[_p$896];
          int32_t _tmp$1626 = _cursor$469;
          _cursor$469 = _tmp$1626 + 1;
          if (next_char$496 < 56320) {
            goto $join$475;
          } else if (next_char$496 > 65535) {
            goto $join$475;
          } else {
            continue;
          }
        } else {
          goto $join$475;
        }
      }
      goto $joinlet$2201;
      $join$482:;
      continue;
      $joinlet$2201:;
    } else {
      goto $join$475;
    }
    break;
  }
  goto $joinlet$2199;
  $join$475:;
  switch (accept_state$470) {
    case 0: {
      void* _try_err$478;
      struct $StringView package_name$476;
      int32_t _tmp$1605;
      int32_t _tmp$1604;
      int64_t _tmp$1601;
      int32_t _tmp$1603;
      int64_t _tmp$1602;
      struct moonbit_result_0 _tmp$2207;
      void* _try_err$481;
      struct $StringView module_name$479;
      int64_t _tmp$1596;
      int32_t _tmp$1598;
      int64_t _tmp$1597;
      struct moonbit_result_0 _tmp$2209;
      void* Some$1595;
      moonbit_decref(pkg$464.$0);
      _tmp$1605 = match_tag_saver_0$472;
      _tmp$1604 = _tmp$1605 + 1;
      _tmp$1601 = (int64_t)_tmp$1604;
      _tmp$1603 = match_end$471;
      _tmp$1602 = (int64_t)_tmp$1603;
      moonbit_incref(_data$466);
      _tmp$2207 = $String$$sub(_data$466, _tmp$1601, _tmp$1602);
      if (_tmp$2207.tag) {
        struct $StringView const _ok$1606 = _tmp$2207.data.ok;
        package_name$476 = _ok$1606;
      } else {
        void* const _err$1607 = _tmp$2207.data.err;
        _try_err$478 = _err$1607;
        goto $join$477;
      }
      goto $joinlet$2206;
      $join$477:;
      moonbit_decref(_try_err$478);
      moonbit_panic();
      $joinlet$2206:;
      _tmp$1596 = (int64_t)_start$467;
      _tmp$1598 = match_tag_saver_0$472;
      _tmp$1597 = (int64_t)_tmp$1598;
      _tmp$2209 = $String$$sub(_data$466, _tmp$1596, _tmp$1597);
      if (_tmp$2209.tag) {
        struct $StringView const _ok$1599 = _tmp$2209.data.ok;
        module_name$479 = _ok$1599;
      } else {
        void* const _err$1600 = _tmp$2209.data.err;
        _try_err$481 = _err$1600;
        goto $join$480;
      }
      goto $joinlet$2208;
      $join$480:;
      moonbit_decref(_try_err$481);
      moonbit_panic();
      $joinlet$2208:;
      Some$1595
      = (void*)moonbit_malloc(sizeof(struct $Option$3c$StringView$3e$$Some));
      Moonbit_object_header(Some$1595)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $Option$3c$StringView$3e$$Some, $0_0) >> 2, 1, 1
      );
      ((struct $Option$3c$StringView$3e$$Some*)Some$1595)->$0_0
      = package_name$476.$0;
      ((struct $Option$3c$StringView$3e$$Some*)Some$1595)->$0_1
      = package_name$476.$1;
      ((struct $Option$3c$StringView$3e$$Some*)Some$1595)->$0_2
      = package_name$476.$2;
      _bind$474
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$474)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$474->$0_0 = module_name$479.$0;
      _bind$474->$0_1 = module_name$479.$1;
      _bind$474->$0_2 = module_name$479.$2;
      _bind$474->$1 = Some$1595;
      break;
    }
    default: {
      void* None$1608;
      moonbit_decref(_data$466);
      None$1608 = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _bind$474
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$474)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$474->$0_0 = pkg$464.$0;
      _bind$474->$0_1 = pkg$464.$1;
      _bind$474->$0_2 = pkg$464.$2;
      _bind$474->$1 = None$1608;
      break;
    }
  }
  $joinlet$2199:;
  _field$2063
  = (struct $StringView){
    _bind$474->$0_1, _bind$474->$0_2, _bind$474->$0_0
  };
  _module_name$497 = _field$2063;
  _field$2062 = _bind$474->$1;
  _cnt$2149 = Moonbit_object_header(_bind$474)->rc;
  if (_cnt$2149 > 1) {
    int32_t _new_cnt$2150 = _cnt$2149 - 1;
    Moonbit_object_header(_bind$474)->rc = _new_cnt$2150;
    moonbit_incref(_field$2062);
    moonbit_incref(_module_name$497.$0);
  } else if (_cnt$2149 == 1) {
    moonbit_free(_bind$474);
  }
  _package_name$498 = _field$2062;
  switch (Moonbit_object_tag(_package_name$498)) {
    case 1: {
      struct $Option$3c$StringView$3e$$Some* _Some$499 =
        (struct $Option$3c$StringView$3e$$Some*)_package_name$498;
      struct $StringView _field$2061 =
        (struct $StringView){
          _Some$499->$0_1, _Some$499->$0_2, _Some$499->$0_0
        };
      int32_t _cnt$2151 = Moonbit_object_header(_Some$499)->rc;
      struct $StringView _pkg_name$500;
      struct $$moonbitlang$core$builtin$Logger _bind$1588;
      if (_cnt$2151 > 1) {
        int32_t _new_cnt$2152 = _cnt$2151 - 1;
        Moonbit_object_header(_Some$499)->rc = _new_cnt$2152;
        moonbit_incref(_field$2061.$0);
      } else if (_cnt$2151 == 1) {
        moonbit_free(_Some$499);
      }
      _pkg_name$500 = _field$2061;
      if (logger$501.$1) {
        moonbit_incref(logger$501.$1);
      }
      logger$501.$0->$method_2(logger$501.$1, _pkg_name$500);
      _bind$1588 = logger$501;
      if (_bind$1588.$1) {
        moonbit_incref(_bind$1588.$1);
      }
      _bind$1588.$0->$method_3(_bind$1588.$1, 47);
      break;
    }
    default: {
      moonbit_decref(_package_name$498);
      break;
    }
  }
  _field$2060
  = (struct $StringView){
    self$465->$1_1, self$465->$1_2, self$465->$1_0
  };
  filename$1590 = _field$2060;
  moonbit_incref(filename$1590.$0);
  if (logger$501.$1) {
    moonbit_incref(logger$501.$1);
  }
  logger$501.$0->$method_2(logger$501.$1, filename$1590);
  if (logger$501.$1) {
    moonbit_incref(logger$501.$1);
  }
  logger$501.$0->$method_3(logger$501.$1, 58);
  _field$2059
  = (struct $StringView){
    self$465->$2_1, self$465->$2_2, self$465->$2_0
  };
  start_line$1591 = _field$2059;
  moonbit_incref(start_line$1591.$0);
  if (logger$501.$1) {
    moonbit_incref(logger$501.$1);
  }
  logger$501.$0->$method_2(logger$501.$1, start_line$1591);
  if (logger$501.$1) {
    moonbit_incref(logger$501.$1);
  }
  logger$501.$0->$method_3(logger$501.$1, 58);
  _field$2058
  = (struct $StringView){
    self$465->$3_1, self$465->$3_2, self$465->$3_0
  };
  start_column$1592 = _field$2058;
  moonbit_incref(start_column$1592.$0);
  if (logger$501.$1) {
    moonbit_incref(logger$501.$1);
  }
  logger$501.$0->$method_2(logger$501.$1, start_column$1592);
  if (logger$501.$1) {
    moonbit_incref(logger$501.$1);
  }
  logger$501.$0->$method_3(logger$501.$1, 45);
  _field$2057
  = (struct $StringView){
    self$465->$4_1, self$465->$4_2, self$465->$4_0
  };
  end_line$1593 = _field$2057;
  moonbit_incref(end_line$1593.$0);
  if (logger$501.$1) {
    moonbit_incref(logger$501.$1);
  }
  logger$501.$0->$method_2(logger$501.$1, end_line$1593);
  if (logger$501.$1) {
    moonbit_incref(logger$501.$1);
  }
  logger$501.$0->$method_3(logger$501.$1, 58);
  _field$2056
  = (struct $StringView){
    self$465->$5_1, self$465->$5_2, self$465->$5_0
  };
  _cnt$2153 = Moonbit_object_header(self$465)->rc;
  if (_cnt$2153 > 1) {
    int32_t _new_cnt$2159 = _cnt$2153 - 1;
    Moonbit_object_header(self$465)->rc = _new_cnt$2159;
    moonbit_incref(_field$2056.$0);
  } else if (_cnt$2153 == 1) {
    struct $StringView _field$2158 =
      (struct $StringView){self$465->$4_1, self$465->$4_2, self$465->$4_0};
    struct $StringView _field$2157;
    struct $StringView _field$2156;
    struct $StringView _field$2155;
    struct $StringView _field$2154;
    moonbit_decref(_field$2158.$0);
    _field$2157
    = (struct $StringView){
      self$465->$3_1, self$465->$3_2, self$465->$3_0
    };
    moonbit_decref(_field$2157.$0);
    _field$2156
    = (struct $StringView){
      self$465->$2_1, self$465->$2_2, self$465->$2_0
    };
    moonbit_decref(_field$2156.$0);
    _field$2155
    = (struct $StringView){
      self$465->$1_1, self$465->$1_2, self$465->$1_0
    };
    moonbit_decref(_field$2155.$0);
    _field$2154
    = (struct $StringView){
      self$465->$0_1, self$465->$0_2, self$465->$0_0
    };
    moonbit_decref(_field$2154.$0);
    moonbit_free(self$465);
  }
  end_column$1594 = _field$2056;
  if (logger$501.$1) {
    moonbit_incref(logger$501.$1);
  }
  logger$501.$0->$method_2(logger$501.$1, end_column$1594);
  if (logger$501.$1) {
    moonbit_incref(logger$501.$1);
  }
  logger$501.$0->$method_3(logger$501.$1, 64);
  _bind$1589 = logger$501;
  _bind$1589.$0->$method_2(_bind$1589.$1, _module_name$497);
  return 0;
}

moonbit_bytes_t $Bytes$$to_fixedarray(
  moonbit_bytes_t self$460,
  int64_t len$459
) {
  int32_t len$458;
  moonbit_bytes_t _tmp$1586;
  moonbit_bytes_t arr$463;
  if (len$459 == 4294967296ll) {
    len$458 = Moonbit_array_length(self$460);
  } else {
    int64_t _Some$461 = len$459;
    int32_t _x$462 = (int32_t)_Some$461;
    int32_t _if_result$2210;
    if (0 <= _x$462) {
      int32_t _tmp$1587 = Moonbit_array_length(self$460);
      _if_result$2210 = _x$462 <= _tmp$1587;
    } else {
      _if_result$2210 = 0;
    }
    if (_if_result$2210) {
      len$458 = _x$462;
    } else {
      moonbit_panic();
    }
  }
  _tmp$1586 = (moonbit_bytes_t)moonbit_make_bytes_raw(len$458);
  arr$463 = _tmp$1586;
  moonbit_incref(arr$463);
  $FixedArray$$blit_from_bytes(arr$463, 0, self$460, 0, len$458);
  return arr$463;
}

moonbit_bytes_t $Bytes$$from_array(
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ arr$456
) {
  int32_t end$1584 = arr$456.$2;
  int32_t start$1585 = arr$456.$1;
  int32_t _tmp$1580 = end$1584 - start$1585;
  struct $Bytes$$from_array$fn$2$2d$cap* _closure$2211 =
    (struct $Bytes$$from_array$fn$2$2d$cap*)moonbit_malloc(
      sizeof(struct $Bytes$$from_array$fn$2$2d$cap)
    );
  struct $$3c$Int$3e$$3d$$3e$Byte* _tmp$1581;
  Moonbit_object_header(_closure$2211)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Bytes$$from_array$fn$2$2d$cap, $0_0) >> 2, 1, 0
  );
  _closure$2211->code = &$Bytes$$from_array$fn$2;
  _closure$2211->$0_0 = arr$456.$0;
  _closure$2211->$0_1 = arr$456.$1;
  _closure$2211->$0_2 = arr$456.$2;
  _tmp$1581 = (struct $$3c$Int$3e$$3d$$3e$Byte*)_closure$2211;
  return $Bytes$$makei$0(_tmp$1580, _tmp$1581);
}

int32_t $Bytes$$from_array$fn$2(
  struct $$3c$Int$3e$$3d$$3e$Byte* _env$1582,
  int32_t i$457
) {
  struct $Bytes$$from_array$fn$2$2d$cap* _casted_env$1583 =
    (struct $Bytes$$from_array$fn$2$2d$cap*)_env$1582;
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ _field$2066 =
    (struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$){
      _casted_env$1583->$0_1, _casted_env$1583->$0_2, _casted_env$1583->$0_0
    };
  int32_t _cnt$2160 = Moonbit_object_header(_casted_env$1583)->rc;
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ arr$456;
  if (_cnt$2160 > 1) {
    int32_t _new_cnt$2161 = _cnt$2160 - 1;
    Moonbit_object_header(_casted_env$1583)->rc = _new_cnt$2161;
    moonbit_incref(_field$2066.$0);
  } else if (_cnt$2160 == 1) {
    moonbit_free(_casted_env$1583);
  }
  arr$456 = _field$2066;
  return $$moonbitlang$core$builtin$ArrayView$$at$0(arr$456, i$457);
}

int32_t $FixedArray$$blit_from_bytesview(
  moonbit_bytes_t self$453,
  int32_t bytes_offset$454,
  struct $BytesView src$455
) {
  moonbit_bytes_t _field$2068 = src$455.$0;
  moonbit_bytes_t bytes$1575 = _field$2068;
  int32_t start$1576 = src$455.$1;
  int32_t end$1578 = src$455.$2;
  int32_t _field$2067 = src$455.$1;
  int32_t start$1579 = _field$2067;
  int32_t _tmp$1577 = end$1578 - start$1579;
  $FixedArray$$blit_from_bytes(
    self$453, bytes_offset$454, bytes$1575, start$1576, _tmp$1577
  );
  return 0;
}

int32_t $FixedArray$$blit_from_bytes(
  moonbit_bytes_t self$450,
  int32_t bytes_offset$445,
  moonbit_bytes_t src$452,
  int32_t src_offset$448,
  int32_t length$446
) {
  int32_t _tmp$1574 = bytes_offset$445 + length$446;
  int32_t e1$444 = _tmp$1574 - 1;
  int32_t _tmp$1573 = src_offset$448 + length$446;
  int32_t e2$447 = _tmp$1573 - 1;
  int32_t len1$449 = Moonbit_array_length(self$450);
  int32_t len2$451 = Moonbit_array_length(src$452);
  if (
    length$446 >= 0
    && bytes_offset$445 >= 0
    && e1$444 < len1$449
    && src_offset$448 >= 0
    && e2$447 < len2$451
  ) {
    moonbit_bytes_t _tmp$1572 = src$452;
    $FixedArray$$unsafe_blit$0(
      self$450, bytes_offset$445, _tmp$1572, src_offset$448, length$446
    );
  } else {
    moonbit_decref(src$452);
    moonbit_decref(self$450);
    moonbit_panic();
  }
  return 0;
}

struct $BytesView $Bytes$$sub$inner(
  moonbit_bytes_t self$437,
  int32_t start$443,
  int64_t end$439
) {
  int32_t len$436 = Moonbit_array_length(self$437);
  int32_t end$438;
  int32_t start$442;
  if (end$439 == 4294967296ll) {
    end$438 = len$436;
  } else {
    int64_t _Some$440 = end$439;
    int32_t _end$441 = (int32_t)_Some$440;
    if (_end$441 < 0) {
      end$438 = len$436 + _end$441;
    } else {
      end$438 = _end$441;
    }
  }
  if (start$443 < 0) {
    start$442 = len$436 + start$443;
  } else {
    start$442 = start$443;
  }
  if (start$442 >= 0 && start$442 <= end$438 && end$438 <= len$436) {
    return (struct $BytesView){start$442, end$438, self$437};
  } else {
    moonbit_decref(self$437);
    return $moonbitlang$core$builtin$abort$2(
             (moonbit_string_t)moonbit_string_literal_10.data,
               (moonbit_string_t)moonbit_string_literal_11.data
           );
  }
}

int32_t $BytesView$$length(struct $BytesView self$435) {
  int32_t end$1570 = self$435.$2;
  int32_t _field$2069 = self$435.$1;
  int32_t start$1571;
  moonbit_decref(self$435.$0);
  start$1571 = _field$2069;
  return end$1570 - start$1571;
}

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$434) {
  moonbit_println(input$434);
  moonbit_decref(input$434);
  return 0;
}

moonbit_bytes_t $Bytes$$makei$0(
  int32_t length$429,
  struct $$3c$Int$3e$$3d$$3e$Byte* value$431
) {
  int32_t _tmp$1569;
  moonbit_bytes_t arr$430;
  int32_t i$432;
  if (length$429 <= 0) {
    moonbit_decref(value$431);
    return (moonbit_bytes_t)moonbit_bytes_literal_0.data;
  }
  moonbit_incref(value$431);
  _tmp$1569 = value$431->code(value$431, 0);
  arr$430 = (moonbit_bytes_t)moonbit_make_bytes(length$429, _tmp$1569);
  i$432 = 1;
  while (1) {
    if (i$432 < length$429) {
      int32_t _tmp$1567;
      int32_t _tmp$1568;
      moonbit_incref(value$431);
      _tmp$1567 = value$431->code(value$431, i$432);
      if (i$432 < 0 || i$432 >= Moonbit_array_length(arr$430)) {
        moonbit_panic();
      }
      arr$430[i$432] = _tmp$1567;
      _tmp$1568 = i$432 + 1;
      i$432 = _tmp$1568;
      continue;
    } else {
      moonbit_decref(value$431);
    }
    break;
  }
  return arr$430;
}

int32_t $FixedArray$$blit_to$inner$0(
  moonbit_bytes_t self$428,
  moonbit_bytes_t dst$427,
  int32_t len$426,
  int32_t src_offset$425,
  int32_t dst_offset$424
) {
  int32_t _if_result$2213;
  if (dst_offset$424 >= 0) {
    if (src_offset$425 >= 0) {
      int32_t _tmp$1549 = dst_offset$424 + len$426;
      int32_t _tmp$1550 = Moonbit_array_length(dst$427);
      if (_tmp$1549 <= _tmp$1550) {
        int32_t _tmp$1547 = src_offset$425 + len$426;
        int32_t _tmp$1548 = Moonbit_array_length(self$428);
        _if_result$2213 = _tmp$1547 <= _tmp$1548;
      } else {
        _if_result$2213 = 0;
      }
    } else {
      _if_result$2213 = 0;
    }
  } else {
    _if_result$2213 = 0;
  }
  if (_if_result$2213) {
    $FixedArray$$unsafe_blit$0(
      dst$427, dst_offset$424, self$428, src_offset$425, len$426
    );
  } else {
    moonbit_string_t _tmp$1566 =
      $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
        dst_offset$424
      );
    moonbit_string_t _tmp$1565 =
      moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_12.data, _tmp$1566
      );
    moonbit_string_t _tmp$1563 =
      moonbit_add_string(
        _tmp$1565, (moonbit_string_t)moonbit_string_literal_13.data
      );
    moonbit_string_t _tmp$1564 =
      $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
        src_offset$425
      );
    moonbit_string_t _tmp$1562 = moonbit_add_string(_tmp$1563, _tmp$1564);
    moonbit_string_t _tmp$1560 =
      moonbit_add_string(
        _tmp$1562, (moonbit_string_t)moonbit_string_literal_14.data
      );
    moonbit_string_t _tmp$1561 =
      $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(len$426);
    moonbit_string_t _tmp$1559 = moonbit_add_string(_tmp$1560, _tmp$1561);
    moonbit_string_t _tmp$1556 =
      moonbit_add_string(
        _tmp$1559, (moonbit_string_t)moonbit_string_literal_15.data
      );
    int32_t _tmp$2071 = Moonbit_array_length(dst$427);
    int32_t _tmp$1558;
    moonbit_string_t _tmp$1557;
    moonbit_string_t _tmp$1555;
    moonbit_string_t _tmp$1552;
    int32_t _tmp$2070;
    int32_t _tmp$1554;
    moonbit_string_t _tmp$1553;
    moonbit_string_t _tmp$1551;
    moonbit_decref(dst$427);
    _tmp$1558 = _tmp$2071;
    _tmp$1557
    = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
      _tmp$1558
    );
    _tmp$1555 = moonbit_add_string(_tmp$1556, _tmp$1557);
    _tmp$1552
    = moonbit_add_string(
      _tmp$1555, (moonbit_string_t)moonbit_string_literal_16.data
    );
    _tmp$2070 = Moonbit_array_length(self$428);
    moonbit_decref(self$428);
    _tmp$1554 = _tmp$2070;
    _tmp$1553
    = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
      _tmp$1554
    );
    _tmp$1551 = moonbit_add_string(_tmp$1552, _tmp$1553);
    $moonbitlang$core$builtin$abort$0(
      _tmp$1551, (moonbit_string_t)moonbit_string_literal_17.data
    );
  }
  return 0;
}

moonbit_bytes_t $Bytes$$new(int32_t len$423) {
  return $Bytes$$make(len$423, 0);
}

moonbit_bytes_t $Bytes$$make(int32_t len$421, int32_t init$422) {
  if (len$421 < 0) {
    return (moonbit_bytes_t)moonbit_bytes_literal_0.data;
  }
  return moonbit_make_bytes(len$421, init$422);
}

moonbit_bytes_t $$moonbitlang$core$builtin$Default$$FixedArray$$default$0() {
  return (moonbit_bytes_t)moonbit_empty_int8_array;
}

struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ $FixedArray$$sub$inner$0(
  moonbit_bytes_t self$414,
  int32_t start$420,
  int64_t end$416
) {
  int32_t len$413 = Moonbit_array_length(self$414);
  int32_t end$415;
  int32_t start$419;
  if (end$416 == 4294967296ll) {
    end$415 = len$413;
  } else {
    int64_t _Some$417 = end$416;
    int32_t _end$418 = (int32_t)_Some$417;
    if (_end$418 < 0) {
      end$415 = len$413 + _end$418;
    } else {
      end$415 = _end$418;
    }
  }
  if (start$420 < 0) {
    start$419 = len$413 + start$420;
  } else {
    start$419 = start$420;
  }
  if (start$419 >= 0 && start$419 <= end$415 && end$415 <= len$413) {
    moonbit_bytes_t _tmp$1546 = self$414;
    return (struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$){
             start$419, end$415, _tmp$1546
           };
  } else {
    moonbit_decref(self$414);
    return $moonbitlang$core$builtin$abort$3(
             (moonbit_string_t)moonbit_string_literal_18.data,
               (moonbit_string_t)moonbit_string_literal_19.data
           );
  }
}

int32_t $$moonbitlang$core$builtin$ArrayView$$at$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ self$412,
  int32_t index$411
) {
  int32_t _if_result$2214;
  if (index$411 >= 0) {
    int32_t end$1533 = self$412.$2;
    int32_t start$1534 = self$412.$1;
    int32_t _tmp$1532 = end$1533 - start$1534;
    _if_result$2214 = index$411 < _tmp$1532;
  } else {
    _if_result$2214 = 0;
  }
  if (_if_result$2214) {
    moonbit_bytes_t _field$2074 = self$412.$0;
    moonbit_bytes_t buf$1535 = _field$2074;
    int32_t _field$2073 = self$412.$1;
    int32_t start$1537 = _field$2073;
    int32_t _tmp$1536 = start$1537 + index$411;
    int32_t _tmp$2072;
    if (_tmp$1536 < 0 || _tmp$1536 >= Moonbit_array_length(buf$1535)) {
      moonbit_panic();
    }
    _tmp$2072 = (int32_t)buf$1535[_tmp$1536];
    moonbit_decref(buf$1535);
    return _tmp$2072;
  } else {
    int32_t end$1544 = self$412.$2;
    int32_t _field$2075 = self$412.$1;
    int32_t start$1545;
    int32_t _tmp$1543;
    moonbit_string_t _tmp$1542;
    moonbit_string_t _tmp$1541;
    moonbit_string_t _tmp$1539;
    moonbit_string_t _tmp$1540;
    moonbit_string_t _tmp$1538;
    moonbit_decref(self$412.$0);
    start$1545 = _field$2075;
    _tmp$1543 = end$1544 - start$1545;
    _tmp$1542
    = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
      _tmp$1543
    );
    _tmp$1541
    = moonbit_add_string(
      (moonbit_string_t)moonbit_string_literal_20.data, _tmp$1542
    );
    _tmp$1539
    = moonbit_add_string(
      _tmp$1541, (moonbit_string_t)moonbit_string_literal_21.data
    );
    _tmp$1540
    = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
      index$411
    );
    _tmp$1538 = moonbit_add_string(_tmp$1539, _tmp$1540);
    return $moonbitlang$core$builtin$abort$4(
             _tmp$1538, (moonbit_string_t)moonbit_string_literal_22.data
           );
  }
}

int32_t $ReadOnlyArray$$at$0(int32_t* self$409, int32_t index$410) {
  int32_t* _tmp$1531 = self$409;
  int32_t _tmp$2076;
  if (index$410 < 0 || index$410 >= Moonbit_array_length(_tmp$1531)) {
    moonbit_panic();
  }
  _tmp$2076 = (int32_t)_tmp$1531[index$410];
  moonbit_decref(_tmp$1531);
  return _tmp$2076;
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$408
) {
  return self$408;
}

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$407,
  struct $$moonbitlang$core$builtin$Logger logger$406
) {
  moonbit_string_t _tmp$1530 = $Int$$to_string$inner(self$407, 10);
  logger$406.$0->$method_0(logger$406.$1, _tmp$1530);
  return 0;
}

moonbit_string_t $$moonbitlang$core$builtin$Default$$String$$default() {
  return (moonbit_string_t)moonbit_string_literal_4.data;
}

struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* $String$$iter(
  moonbit_string_t self$401
) {
  int32_t len$400 = Moonbit_array_length(self$401);
  struct $Ref$3c$Int$3e$* index$402 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  struct $String$$iter$$2a$p$fn$1$2d$cap* _closure$2215;
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* _p$859;
  Moonbit_object_header(index$402)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  index$402->$0 = 0;
  _closure$2215
  = (struct $String$$iter$$2a$p$fn$1$2d$cap*)moonbit_malloc(
      sizeof(struct $String$$iter$$2a$p$fn$1$2d$cap)
    );
  Moonbit_object_header(_closure$2215)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $String$$iter$$2a$p$fn$1$2d$cap, $0) >> 2, 2, 0
  );
  _closure$2215->code = &$String$$iter$$2a$p$fn$1;
  _closure$2215->$0 = index$402;
  _closure$2215->$1 = self$401;
  _closure$2215->$2 = len$400;
  _p$859 = (struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$*)_closure$2215;
  return _p$859;
}

int32_t $String$$iter$$2a$p$fn$1(
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* _env$1517
) {
  struct $String$$iter$$2a$p$fn$1$2d$cap* _casted_env$1518 =
    (struct $String$$iter$$2a$p$fn$1$2d$cap*)_env$1517;
  int32_t len$400 = _casted_env$1518->$2;
  moonbit_string_t _field$2079 = _casted_env$1518->$1;
  moonbit_string_t self$401 = _field$2079;
  struct $Ref$3c$Int$3e$* _field$2078 = _casted_env$1518->$0;
  int32_t _cnt$2162 = Moonbit_object_header(_casted_env$1518)->rc;
  struct $Ref$3c$Int$3e$* index$402;
  int32_t val$1519;
  if (_cnt$2162 > 1) {
    int32_t _new_cnt$2163 = _cnt$2162 - 1;
    Moonbit_object_header(_casted_env$1518)->rc = _new_cnt$2163;
    moonbit_incref(self$401);
    moonbit_incref(_field$2078);
  } else if (_cnt$2162 == 1) {
    moonbit_free(_casted_env$1518);
  }
  index$402 = _field$2078;
  val$1519 = index$402->$0;
  if (val$1519 < len$400) {
    int32_t val$1529 = index$402->$0;
    int32_t c1$403 = self$401[val$1529];
    int32_t _if_result$2216;
    int32_t val$1527;
    int32_t _tmp$1526;
    int32_t _tmp$1528;
    if (55296 <= c1$403 && c1$403 <= 56319) {
      int32_t val$1521 = index$402->$0;
      int32_t _tmp$1520 = val$1521 + 1;
      _if_result$2216 = _tmp$1520 < len$400;
    } else {
      _if_result$2216 = 0;
    }
    if (_if_result$2216) {
      int32_t val$1525 = index$402->$0;
      int32_t _tmp$1524 = val$1525 + 1;
      int32_t _tmp$2077 = self$401[_tmp$1524];
      int32_t c2$404;
      moonbit_decref(self$401);
      c2$404 = _tmp$2077;
      if (56320 <= c2$404 && c2$404 <= 57343) {
        int32_t c$405 =
          $moonbitlang$core$builtin$code_point_of_surrogate_pair(
            c1$403, c2$404
          );
        int32_t val$1523 = index$402->$0;
        int32_t _tmp$1522 = val$1523 + 2;
        index$402->$0 = _tmp$1522;
        moonbit_decref(index$402);
        return c$405;
      }
    } else {
      moonbit_decref(self$401);
    }
    val$1527 = index$402->$0;
    _tmp$1526 = val$1527 + 1;
    index$402->$0 = _tmp$1526;
    moonbit_decref(index$402);
    _tmp$1528 = c1$403;
    return _tmp$1528;
  } else {
    moonbit_decref(index$402);
    moonbit_decref(self$401);
    return -1;
  }
}

int32_t $$moonbitlang$core$builtin$Iter$$next$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* self$399
) {
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* _func$398 = self$399;
  return _func$398->code(_func$398);
}

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$395,
  int32_t value$397
) {
  int32_t len$1512 = self$395->$1;
  int32_t* _field$2082 = self$395->$0;
  int32_t* buf$1514 = _field$2082;
  int32_t _tmp$2081 = Moonbit_array_length(buf$1514);
  int32_t _tmp$1513 = _tmp$2081;
  int32_t length$396;
  int32_t* _field$2080;
  int32_t* buf$1515;
  int32_t _tmp$1516;
  if (len$1512 == _tmp$1513) {
    moonbit_incref(self$395);
    $$moonbitlang$core$builtin$Array$$realloc$1(self$395);
  }
  length$396 = self$395->$1;
  _field$2080 = self$395->$0;
  buf$1515 = _field$2080;
  buf$1515[length$396] = value$397;
  _tmp$1516 = length$396 + 1;
  self$395->$1 = _tmp$1516;
  moonbit_decref(self$395);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$$example$gen$ListUsersRow$3e$* self$392,
  struct $$example$gen$ListUsersRow* value$394
) {
  int32_t len$1507 = self$392->$1;
  struct $$example$gen$ListUsersRow** _field$2086 = self$392->$0;
  struct $$example$gen$ListUsersRow** buf$1509 = _field$2086;
  int32_t _tmp$2085 = Moonbit_array_length(buf$1509);
  int32_t _tmp$1508 = _tmp$2085;
  int32_t length$393;
  struct $$example$gen$ListUsersRow** _field$2084;
  struct $$example$gen$ListUsersRow** buf$1510;
  struct $$example$gen$ListUsersRow* _old$2083;
  int32_t _tmp$1511;
  if (len$1507 == _tmp$1508) {
    moonbit_incref(self$392);
    $$moonbitlang$core$builtin$Array$$realloc$0(self$392);
  }
  length$393 = self$392->$1;
  _field$2084 = self$392->$0;
  buf$1510 = _field$2084;
  _old$2083 = (struct $$example$gen$ListUsersRow*)buf$1510[length$393];
  if (_old$2083) {
    moonbit_decref(_old$2083);
  }
  buf$1510[length$393] = value$394;
  _tmp$1511 = length$393 + 1;
  self$392->$1 = _tmp$1511;
  moonbit_decref(self$392);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$390
) {
  int32_t old_cap$389 = self$390->$1;
  int32_t new_cap$391;
  if (old_cap$389 == 0) {
    new_cap$391 = 8;
  } else {
    new_cap$391 = old_cap$389 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$1(self$390, new_cap$391);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$$example$gen$ListUsersRow$3e$* self$387
) {
  int32_t old_cap$386 = self$387->$1;
  int32_t new_cap$388;
  if (old_cap$386 == 0) {
    new_cap$388 = 8;
  } else {
    new_cap$388 = old_cap$386 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$0(self$387, new_cap$388);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$383,
  int32_t new_capacity$381
) {
  int32_t* new_buf$380 =
    (int32_t*)moonbit_make_int32_array_raw(new_capacity$381);
  int32_t* _field$2088 = self$383->$0;
  int32_t* old_buf$382 = _field$2088;
  int32_t old_cap$384 = Moonbit_array_length(old_buf$382);
  int32_t copy_len$385;
  int32_t* _old$2087;
  if (old_cap$384 < new_capacity$381) {
    copy_len$385 = old_cap$384;
  } else {
    copy_len$385 = new_capacity$381;
  }
  moonbit_incref(old_buf$382);
  moonbit_incref(new_buf$380);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
    new_buf$380, 0, old_buf$382, 0, copy_len$385
  );
  _old$2087 = self$383->$0;
  moonbit_decref(_old$2087);
  self$383->$0 = new_buf$380;
  moonbit_decref(self$383);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$example$gen$ListUsersRow$3e$* self$377,
  int32_t new_capacity$375
) {
  struct $$example$gen$ListUsersRow** new_buf$374 =
    (struct $$example$gen$ListUsersRow**)moonbit_make_ref_array(
      new_capacity$375, 0
    );
  struct $$example$gen$ListUsersRow** _field$2090 = self$377->$0;
  struct $$example$gen$ListUsersRow** old_buf$376 = _field$2090;
  int32_t old_cap$378 = Moonbit_array_length(old_buf$376);
  int32_t copy_len$379;
  struct $$example$gen$ListUsersRow** _old$2089;
  if (old_cap$378 < new_capacity$375) {
    copy_len$379 = old_cap$378;
  } else {
    copy_len$379 = new_capacity$375;
  }
  moonbit_incref(old_buf$376);
  moonbit_incref(new_buf$374);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
    new_buf$374, 0, old_buf$376, 0, copy_len$379
  );
  _old$2089 = self$377->$0;
  moonbit_decref(_old$2089);
  self$377->$0 = new_buf$374;
  moonbit_decref(self$377);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$372,
  struct $StringView str$373
) {
  int32_t len$1489 = self$372->$1;
  int32_t end$1492 = str$373.$2;
  int32_t start$1493 = str$373.$1;
  int32_t _tmp$1491 = end$1492 - start$1493;
  int32_t _tmp$1490 = _tmp$1491 * 2;
  int32_t _tmp$1488 = len$1489 + _tmp$1490;
  moonbit_bytes_t _field$2093;
  moonbit_bytes_t data$1494;
  int32_t len$1495;
  moonbit_string_t _field$2092;
  moonbit_string_t str$1496;
  int32_t start$1497;
  int32_t end$1499;
  int32_t start$1500;
  int32_t _tmp$1498;
  int32_t len$1502;
  int32_t end$1505;
  int32_t _field$2091;
  int32_t start$1506;
  int32_t _tmp$1504;
  int32_t _tmp$1503;
  int32_t _tmp$1501;
  moonbit_incref(self$372);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$372, _tmp$1488
  );
  _field$2093 = self$372->$0;
  data$1494 = _field$2093;
  len$1495 = self$372->$1;
  _field$2092 = str$373.$0;
  str$1496 = _field$2092;
  start$1497 = str$373.$1;
  end$1499 = str$373.$2;
  start$1500 = str$373.$1;
  _tmp$1498 = end$1499 - start$1500;
  moonbit_incref(str$1496);
  moonbit_incref(data$1494);
  $FixedArray$$blit_from_string(
    data$1494, len$1495, str$1496, start$1497, _tmp$1498
  );
  len$1502 = self$372->$1;
  end$1505 = str$373.$2;
  _field$2091 = str$373.$1;
  moonbit_decref(str$373.$0);
  start$1506 = _field$2091;
  _tmp$1504 = end$1505 - start$1506;
  _tmp$1503 = _tmp$1504 * 2;
  _tmp$1501 = len$1502 + _tmp$1503;
  self$372->$1 = _tmp$1501;
  moonbit_decref(self$372);
  return 0;
}

moonbit_string_t $String$$from_array(
  struct $$moonbitlang$core$builtin$ArrayView$3c$Char$3e$ chars$367
) {
  int32_t end$1486 = chars$367.$2;
  int32_t start$1487 = chars$367.$1;
  int32_t _tmp$1485 = end$1486 - start$1487;
  int32_t _tmp$1484 = _tmp$1485 * 4;
  struct $$moonbitlang$core$builtin$StringBuilder* buf$366 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(_tmp$1484);
  int32_t end$1482 = chars$367.$2;
  int32_t start$1483 = chars$367.$1;
  int32_t _len$368 = end$1482 - start$1483;
  int32_t _i$369 = 0;
  while (1) {
    if (_i$369 < _len$368) {
      int32_t* _field$2095 = chars$367.$0;
      int32_t* buf$1478 = _field$2095;
      int32_t start$1480 = chars$367.$1;
      int32_t _tmp$1479 = start$1480 + _i$369;
      int32_t _tmp$2094 = (int32_t)buf$1478[_tmp$1479];
      int32_t c$370 = _tmp$2094;
      int32_t _tmp$1481;
      moonbit_incref(buf$366);
      $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
        buf$366, c$370
      );
      _tmp$1481 = _i$369 + 1;
      _i$369 = _tmp$1481;
      continue;
    } else {
      moonbit_decref(chars$367.$0);
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$366);
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$Char$3e$ self$365
) {
  int32_t end$1476 = self$365.$2;
  int32_t _field$2096 = self$365.$1;
  int32_t start$1477;
  moonbit_decref(self$365.$0);
  start$1477 = _field$2096;
  return end$1476 - start$1477;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ self$364
) {
  int32_t end$1474 = self$364.$2;
  int32_t _field$2097 = self$364.$1;
  int32_t start$1475;
  moonbit_decref(self$364.$0);
  start$1475 = _field$2097;
  return end$1474 - start$1475;
}

struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* $$moonbitlang$core$builtin$Iter$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$Char$3e$* f$363
) {
  return f$363;
}

moonbit_string_t $Int64$$to_string$inner(int64_t self$347, int32_t radix$346) {
  int32_t is_negative$348;
  uint64_t num$349;
  uint16_t* buffer$350;
  if (radix$346 < 2 || radix$346 > 36) {
    $moonbitlang$core$builtin$abort$0(
      (moonbit_string_t)moonbit_string_literal_23.data,
        (moonbit_string_t)moonbit_string_literal_24.data
    );
  }
  if (self$347 == 0ll) {
    return (moonbit_string_t)moonbit_string_literal_25.data;
  }
  is_negative$348 = self$347 < 0ll;
  if (is_negative$348) {
    int64_t _tmp$1473 = -self$347;
    num$349 = *(uint64_t*)&_tmp$1473;
  } else {
    num$349 = *(uint64_t*)&self$347;
  }
  switch (radix$346) {
    case 10: {
      int32_t digit_len$351 = $moonbitlang$core$builtin$dec_count64(num$349);
      int32_t _tmp$1470;
      int32_t total_len$352;
      uint16_t* buffer$353;
      int32_t digit_start$354;
      if (is_negative$348) {
        _tmp$1470 = 1;
      } else {
        _tmp$1470 = 0;
      }
      total_len$352 = digit_len$351 + _tmp$1470;
      buffer$353 = (uint16_t*)moonbit_make_string(total_len$352, 0);
      if (is_negative$348) {
        digit_start$354 = 1;
      } else {
        digit_start$354 = 0;
      }
      moonbit_incref(buffer$353);
      $moonbitlang$core$builtin$int64_to_string_dec(
        buffer$353, num$349, digit_start$354, total_len$352
      );
      buffer$350 = buffer$353;
      break;
    }
    
    case 16: {
      int32_t digit_len$355 = $moonbitlang$core$builtin$hex_count64(num$349);
      int32_t _tmp$1471;
      int32_t total_len$356;
      uint16_t* buffer$357;
      int32_t digit_start$358;
      if (is_negative$348) {
        _tmp$1471 = 1;
      } else {
        _tmp$1471 = 0;
      }
      total_len$356 = digit_len$355 + _tmp$1471;
      buffer$357 = (uint16_t*)moonbit_make_string(total_len$356, 0);
      if (is_negative$348) {
        digit_start$358 = 1;
      } else {
        digit_start$358 = 0;
      }
      moonbit_incref(buffer$357);
      $moonbitlang$core$builtin$int64_to_string_hex(
        buffer$357, num$349, digit_start$358, total_len$356
      );
      buffer$350 = buffer$357;
      break;
    }
    default: {
      int32_t digit_len$359 =
        $moonbitlang$core$builtin$radix_count64(num$349, radix$346);
      int32_t _tmp$1472;
      int32_t total_len$360;
      uint16_t* buffer$361;
      int32_t digit_start$362;
      if (is_negative$348) {
        _tmp$1472 = 1;
      } else {
        _tmp$1472 = 0;
      }
      total_len$360 = digit_len$359 + _tmp$1472;
      buffer$361 = (uint16_t*)moonbit_make_string(total_len$360, 0);
      if (is_negative$348) {
        digit_start$362 = 1;
      } else {
        digit_start$362 = 0;
      }
      moonbit_incref(buffer$361);
      $moonbitlang$core$builtin$int64_to_string_generic(
        buffer$361, num$349, digit_start$362, total_len$360, radix$346
      );
      buffer$350 = buffer$361;
      break;
    }
  }
  if (is_negative$348) {
    buffer$350[0] = 45;
  }
  return buffer$350;
}

int32_t $moonbitlang$core$builtin$int64_to_string_dec(
  uint16_t* buffer$336,
  uint64_t num$324,
  int32_t digit_start$327,
  int32_t total_len$326
) {
  uint64_t num$323 = num$324;
  int32_t offset$325 = total_len$326 - digit_start$327;
  uint64_t _tmp$1469;
  int32_t remaining$338;
  int32_t _tmp$1450;
  while (1) {
    uint64_t _tmp$1413 = num$323;
    if (_tmp$1413 >= 10000ull) {
      uint64_t _tmp$1436 = num$323;
      uint64_t t$328 = _tmp$1436 / 10000ull;
      uint64_t _tmp$1435 = num$323;
      uint64_t _tmp$1434 = _tmp$1435 % 10000ull;
      int32_t r$329 = (int32_t)_tmp$1434;
      int32_t d1$330;
      int32_t d2$331;
      int32_t _tmp$1414;
      int32_t _tmp$1433;
      int32_t _tmp$1432;
      int32_t d1_hi$332;
      int32_t _tmp$1431;
      int32_t _tmp$1430;
      int32_t d1_lo$333;
      int32_t _tmp$1429;
      int32_t _tmp$1428;
      int32_t d2_hi$334;
      int32_t _tmp$1427;
      int32_t _tmp$1426;
      int32_t d2_lo$335;
      int32_t _tmp$1416;
      int32_t _tmp$1415;
      int32_t _tmp$1419;
      int32_t _tmp$1418;
      int32_t _tmp$1417;
      int32_t _tmp$1422;
      int32_t _tmp$1421;
      int32_t _tmp$1420;
      int32_t _tmp$1425;
      int32_t _tmp$1424;
      int32_t _tmp$1423;
      num$323 = t$328;
      d1$330 = r$329 / 100;
      d2$331 = r$329 % 100;
      _tmp$1414 = offset$325;
      offset$325 = _tmp$1414 - 4;
      _tmp$1433 = d1$330 / 10;
      _tmp$1432 = 48 + _tmp$1433;
      d1_hi$332 = (uint16_t)_tmp$1432;
      _tmp$1431 = d1$330 % 10;
      _tmp$1430 = 48 + _tmp$1431;
      d1_lo$333 = (uint16_t)_tmp$1430;
      _tmp$1429 = d2$331 / 10;
      _tmp$1428 = 48 + _tmp$1429;
      d2_hi$334 = (uint16_t)_tmp$1428;
      _tmp$1427 = d2$331 % 10;
      _tmp$1426 = 48 + _tmp$1427;
      d2_lo$335 = (uint16_t)_tmp$1426;
      _tmp$1416 = offset$325;
      _tmp$1415 = digit_start$327 + _tmp$1416;
      buffer$336[_tmp$1415] = d1_hi$332;
      _tmp$1419 = offset$325;
      _tmp$1418 = digit_start$327 + _tmp$1419;
      _tmp$1417 = _tmp$1418 + 1;
      buffer$336[_tmp$1417] = d1_lo$333;
      _tmp$1422 = offset$325;
      _tmp$1421 = digit_start$327 + _tmp$1422;
      _tmp$1420 = _tmp$1421 + 2;
      buffer$336[_tmp$1420] = d2_hi$334;
      _tmp$1425 = offset$325;
      _tmp$1424 = digit_start$327 + _tmp$1425;
      _tmp$1423 = _tmp$1424 + 3;
      buffer$336[_tmp$1423] = d2_lo$335;
      continue;
    }
    break;
  }
  _tmp$1469 = num$323;
  remaining$338 = (int32_t)_tmp$1469;
  while (1) {
    int32_t _tmp$1437 = remaining$338;
    if (_tmp$1437 >= 100) {
      int32_t _tmp$1449 = remaining$338;
      int32_t t$339 = _tmp$1449 / 100;
      int32_t _tmp$1448 = remaining$338;
      int32_t d$340 = _tmp$1448 % 100;
      int32_t _tmp$1438;
      int32_t _tmp$1447;
      int32_t _tmp$1446;
      int32_t d_hi$341;
      int32_t _tmp$1445;
      int32_t _tmp$1444;
      int32_t d_lo$342;
      int32_t _tmp$1440;
      int32_t _tmp$1439;
      int32_t _tmp$1443;
      int32_t _tmp$1442;
      int32_t _tmp$1441;
      remaining$338 = t$339;
      _tmp$1438 = offset$325;
      offset$325 = _tmp$1438 - 2;
      _tmp$1447 = d$340 / 10;
      _tmp$1446 = 48 + _tmp$1447;
      d_hi$341 = (uint16_t)_tmp$1446;
      _tmp$1445 = d$340 % 10;
      _tmp$1444 = 48 + _tmp$1445;
      d_lo$342 = (uint16_t)_tmp$1444;
      _tmp$1440 = offset$325;
      _tmp$1439 = digit_start$327 + _tmp$1440;
      buffer$336[_tmp$1439] = d_hi$341;
      _tmp$1443 = offset$325;
      _tmp$1442 = digit_start$327 + _tmp$1443;
      _tmp$1441 = _tmp$1442 + 1;
      buffer$336[_tmp$1441] = d_lo$342;
      continue;
    }
    break;
  }
  _tmp$1450 = remaining$338;
  if (_tmp$1450 >= 10) {
    int32_t _tmp$1451 = offset$325;
    int32_t _tmp$1462;
    int32_t _tmp$1461;
    int32_t _tmp$1460;
    int32_t d_hi$344;
    int32_t _tmp$1459;
    int32_t _tmp$1458;
    int32_t _tmp$1457;
    int32_t d_lo$345;
    int32_t _tmp$1453;
    int32_t _tmp$1452;
    int32_t _tmp$1456;
    int32_t _tmp$1455;
    int32_t _tmp$1454;
    offset$325 = _tmp$1451 - 2;
    _tmp$1462 = remaining$338;
    _tmp$1461 = _tmp$1462 / 10;
    _tmp$1460 = 48 + _tmp$1461;
    d_hi$344 = (uint16_t)_tmp$1460;
    _tmp$1459 = remaining$338;
    _tmp$1458 = _tmp$1459 % 10;
    _tmp$1457 = 48 + _tmp$1458;
    d_lo$345 = (uint16_t)_tmp$1457;
    _tmp$1453 = offset$325;
    _tmp$1452 = digit_start$327 + _tmp$1453;
    buffer$336[_tmp$1452] = d_hi$344;
    _tmp$1456 = offset$325;
    _tmp$1455 = digit_start$327 + _tmp$1456;
    _tmp$1454 = _tmp$1455 + 1;
    buffer$336[_tmp$1454] = d_lo$345;
    moonbit_decref(buffer$336);
  } else {
    int32_t _tmp$1463 = offset$325;
    int32_t _tmp$1468;
    int32_t _tmp$1464;
    int32_t _tmp$1467;
    int32_t _tmp$1466;
    int32_t _tmp$1465;
    offset$325 = _tmp$1463 - 1;
    _tmp$1468 = offset$325;
    _tmp$1464 = digit_start$327 + _tmp$1468;
    _tmp$1467 = remaining$338;
    _tmp$1466 = 48 + _tmp$1467;
    _tmp$1465 = (uint16_t)_tmp$1466;
    buffer$336[_tmp$1464] = _tmp$1465;
    moonbit_decref(buffer$336);
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int64_to_string_generic(
  uint16_t* buffer$318,
  uint64_t num$312,
  int32_t digit_start$310,
  int32_t total_len$309,
  int32_t radix$314
) {
  int32_t offset$308 = total_len$309 - digit_start$310;
  uint64_t n$311 = num$312;
  int64_t _tmp$1412 = (int64_t)radix$314;
  uint64_t base$313 = *(uint64_t*)&_tmp$1412;
  int32_t _tmp$1392 = radix$314 - 1;
  int32_t _tmp$1391 = radix$314 & _tmp$1392;
  if (_tmp$1391 == 0) {
    int32_t shift$315 = moonbit_ctz32(radix$314);
    uint64_t mask$316 = base$313 - 1ull;
    while (1) {
      uint64_t _tmp$1393 = n$311;
      if (_tmp$1393 > 0ull) {
        int32_t _tmp$1394 = offset$308;
        uint64_t _tmp$1401;
        uint64_t _tmp$1400;
        int32_t digit$317;
        int32_t _tmp$1398;
        int32_t _tmp$1395;
        int32_t _tmp$1397;
        int32_t _tmp$1396;
        uint64_t _tmp$1399;
        offset$308 = _tmp$1394 - 1;
        _tmp$1401 = n$311;
        _tmp$1400 = _tmp$1401 & mask$316;
        digit$317 = (int32_t)_tmp$1400;
        _tmp$1398 = offset$308;
        _tmp$1395 = digit_start$310 + _tmp$1398;
        _tmp$1397
        = ((moonbit_string_t)moonbit_string_literal_26.data)[
          digit$317
        ];
        _tmp$1396 = (uint16_t)_tmp$1397;
        buffer$318[_tmp$1395] = _tmp$1396;
        _tmp$1399 = n$311;
        n$311 = _tmp$1399 >> (shift$315 & 63);
        continue;
      } else {
        moonbit_decref(buffer$318);
      }
      break;
    }
  } else {
    while (1) {
      uint64_t _tmp$1402 = n$311;
      if (_tmp$1402 > 0ull) {
        int32_t _tmp$1403 = offset$308;
        uint64_t _tmp$1411;
        uint64_t q$320;
        uint64_t _tmp$1409;
        uint64_t _tmp$1410;
        uint64_t _tmp$1408;
        int32_t digit$321;
        int32_t _tmp$1407;
        int32_t _tmp$1404;
        int32_t _tmp$1406;
        int32_t _tmp$1405;
        offset$308 = _tmp$1403 - 1;
        _tmp$1411 = n$311;
        q$320 = _tmp$1411 / base$313;
        _tmp$1409 = n$311;
        _tmp$1410 = q$320 * base$313;
        _tmp$1408 = _tmp$1409 - _tmp$1410;
        digit$321 = (int32_t)_tmp$1408;
        _tmp$1407 = offset$308;
        _tmp$1404 = digit_start$310 + _tmp$1407;
        _tmp$1406
        = ((moonbit_string_t)moonbit_string_literal_26.data)[
          digit$321
        ];
        _tmp$1405 = (uint16_t)_tmp$1406;
        buffer$318[_tmp$1404] = _tmp$1405;
        n$311 = q$320;
        continue;
      } else {
        moonbit_decref(buffer$318);
      }
      break;
    }
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int64_to_string_hex(
  uint16_t* buffer$305,
  uint64_t num$301,
  int32_t digit_start$299,
  int32_t total_len$298
) {
  int32_t offset$297 = total_len$298 - digit_start$299;
  uint64_t n$300 = num$301;
  int32_t _tmp$1386;
  while (1) {
    int32_t _tmp$1372 = offset$297;
    if (_tmp$1372 >= 2) {
      int32_t _tmp$1373 = offset$297;
      uint64_t _tmp$1385;
      uint64_t _tmp$1384;
      int32_t byte_val$302;
      int32_t hi$303;
      int32_t lo$304;
      int32_t _tmp$1377;
      int32_t _tmp$1374;
      int32_t _tmp$1376;
      int32_t _tmp$1375;
      int32_t _tmp$1382;
      int32_t _tmp$1381;
      int32_t _tmp$1378;
      int32_t _tmp$1380;
      int32_t _tmp$1379;
      uint64_t _tmp$1383;
      offset$297 = _tmp$1373 - 2;
      _tmp$1385 = n$300;
      _tmp$1384 = _tmp$1385 & 255ull;
      byte_val$302 = (int32_t)_tmp$1384;
      hi$303 = byte_val$302 / 16;
      lo$304 = byte_val$302 % 16;
      _tmp$1377 = offset$297;
      _tmp$1374 = digit_start$299 + _tmp$1377;
      _tmp$1376 = ((moonbit_string_t)moonbit_string_literal_26.data)[hi$303];
      _tmp$1375 = (uint16_t)_tmp$1376;
      buffer$305[_tmp$1374] = _tmp$1375;
      _tmp$1382 = offset$297;
      _tmp$1381 = digit_start$299 + _tmp$1382;
      _tmp$1378 = _tmp$1381 + 1;
      _tmp$1380 = ((moonbit_string_t)moonbit_string_literal_26.data)[lo$304];
      _tmp$1379 = (uint16_t)_tmp$1380;
      buffer$305[_tmp$1378] = _tmp$1379;
      _tmp$1383 = n$300;
      n$300 = _tmp$1383 >> 8;
      continue;
    }
    break;
  }
  _tmp$1386 = offset$297;
  if (_tmp$1386 == 1) {
    uint64_t _tmp$1390 = n$300;
    uint64_t _tmp$1389 = _tmp$1390 & 15ull;
    int32_t nibble$307 = (int32_t)_tmp$1389;
    int32_t _tmp$1388 =
      ((moonbit_string_t)moonbit_string_literal_26.data)[nibble$307];
    int32_t _tmp$1387 = (uint16_t)_tmp$1388;
    buffer$305[digit_start$299] = _tmp$1387;
    moonbit_decref(buffer$305);
  } else {
    moonbit_decref(buffer$305);
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$radix_count64(
  uint64_t value$291,
  int32_t radix$294
) {
  uint64_t num$292;
  int64_t _tmp$1371;
  uint64_t base$293;
  int32_t count$295;
  if (value$291 == 0ull) {
    return 1;
  }
  num$292 = value$291;
  _tmp$1371 = (int64_t)radix$294;
  base$293 = *(uint64_t*)&_tmp$1371;
  count$295 = 0;
  while (1) {
    uint64_t _tmp$1368 = num$292;
    if (_tmp$1368 > 0ull) {
      int32_t _tmp$1369 = count$295;
      uint64_t _tmp$1370;
      count$295 = _tmp$1369 + 1;
      _tmp$1370 = num$292;
      num$292 = _tmp$1370 / base$293;
      continue;
    }
    break;
  }
  return count$295;
}

int32_t $moonbitlang$core$builtin$hex_count64(uint64_t value$289) {
  if (value$289 == 0ull) {
    return 1;
  } else {
    int32_t leading_zeros$290 = moonbit_clz64(value$289);
    int32_t _tmp$1367 = 63 - leading_zeros$290;
    int32_t _tmp$1366 = _tmp$1367 / 4;
    return _tmp$1366 + 1;
  }
}

int32_t $moonbitlang$core$builtin$dec_count64(uint64_t value$288) {
  if (value$288 >= 10000000000ull) {
    if (value$288 >= 100000000000000ull) {
      if (value$288 >= 10000000000000000ull) {
        if (value$288 >= 1000000000000000000ull) {
          if (value$288 >= 10000000000000000000ull) {
            return 20;
          } else {
            return 19;
          }
        } else if (value$288 >= 100000000000000000ull) {
          return 18;
        } else {
          return 17;
        }
      } else if (value$288 >= 1000000000000000ull) {
        return 16;
      } else {
        return 15;
      }
    } else if (value$288 >= 1000000000000ull) {
      if (value$288 >= 10000000000000ull) {
        return 14;
      } else {
        return 13;
      }
    } else if (value$288 >= 100000000000ull) {
      return 12;
    } else {
      return 11;
    }
  } else if (value$288 >= 100000ull) {
    if (value$288 >= 10000000ull) {
      if (value$288 >= 1000000000ull) {
        return 10;
      } else if (value$288 >= 100000000ull) {
        return 9;
      } else {
        return 8;
      }
    } else if (value$288 >= 1000000ull) {
      return 7;
    } else {
      return 6;
    }
  } else if (value$288 >= 1000ull) {
    if (value$288 >= 10000ull) {
      return 5;
    } else {
      return 4;
    }
  } else if (value$288 >= 100ull) {
    return 3;
  } else if (value$288 >= 10ull) {
    return 2;
  } else {
    return 1;
  }
}

moonbit_string_t $Int$$to_string$inner(int32_t self$272, int32_t radix$271) {
  int32_t is_negative$273;
  uint32_t num$274;
  uint16_t* buffer$275;
  if (radix$271 < 2 || radix$271 > 36) {
    $moonbitlang$core$builtin$abort$0(
      (moonbit_string_t)moonbit_string_literal_23.data,
        (moonbit_string_t)moonbit_string_literal_27.data
    );
  }
  if (self$272 == 0) {
    return (moonbit_string_t)moonbit_string_literal_25.data;
  }
  is_negative$273 = self$272 < 0;
  if (is_negative$273) {
    int32_t _tmp$1365 = -self$272;
    num$274 = *(uint32_t*)&_tmp$1365;
  } else {
    num$274 = *(uint32_t*)&self$272;
  }
  switch (radix$271) {
    case 10: {
      int32_t digit_len$276 = $moonbitlang$core$builtin$dec_count32(num$274);
      int32_t _tmp$1362;
      int32_t total_len$277;
      uint16_t* buffer$278;
      int32_t digit_start$279;
      if (is_negative$273) {
        _tmp$1362 = 1;
      } else {
        _tmp$1362 = 0;
      }
      total_len$277 = digit_len$276 + _tmp$1362;
      buffer$278 = (uint16_t*)moonbit_make_string(total_len$277, 0);
      if (is_negative$273) {
        digit_start$279 = 1;
      } else {
        digit_start$279 = 0;
      }
      moonbit_incref(buffer$278);
      $moonbitlang$core$builtin$int_to_string_dec(
        buffer$278, num$274, digit_start$279, total_len$277
      );
      buffer$275 = buffer$278;
      break;
    }
    
    case 16: {
      int32_t digit_len$280 = $moonbitlang$core$builtin$hex_count32(num$274);
      int32_t _tmp$1363;
      int32_t total_len$281;
      uint16_t* buffer$282;
      int32_t digit_start$283;
      if (is_negative$273) {
        _tmp$1363 = 1;
      } else {
        _tmp$1363 = 0;
      }
      total_len$281 = digit_len$280 + _tmp$1363;
      buffer$282 = (uint16_t*)moonbit_make_string(total_len$281, 0);
      if (is_negative$273) {
        digit_start$283 = 1;
      } else {
        digit_start$283 = 0;
      }
      moonbit_incref(buffer$282);
      $moonbitlang$core$builtin$int_to_string_hex(
        buffer$282, num$274, digit_start$283, total_len$281
      );
      buffer$275 = buffer$282;
      break;
    }
    default: {
      int32_t digit_len$284 =
        $moonbitlang$core$builtin$radix_count32(num$274, radix$271);
      int32_t _tmp$1364;
      int32_t total_len$285;
      uint16_t* buffer$286;
      int32_t digit_start$287;
      if (is_negative$273) {
        _tmp$1364 = 1;
      } else {
        _tmp$1364 = 0;
      }
      total_len$285 = digit_len$284 + _tmp$1364;
      buffer$286 = (uint16_t*)moonbit_make_string(total_len$285, 0);
      if (is_negative$273) {
        digit_start$287 = 1;
      } else {
        digit_start$287 = 0;
      }
      moonbit_incref(buffer$286);
      $moonbitlang$core$builtin$int_to_string_generic(
        buffer$286, num$274, digit_start$287, total_len$285, radix$271
      );
      buffer$275 = buffer$286;
      break;
    }
  }
  if (is_negative$273) {
    buffer$275[0] = 45;
  }
  return buffer$275;
}

int32_t $moonbitlang$core$builtin$radix_count32(
  uint32_t value$265,
  int32_t radix$268
) {
  uint32_t num$266;
  uint32_t base$267;
  int32_t count$269;
  if (value$265 == 0u) {
    return 1;
  }
  num$266 = value$265;
  base$267 = *(uint32_t*)&radix$268;
  count$269 = 0;
  while (1) {
    uint32_t _tmp$1359 = num$266;
    if (_tmp$1359 > 0u) {
      int32_t _tmp$1360 = count$269;
      uint32_t _tmp$1361;
      count$269 = _tmp$1360 + 1;
      _tmp$1361 = num$266;
      num$266 = _tmp$1361 / base$267;
      continue;
    }
    break;
  }
  return count$269;
}

int32_t $moonbitlang$core$builtin$hex_count32(uint32_t value$263) {
  if (value$263 == 0u) {
    return 1;
  } else {
    int32_t leading_zeros$264 = moonbit_clz32(value$263);
    int32_t _tmp$1358 = 31 - leading_zeros$264;
    int32_t _tmp$1357 = _tmp$1358 / 4;
    return _tmp$1357 + 1;
  }
}

int32_t $moonbitlang$core$builtin$dec_count32(uint32_t value$262) {
  if (value$262 >= 100000u) {
    if (value$262 >= 10000000u) {
      if (value$262 >= 1000000000u) {
        return 10;
      } else if (value$262 >= 100000000u) {
        return 9;
      } else {
        return 8;
      }
    } else if (value$262 >= 1000000u) {
      return 7;
    } else {
      return 6;
    }
  } else if (value$262 >= 1000u) {
    if (value$262 >= 10000u) {
      return 5;
    } else {
      return 4;
    }
  } else if (value$262 >= 100u) {
    return 3;
  } else if (value$262 >= 10u) {
    return 2;
  } else {
    return 1;
  }
}

int32_t $moonbitlang$core$builtin$int_to_string_dec(
  uint16_t* buffer$252,
  uint32_t num$240,
  int32_t digit_start$243,
  int32_t total_len$242
) {
  uint32_t num$239 = num$240;
  int32_t offset$241 = total_len$242 - digit_start$243;
  uint32_t _tmp$1356;
  int32_t remaining$254;
  int32_t _tmp$1337;
  while (1) {
    uint32_t _tmp$1300 = num$239;
    if (_tmp$1300 >= 10000u) {
      uint32_t _tmp$1323 = num$239;
      uint32_t t$244 = _tmp$1323 / 10000u;
      uint32_t _tmp$1322 = num$239;
      uint32_t _tmp$1321 = _tmp$1322 % 10000u;
      int32_t r$245 = *(int32_t*)&_tmp$1321;
      int32_t d1$246;
      int32_t d2$247;
      int32_t _tmp$1301;
      int32_t _tmp$1320;
      int32_t _tmp$1319;
      int32_t d1_hi$248;
      int32_t _tmp$1318;
      int32_t _tmp$1317;
      int32_t d1_lo$249;
      int32_t _tmp$1316;
      int32_t _tmp$1315;
      int32_t d2_hi$250;
      int32_t _tmp$1314;
      int32_t _tmp$1313;
      int32_t d2_lo$251;
      int32_t _tmp$1303;
      int32_t _tmp$1302;
      int32_t _tmp$1306;
      int32_t _tmp$1305;
      int32_t _tmp$1304;
      int32_t _tmp$1309;
      int32_t _tmp$1308;
      int32_t _tmp$1307;
      int32_t _tmp$1312;
      int32_t _tmp$1311;
      int32_t _tmp$1310;
      num$239 = t$244;
      d1$246 = r$245 / 100;
      d2$247 = r$245 % 100;
      _tmp$1301 = offset$241;
      offset$241 = _tmp$1301 - 4;
      _tmp$1320 = d1$246 / 10;
      _tmp$1319 = 48 + _tmp$1320;
      d1_hi$248 = (uint16_t)_tmp$1319;
      _tmp$1318 = d1$246 % 10;
      _tmp$1317 = 48 + _tmp$1318;
      d1_lo$249 = (uint16_t)_tmp$1317;
      _tmp$1316 = d2$247 / 10;
      _tmp$1315 = 48 + _tmp$1316;
      d2_hi$250 = (uint16_t)_tmp$1315;
      _tmp$1314 = d2$247 % 10;
      _tmp$1313 = 48 + _tmp$1314;
      d2_lo$251 = (uint16_t)_tmp$1313;
      _tmp$1303 = offset$241;
      _tmp$1302 = digit_start$243 + _tmp$1303;
      buffer$252[_tmp$1302] = d1_hi$248;
      _tmp$1306 = offset$241;
      _tmp$1305 = digit_start$243 + _tmp$1306;
      _tmp$1304 = _tmp$1305 + 1;
      buffer$252[_tmp$1304] = d1_lo$249;
      _tmp$1309 = offset$241;
      _tmp$1308 = digit_start$243 + _tmp$1309;
      _tmp$1307 = _tmp$1308 + 2;
      buffer$252[_tmp$1307] = d2_hi$250;
      _tmp$1312 = offset$241;
      _tmp$1311 = digit_start$243 + _tmp$1312;
      _tmp$1310 = _tmp$1311 + 3;
      buffer$252[_tmp$1310] = d2_lo$251;
      continue;
    }
    break;
  }
  _tmp$1356 = num$239;
  remaining$254 = *(int32_t*)&_tmp$1356;
  while (1) {
    int32_t _tmp$1324 = remaining$254;
    if (_tmp$1324 >= 100) {
      int32_t _tmp$1336 = remaining$254;
      int32_t t$255 = _tmp$1336 / 100;
      int32_t _tmp$1335 = remaining$254;
      int32_t d$256 = _tmp$1335 % 100;
      int32_t _tmp$1325;
      int32_t _tmp$1334;
      int32_t _tmp$1333;
      int32_t d_hi$257;
      int32_t _tmp$1332;
      int32_t _tmp$1331;
      int32_t d_lo$258;
      int32_t _tmp$1327;
      int32_t _tmp$1326;
      int32_t _tmp$1330;
      int32_t _tmp$1329;
      int32_t _tmp$1328;
      remaining$254 = t$255;
      _tmp$1325 = offset$241;
      offset$241 = _tmp$1325 - 2;
      _tmp$1334 = d$256 / 10;
      _tmp$1333 = 48 + _tmp$1334;
      d_hi$257 = (uint16_t)_tmp$1333;
      _tmp$1332 = d$256 % 10;
      _tmp$1331 = 48 + _tmp$1332;
      d_lo$258 = (uint16_t)_tmp$1331;
      _tmp$1327 = offset$241;
      _tmp$1326 = digit_start$243 + _tmp$1327;
      buffer$252[_tmp$1326] = d_hi$257;
      _tmp$1330 = offset$241;
      _tmp$1329 = digit_start$243 + _tmp$1330;
      _tmp$1328 = _tmp$1329 + 1;
      buffer$252[_tmp$1328] = d_lo$258;
      continue;
    }
    break;
  }
  _tmp$1337 = remaining$254;
  if (_tmp$1337 >= 10) {
    int32_t _tmp$1338 = offset$241;
    int32_t _tmp$1349;
    int32_t _tmp$1348;
    int32_t _tmp$1347;
    int32_t d_hi$260;
    int32_t _tmp$1346;
    int32_t _tmp$1345;
    int32_t _tmp$1344;
    int32_t d_lo$261;
    int32_t _tmp$1340;
    int32_t _tmp$1339;
    int32_t _tmp$1343;
    int32_t _tmp$1342;
    int32_t _tmp$1341;
    offset$241 = _tmp$1338 - 2;
    _tmp$1349 = remaining$254;
    _tmp$1348 = _tmp$1349 / 10;
    _tmp$1347 = 48 + _tmp$1348;
    d_hi$260 = (uint16_t)_tmp$1347;
    _tmp$1346 = remaining$254;
    _tmp$1345 = _tmp$1346 % 10;
    _tmp$1344 = 48 + _tmp$1345;
    d_lo$261 = (uint16_t)_tmp$1344;
    _tmp$1340 = offset$241;
    _tmp$1339 = digit_start$243 + _tmp$1340;
    buffer$252[_tmp$1339] = d_hi$260;
    _tmp$1343 = offset$241;
    _tmp$1342 = digit_start$243 + _tmp$1343;
    _tmp$1341 = _tmp$1342 + 1;
    buffer$252[_tmp$1341] = d_lo$261;
    moonbit_decref(buffer$252);
  } else {
    int32_t _tmp$1350 = offset$241;
    int32_t _tmp$1355;
    int32_t _tmp$1351;
    int32_t _tmp$1354;
    int32_t _tmp$1353;
    int32_t _tmp$1352;
    offset$241 = _tmp$1350 - 1;
    _tmp$1355 = offset$241;
    _tmp$1351 = digit_start$243 + _tmp$1355;
    _tmp$1354 = remaining$254;
    _tmp$1353 = 48 + _tmp$1354;
    _tmp$1352 = (uint16_t)_tmp$1353;
    buffer$252[_tmp$1351] = _tmp$1352;
    moonbit_decref(buffer$252);
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int_to_string_generic(
  uint16_t* buffer$234,
  uint32_t num$228,
  int32_t digit_start$226,
  int32_t total_len$225,
  int32_t radix$230
) {
  int32_t offset$224 = total_len$225 - digit_start$226;
  uint32_t n$227 = num$228;
  uint32_t base$229 = *(uint32_t*)&radix$230;
  int32_t _tmp$1280 = radix$230 - 1;
  int32_t _tmp$1279 = radix$230 & _tmp$1280;
  if (_tmp$1279 == 0) {
    int32_t shift$231 = moonbit_ctz32(radix$230);
    uint32_t mask$232 = base$229 - 1u;
    while (1) {
      uint32_t _tmp$1281 = n$227;
      if (_tmp$1281 > 0u) {
        int32_t _tmp$1282 = offset$224;
        uint32_t _tmp$1289;
        uint32_t _tmp$1288;
        int32_t digit$233;
        int32_t _tmp$1286;
        int32_t _tmp$1283;
        int32_t _tmp$1285;
        int32_t _tmp$1284;
        uint32_t _tmp$1287;
        offset$224 = _tmp$1282 - 1;
        _tmp$1289 = n$227;
        _tmp$1288 = _tmp$1289 & mask$232;
        digit$233 = *(int32_t*)&_tmp$1288;
        _tmp$1286 = offset$224;
        _tmp$1283 = digit_start$226 + _tmp$1286;
        _tmp$1285
        = ((moonbit_string_t)moonbit_string_literal_26.data)[
          digit$233
        ];
        _tmp$1284 = (uint16_t)_tmp$1285;
        buffer$234[_tmp$1283] = _tmp$1284;
        _tmp$1287 = n$227;
        n$227 = _tmp$1287 >> (shift$231 & 31);
        continue;
      } else {
        moonbit_decref(buffer$234);
      }
      break;
    }
  } else {
    while (1) {
      uint32_t _tmp$1290 = n$227;
      if (_tmp$1290 > 0u) {
        int32_t _tmp$1291 = offset$224;
        uint32_t _tmp$1299;
        uint32_t q$236;
        uint32_t _tmp$1297;
        uint32_t _tmp$1298;
        uint32_t _tmp$1296;
        int32_t digit$237;
        int32_t _tmp$1295;
        int32_t _tmp$1292;
        int32_t _tmp$1294;
        int32_t _tmp$1293;
        offset$224 = _tmp$1291 - 1;
        _tmp$1299 = n$227;
        q$236 = _tmp$1299 / base$229;
        _tmp$1297 = n$227;
        _tmp$1298 = q$236 * base$229;
        _tmp$1296 = _tmp$1297 - _tmp$1298;
        digit$237 = *(int32_t*)&_tmp$1296;
        _tmp$1295 = offset$224;
        _tmp$1292 = digit_start$226 + _tmp$1295;
        _tmp$1294
        = ((moonbit_string_t)moonbit_string_literal_26.data)[
          digit$237
        ];
        _tmp$1293 = (uint16_t)_tmp$1294;
        buffer$234[_tmp$1292] = _tmp$1293;
        n$227 = q$236;
        continue;
      } else {
        moonbit_decref(buffer$234);
      }
      break;
    }
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int_to_string_hex(
  uint16_t* buffer$221,
  uint32_t num$217,
  int32_t digit_start$215,
  int32_t total_len$214
) {
  int32_t offset$213 = total_len$214 - digit_start$215;
  uint32_t n$216 = num$217;
  int32_t _tmp$1274;
  while (1) {
    int32_t _tmp$1260 = offset$213;
    if (_tmp$1260 >= 2) {
      int32_t _tmp$1261 = offset$213;
      uint32_t _tmp$1273;
      uint32_t _tmp$1272;
      int32_t byte_val$218;
      int32_t hi$219;
      int32_t lo$220;
      int32_t _tmp$1265;
      int32_t _tmp$1262;
      int32_t _tmp$1264;
      int32_t _tmp$1263;
      int32_t _tmp$1270;
      int32_t _tmp$1269;
      int32_t _tmp$1266;
      int32_t _tmp$1268;
      int32_t _tmp$1267;
      uint32_t _tmp$1271;
      offset$213 = _tmp$1261 - 2;
      _tmp$1273 = n$216;
      _tmp$1272 = _tmp$1273 & 255u;
      byte_val$218 = *(int32_t*)&_tmp$1272;
      hi$219 = byte_val$218 / 16;
      lo$220 = byte_val$218 % 16;
      _tmp$1265 = offset$213;
      _tmp$1262 = digit_start$215 + _tmp$1265;
      _tmp$1264 = ((moonbit_string_t)moonbit_string_literal_26.data)[hi$219];
      _tmp$1263 = (uint16_t)_tmp$1264;
      buffer$221[_tmp$1262] = _tmp$1263;
      _tmp$1270 = offset$213;
      _tmp$1269 = digit_start$215 + _tmp$1270;
      _tmp$1266 = _tmp$1269 + 1;
      _tmp$1268 = ((moonbit_string_t)moonbit_string_literal_26.data)[lo$220];
      _tmp$1267 = (uint16_t)_tmp$1268;
      buffer$221[_tmp$1266] = _tmp$1267;
      _tmp$1271 = n$216;
      n$216 = _tmp$1271 >> 8;
      continue;
    }
    break;
  }
  _tmp$1274 = offset$213;
  if (_tmp$1274 == 1) {
    uint32_t _tmp$1278 = n$216;
    uint32_t _tmp$1277 = _tmp$1278 & 15u;
    int32_t nibble$223 = *(int32_t*)&_tmp$1277;
    int32_t _tmp$1276 =
      ((moonbit_string_t)moonbit_string_literal_26.data)[nibble$223];
    int32_t _tmp$1275 = (uint16_t)_tmp$1276;
    buffer$221[digit_start$215] = _tmp$1275;
    moonbit_decref(buffer$221);
  } else {
    moonbit_decref(buffer$221);
  }
  return 0;
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  int32_t self$212
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$211 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1259;
  moonbit_incref(logger$211);
  _tmp$1259
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$211
  };
  $$moonbitlang$core$builtin$Show$$Int$$output(self$212, _tmp$1259);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$211);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  moonbit_string_t self$210
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$209 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1258;
  moonbit_incref(logger$209);
  _tmp$1258
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$209
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
    self$210, _tmp$1258
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$209);
}

int32_t $StringView$$start_offset(struct $StringView self$208) {
  int32_t _field$2098 = self$208.$1;
  moonbit_decref(self$208.$0);
  return _field$2098;
}

int32_t $StringView$$length(struct $StringView self$207) {
  int32_t end$1256 = self$207.$2;
  int32_t _field$2099 = self$207.$1;
  int32_t start$1257;
  moonbit_decref(self$207.$0);
  start$1257 = _field$2099;
  return end$1256 - start$1257;
}

moonbit_string_t $StringView$$data(struct $StringView self$206) {
  moonbit_string_t _field$2100 = self$206.$0;
  return _field$2100;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$200,
  moonbit_string_t value$203,
  int32_t start$204,
  int32_t len$205
) {
  void* _try_err$202;
  struct $StringView _tmp$1251;
  int32_t _tmp$1253 = start$204 + len$205;
  int64_t _tmp$1252 = (int64_t)_tmp$1253;
  struct moonbit_result_0 _tmp$2231 =
    $String$$sub$inner(value$203, start$204, _tmp$1252);
  if (_tmp$2231.tag) {
    struct $StringView const _ok$1254 = _tmp$2231.data.ok;
    _tmp$1251 = _ok$1254;
  } else {
    void* const _err$1255 = _tmp$2231.data.err;
    _try_err$202 = _err$1255;
    goto $join$201;
  }
  goto $joinlet$2230;
  $join$201:;
  moonbit_decref(_try_err$202);
  moonbit_panic();
  $joinlet$2230:;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    self$200, _tmp$1251
  );
  return 0;
}

struct moonbit_result_0 $String$$sub(
  moonbit_string_t self$198,
  int64_t start$opt$196,
  int64_t end$199
) {
  int32_t start$195;
  if (start$opt$196 == 4294967296ll) {
    start$195 = 0;
  } else {
    int64_t _Some$197 = start$opt$196;
    start$195 = (int32_t)_Some$197;
  }
  return $String$$sub$inner(self$198, start$195, end$199);
}

struct moonbit_result_0 $String$$sub$inner(
  moonbit_string_t self$188,
  int32_t start$194,
  int64_t end$190
) {
  int32_t len$187 = Moonbit_array_length(self$188);
  int32_t end$189;
  int32_t start$193;
  if (end$190 == 4294967296ll) {
    end$189 = len$187;
  } else {
    int64_t _Some$191 = end$190;
    int32_t _end$192 = (int32_t)_Some$191;
    if (_end$192 < 0) {
      end$189 = len$187 + _end$192;
    } else {
      end$189 = _end$192;
    }
  }
  if (start$194 < 0) {
    start$193 = len$187 + start$194;
  } else {
    start$193 = start$194;
  }
  if (start$193 >= 0 && start$193 <= end$189 && end$189 <= len$187) {
    int32_t _if_result$2232;
    int32_t _if_result$2234;
    struct $StringView _tmp$1249;
    struct moonbit_result_0 _result$2236;
    if (start$193 < len$187) {
      int32_t _p$827 = self$188[start$193];
      _if_result$2232 = 56320 <= _p$827 && _p$827 <= 57343;
    } else {
      _if_result$2232 = 0;
    }
    if (_if_result$2232) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1247;
      struct moonbit_result_0 _result$2233;
      moonbit_decref(self$188);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1247
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$2233.tag = 0;
      _result$2233.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1247;
      return _result$2233;
    }
    if (end$189 < len$187) {
      int32_t _p$830 = self$188[end$189];
      _if_result$2234 = 56320 <= _p$830 && _p$830 <= 57343;
    } else {
      _if_result$2234 = 0;
    }
    if (_if_result$2234) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1248;
      struct moonbit_result_0 _result$2235;
      moonbit_decref(self$188);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1248
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$2235.tag = 0;
      _result$2235.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1248;
      return _result$2235;
    }
    _tmp$1249 = (struct $StringView){start$193, end$189, self$188};
    _result$2236.tag = 1;
    _result$2236.data.ok = _tmp$1249;
    return _result$2236;
  } else {
    void* moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1250;
    struct moonbit_result_0 _result$2237;
    moonbit_decref(self$188);
    moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1250
    = (struct moonbit_object*)&moonbit_constant_constructor_1 + 1;
    _result$2237.tag = 0;
    _result$2237.data.err
    = moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1250;
    return _result$2237;
  }
}

uint64_t $Int$$to_uint64(int32_t self$186) {
  int64_t _tmp$1246 = (int64_t)self$186;
  return *(uint64_t*)&_tmp$1246;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$184,
  moonbit_string_t str$185
) {
  int32_t len$1236 = self$184->$1;
  int32_t _tmp$1238 = Moonbit_array_length(str$185);
  int32_t _tmp$1237 = _tmp$1238 * 2;
  int32_t _tmp$1235 = len$1236 + _tmp$1237;
  moonbit_bytes_t _field$2102;
  moonbit_bytes_t data$1239;
  int32_t len$1240;
  int32_t _tmp$1241;
  int32_t len$1243;
  int32_t _tmp$2101;
  int32_t _tmp$1245;
  int32_t _tmp$1244;
  int32_t _tmp$1242;
  moonbit_incref(self$184);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$184, _tmp$1235
  );
  _field$2102 = self$184->$0;
  data$1239 = _field$2102;
  len$1240 = self$184->$1;
  _tmp$1241 = Moonbit_array_length(str$185);
  moonbit_incref(data$1239);
  moonbit_incref(str$185);
  $FixedArray$$blit_from_string(data$1239, len$1240, str$185, 0, _tmp$1241);
  len$1243 = self$184->$1;
  _tmp$2101 = Moonbit_array_length(str$185);
  moonbit_decref(str$185);
  _tmp$1245 = _tmp$2101;
  _tmp$1244 = _tmp$1245 * 2;
  _tmp$1242 = len$1243 + _tmp$1244;
  self$184->$1 = _tmp$1242;
  moonbit_decref(self$184);
  return 0;
}

int32_t $FixedArray$$blit_from_string(
  moonbit_bytes_t self$176,
  int32_t bytes_offset$171,
  moonbit_string_t str$178,
  int32_t str_offset$174,
  int32_t length$172
) {
  int32_t _tmp$1234 = length$172 * 2;
  int32_t _tmp$1233 = bytes_offset$171 + _tmp$1234;
  int32_t e1$170 = _tmp$1233 - 1;
  int32_t _tmp$1232 = str_offset$174 + length$172;
  int32_t e2$173 = _tmp$1232 - 1;
  int32_t len1$175 = Moonbit_array_length(self$176);
  int32_t len2$177 = Moonbit_array_length(str$178);
  if (
    length$172 >= 0
    && bytes_offset$171 >= 0
    && e1$170 < len1$175
    && str_offset$174 >= 0
    && e2$173 < len2$177
  ) {
    int32_t end_str_offset$179 = str_offset$174 + length$172;
    int32_t i$180 = str_offset$174;
    int32_t j$181 = bytes_offset$171;
    while (1) {
      if (i$180 < end_str_offset$179) {
        int32_t _tmp$1229 = str$178[i$180];
        uint32_t c$182 = *(uint32_t*)&_tmp$1229;
        uint32_t _p$821 = c$182 & 255u;
        int32_t _tmp$1225 = *(int32_t*)&_p$821;
        int32_t _tmp$1224 = _tmp$1225 & 0xff;
        int32_t _tmp$1226;
        uint32_t _p$824;
        int32_t _tmp$1228;
        int32_t _tmp$1227;
        int32_t _tmp$1230;
        int32_t _tmp$1231;
        if (j$181 < 0 || j$181 >= Moonbit_array_length(self$176)) {
          moonbit_panic();
        }
        self$176[j$181] = _tmp$1224;
        _tmp$1226 = j$181 + 1;
        _p$824 = c$182 >> 8;
        _tmp$1228 = *(int32_t*)&_p$824;
        _tmp$1227 = _tmp$1228 & 0xff;
        if (_tmp$1226 < 0 || _tmp$1226 >= Moonbit_array_length(self$176)) {
          moonbit_panic();
        }
        self$176[_tmp$1226] = _tmp$1227;
        _tmp$1230 = i$180 + 1;
        _tmp$1231 = j$181 + 2;
        i$180 = _tmp$1230;
        j$181 = _tmp$1231;
        continue;
      } else {
        moonbit_decref(str$178);
        moonbit_decref(self$176);
      }
      break;
    }
  } else {
    moonbit_decref(str$178);
    moonbit_decref(self$176);
    moonbit_panic();
  }
  return 0;
}

struct $$moonbitlang$core$builtin$SourceLocRepr* $$moonbitlang$core$builtin$SourceLocRepr$$parse(
  moonbit_string_t repr$93
) {
  int32_t _tmp$1223 = Moonbit_array_length(repr$93);
  struct $StringView _bind$92 = (struct $StringView){0, _tmp$1223, repr$93};
  moonbit_string_t _field$2104 = _bind$92.$0;
  moonbit_string_t _data$94 = _field$2104;
  int32_t _start$95 = _bind$92.$1;
  int32_t end$1221 = _bind$92.$2;
  int32_t _field$2103 = _bind$92.$1;
  int32_t start$1222 = _field$2103;
  int32_t _tmp$1220 = end$1221 - start$1222;
  int32_t _end$96 = _start$95 + _tmp$1220;
  int32_t _cursor$97 = _start$95;
  int32_t accept_state$98 = -1;
  int32_t match_end$99 = -1;
  int32_t match_tag_saver_0$100 = -1;
  int32_t match_tag_saver_1$101 = -1;
  int32_t match_tag_saver_2$102 = -1;
  int32_t match_tag_saver_3$103 = -1;
  int32_t match_tag_saver_4$104 = -1;
  int32_t tag_0$105 = -1;
  int32_t tag_1$106 = -1;
  int32_t tag_1_1$107 = -1;
  int32_t tag_1_2$108 = -1;
  int32_t tag_3$109 = -1;
  int32_t tag_2$110 = -1;
  int32_t tag_2_1$111 = -1;
  int32_t tag_4$112 = -1;
  int32_t join_dispatch_19$133;
  int32_t _tmp$1210 = _cursor$97;
  int32_t dispatch_19$134;
  if (_tmp$1210 < _end$96) {
    int32_t _p$767 = _cursor$97;
    int32_t next_char$162 = _data$94[_p$767];
    int32_t _tmp$1211 = _cursor$97;
    _cursor$97 = _tmp$1211 + 1;
    if (next_char$162 < 65) {
      if (next_char$162 < 64) {
        goto $join$113;
      } else {
        while (1) {
          int32_t _tmp$1212;
          tag_0$105 = _cursor$97;
          _tmp$1212 = _cursor$97;
          if (_tmp$1212 < _end$96) {
            int32_t _p$770 = _cursor$97;
            int32_t next_char$165 = _data$94[_p$770];
            int32_t _tmp$1213 = _cursor$97;
            _cursor$97 = _tmp$1213 + 1;
            if (next_char$165 < 55296) {
              if (next_char$165 < 58) {
                goto $join$163;
              } else if (next_char$165 > 58) {
                goto $join$163;
              } else {
                int32_t _tmp$1214 = _cursor$97;
                if (_tmp$1214 < _end$96) {
                  int32_t _p$773 = _cursor$97;
                  int32_t next_char$167 = _data$94[_p$773];
                  int32_t _tmp$1215 = _cursor$97;
                  _cursor$97 = _tmp$1215 + 1;
                  if (next_char$167 < 56319) {
                    if (next_char$167 < 55296) {
                      goto $join$166;
                    } else {
                      join_dispatch_19$133 = 7;
                      goto $join$132;
                    }
                  } else if (next_char$167 > 56319) {
                    if (next_char$167 < 65536) {
                      goto $join$166;
                    } else {
                      goto $join$113;
                    }
                  } else {
                    join_dispatch_19$133 = 8;
                    goto $join$132;
                  }
                  $join$166:;
                  join_dispatch_19$133 = 0;
                  goto $join$132;
                } else {
                  goto $join$113;
                }
              }
            } else if (next_char$165 > 56318) {
              if (next_char$165 < 57344) {
                int32_t _tmp$1216 = _cursor$97;
                if (_tmp$1216 < _end$96) {
                  int32_t _p$776 = _cursor$97;
                  int32_t next_char$168 = _data$94[_p$776];
                  int32_t _tmp$1217 = _cursor$97;
                  _cursor$97 = _tmp$1217 + 1;
                  if (next_char$168 < 56320) {
                    goto $join$113;
                  } else if (next_char$168 > 57343) {
                    goto $join$113;
                  } else {
                    continue;
                  }
                } else {
                  goto $join$113;
                }
              } else if (next_char$165 > 65535) {
                goto $join$113;
              } else {
                goto $join$163;
              }
            } else {
              int32_t _tmp$1218 = _cursor$97;
              if (_tmp$1218 < _end$96) {
                int32_t _p$779 = _cursor$97;
                int32_t next_char$169 = _data$94[_p$779];
                int32_t _tmp$1219 = _cursor$97;
                _cursor$97 = _tmp$1219 + 1;
                if (next_char$169 < 56320) {
                  goto $join$113;
                } else if (next_char$169 > 65535) {
                  goto $join$113;
                } else {
                  continue;
                }
              } else {
                goto $join$113;
              }
            }
            $join$163:;
            continue;
          } else {
            goto $join$113;
          }
          break;
        }
      }
    } else {
      goto $join$113;
    }
  } else {
    goto $join$113;
  }
  $join$132:;
  dispatch_19$134 = join_dispatch_19$133;
  $loop_label_19$137:;
  while (1) {
    int32_t _tmp$1184;
    switch (dispatch_19$134) {
      case 3: {
        int32_t _tmp$1186;
        tag_1_2$108 = tag_1_1$107;
        tag_1_1$107 = tag_1$106;
        tag_1$106 = _cursor$97;
        _tmp$1186 = _cursor$97;
        if (_tmp$1186 < _end$96) {
          int32_t _p$782 = _cursor$97;
          int32_t next_char$141 = _data$94[_p$782];
          int32_t _tmp$1187 = _cursor$97;
          _cursor$97 = _tmp$1187 + 1;
          if (next_char$141 < 55296) {
            if (next_char$141 < 58) {
              if (next_char$141 < 48) {
                goto $join$140;
              } else {
                int32_t _tmp$1188;
                tag_1$106 = _cursor$97;
                tag_2_1$111 = tag_2$110;
                tag_2$110 = _cursor$97;
                tag_3$109 = _cursor$97;
                _tmp$1188 = _cursor$97;
                if (_tmp$1188 < _end$96) {
                  int32_t _p$785 = _cursor$97;
                  int32_t next_char$143 = _data$94[_p$785];
                  int32_t _tmp$1189 = _cursor$97;
                  _cursor$97 = _tmp$1189 + 1;
                  if (next_char$143 < 59) {
                    if (next_char$143 < 46) {
                      if (next_char$143 < 45) {
                        goto $join$142;
                      } else {
                        goto $join$135;
                      }
                    } else if (next_char$143 > 47) {
                      if (next_char$143 < 58) {
                        dispatch_19$134 = 6;
                        goto $loop_label_19$137;
                      } else {
                        dispatch_19$134 = 3;
                        goto $loop_label_19$137;
                      }
                    } else {
                      goto $join$142;
                    }
                  } else if (next_char$143 > 55295) {
                    if (next_char$143 < 57344) {
                      if (next_char$143 < 56319) {
                        dispatch_19$134 = 7;
                        goto $loop_label_19$137;
                      } else {
                        dispatch_19$134 = 8;
                        goto $loop_label_19$137;
                      }
                    } else if (next_char$143 > 65535) {
                      goto $join$113;
                    } else {
                      goto $join$142;
                    }
                  } else {
                    goto $join$142;
                  }
                  $join$142:;
                  dispatch_19$134 = 0;
                  goto $loop_label_19$137;
                } else {
                  goto $join$113;
                }
              }
            } else if (next_char$141 > 58) {
              goto $join$140;
            } else {
              dispatch_19$134 = 1;
              goto $loop_label_19$137;
            }
          } else if (next_char$141 > 56318) {
            if (next_char$141 < 57344) {
              dispatch_19$134 = 8;
              goto $loop_label_19$137;
            } else if (next_char$141 > 65535) {
              goto $join$113;
            } else {
              goto $join$140;
            }
          } else {
            dispatch_19$134 = 7;
            goto $loop_label_19$137;
          }
          $join$140:;
          dispatch_19$134 = 0;
          goto $loop_label_19$137;
        } else {
          goto $join$113;
        }
        break;
      }
      
      case 2: {
        int32_t _tmp$1190;
        tag_1$106 = _cursor$97;
        tag_2$110 = _cursor$97;
        _tmp$1190 = _cursor$97;
        if (_tmp$1190 < _end$96) {
          int32_t _p$788 = _cursor$97;
          int32_t next_char$145 = _data$94[_p$788];
          int32_t _tmp$1191 = _cursor$97;
          _cursor$97 = _tmp$1191 + 1;
          if (next_char$145 < 55296) {
            if (next_char$145 < 58) {
              if (next_char$145 < 48) {
                goto $join$144;
              } else {
                dispatch_19$134 = 2;
                goto $loop_label_19$137;
              }
            } else if (next_char$145 > 58) {
              goto $join$144;
            } else {
              dispatch_19$134 = 3;
              goto $loop_label_19$137;
            }
          } else if (next_char$145 > 56318) {
            if (next_char$145 < 57344) {
              dispatch_19$134 = 8;
              goto $loop_label_19$137;
            } else if (next_char$145 > 65535) {
              goto $join$113;
            } else {
              goto $join$144;
            }
          } else {
            dispatch_19$134 = 7;
            goto $loop_label_19$137;
          }
          $join$144:;
          dispatch_19$134 = 0;
          goto $loop_label_19$137;
        } else {
          goto $join$113;
        }
        break;
      }
      
      case 0: {
        int32_t _tmp$1192;
        tag_1$106 = _cursor$97;
        _tmp$1192 = _cursor$97;
        if (_tmp$1192 < _end$96) {
          int32_t _p$791 = _cursor$97;
          int32_t next_char$147 = _data$94[_p$791];
          int32_t _tmp$1193 = _cursor$97;
          _cursor$97 = _tmp$1193 + 1;
          if (next_char$147 < 55296) {
            if (next_char$147 < 58) {
              goto $join$146;
            } else if (next_char$147 > 58) {
              goto $join$146;
            } else {
              dispatch_19$134 = 1;
              goto $loop_label_19$137;
            }
          } else if (next_char$147 > 56318) {
            if (next_char$147 < 57344) {
              dispatch_19$134 = 8;
              goto $loop_label_19$137;
            } else if (next_char$147 > 65535) {
              goto $join$113;
            } else {
              goto $join$146;
            }
          } else {
            dispatch_19$134 = 7;
            goto $loop_label_19$137;
          }
          $join$146:;
          dispatch_19$134 = 0;
          goto $loop_label_19$137;
        } else {
          goto $join$113;
        }
        break;
      }
      
      case 8: {
        int32_t _tmp$1194 = _cursor$97;
        if (_tmp$1194 < _end$96) {
          int32_t _p$794 = _cursor$97;
          int32_t next_char$148 = _data$94[_p$794];
          int32_t _tmp$1195 = _cursor$97;
          _cursor$97 = _tmp$1195 + 1;
          if (next_char$148 < 56320) {
            goto $join$113;
          } else if (next_char$148 > 57343) {
            goto $join$113;
          } else {
            dispatch_19$134 = 0;
            goto $loop_label_19$137;
          }
        } else {
          goto $join$113;
        }
        break;
      }
      
      case 4: {
        int32_t _tmp$1196;
        tag_1$106 = _cursor$97;
        tag_4$112 = _cursor$97;
        _tmp$1196 = _cursor$97;
        if (_tmp$1196 < _end$96) {
          int32_t _p$797 = _cursor$97;
          int32_t next_char$150 = _data$94[_p$797];
          int32_t _tmp$1197 = _cursor$97;
          _cursor$97 = _tmp$1197 + 1;
          if (next_char$150 < 55296) {
            if (next_char$150 < 58) {
              if (next_char$150 < 48) {
                goto $join$149;
              } else {
                dispatch_19$134 = 4;
                goto $loop_label_19$137;
              }
            } else if (next_char$150 > 58) {
              goto $join$149;
            } else {
              int32_t _tmp$1198;
              tag_1_2$108 = tag_1_1$107;
              tag_1_1$107 = tag_1$106;
              tag_1$106 = _cursor$97;
              _tmp$1198 = _cursor$97;
              if (_tmp$1198 < _end$96) {
                int32_t _p$800 = _cursor$97;
                int32_t next_char$152 = _data$94[_p$800];
                int32_t _tmp$1199 = _cursor$97;
                _cursor$97 = _tmp$1199 + 1;
                if (next_char$152 < 55296) {
                  if (next_char$152 < 58) {
                    if (next_char$152 < 48) {
                      goto $join$151;
                    } else {
                      int32_t _tmp$1200;
                      tag_1$106 = _cursor$97;
                      tag_2_1$111 = tag_2$110;
                      tag_2$110 = _cursor$97;
                      _tmp$1200 = _cursor$97;
                      if (_tmp$1200 < _end$96) {
                        int32_t _p$803 = _cursor$97;
                        int32_t next_char$154 = _data$94[_p$803];
                        int32_t _tmp$1201 = _cursor$97;
                        _cursor$97 = _tmp$1201 + 1;
                        if (next_char$154 < 55296) {
                          if (next_char$154 < 58) {
                            if (next_char$154 < 48) {
                              goto $join$153;
                            } else {
                              dispatch_19$134 = 5;
                              goto $loop_label_19$137;
                            }
                          } else if (next_char$154 > 58) {
                            goto $join$153;
                          } else {
                            dispatch_19$134 = 3;
                            goto $loop_label_19$137;
                          }
                        } else if (next_char$154 > 56318) {
                          if (next_char$154 < 57344) {
                            dispatch_19$134 = 8;
                            goto $loop_label_19$137;
                          } else if (next_char$154 > 65535) {
                            goto $join$113;
                          } else {
                            goto $join$153;
                          }
                        } else {
                          dispatch_19$134 = 7;
                          goto $loop_label_19$137;
                        }
                        $join$153:;
                        dispatch_19$134 = 0;
                        goto $loop_label_19$137;
                      } else {
                        goto $join$139;
                      }
                    }
                  } else if (next_char$152 > 58) {
                    goto $join$151;
                  } else {
                    dispatch_19$134 = 1;
                    goto $loop_label_19$137;
                  }
                } else if (next_char$152 > 56318) {
                  if (next_char$152 < 57344) {
                    dispatch_19$134 = 8;
                    goto $loop_label_19$137;
                  } else if (next_char$152 > 65535) {
                    goto $join$113;
                  } else {
                    goto $join$151;
                  }
                } else {
                  dispatch_19$134 = 7;
                  goto $loop_label_19$137;
                }
                $join$151:;
                dispatch_19$134 = 0;
                goto $loop_label_19$137;
              } else {
                goto $join$113;
              }
            }
          } else if (next_char$150 > 56318) {
            if (next_char$150 < 57344) {
              dispatch_19$134 = 8;
              goto $loop_label_19$137;
            } else if (next_char$150 > 65535) {
              goto $join$113;
            } else {
              goto $join$149;
            }
          } else {
            dispatch_19$134 = 7;
            goto $loop_label_19$137;
          }
          $join$149:;
          dispatch_19$134 = 0;
          goto $loop_label_19$137;
        } else {
          goto $join$113;
        }
        break;
      }
      
      case 5: {
        int32_t _tmp$1202;
        tag_1$106 = _cursor$97;
        tag_2$110 = _cursor$97;
        _tmp$1202 = _cursor$97;
        if (_tmp$1202 < _end$96) {
          int32_t _p$806 = _cursor$97;
          int32_t next_char$156 = _data$94[_p$806];
          int32_t _tmp$1203 = _cursor$97;
          _cursor$97 = _tmp$1203 + 1;
          if (next_char$156 < 55296) {
            if (next_char$156 < 58) {
              if (next_char$156 < 48) {
                goto $join$155;
              } else {
                dispatch_19$134 = 5;
                goto $loop_label_19$137;
              }
            } else if (next_char$156 > 58) {
              goto $join$155;
            } else {
              dispatch_19$134 = 3;
              goto $loop_label_19$137;
            }
          } else if (next_char$156 > 56318) {
            if (next_char$156 < 57344) {
              dispatch_19$134 = 8;
              goto $loop_label_19$137;
            } else if (next_char$156 > 65535) {
              goto $join$113;
            } else {
              goto $join$155;
            }
          } else {
            dispatch_19$134 = 7;
            goto $loop_label_19$137;
          }
          $join$155:;
          dispatch_19$134 = 0;
          goto $loop_label_19$137;
        } else {
          goto $join$139;
        }
        break;
      }
      
      case 6: {
        int32_t _tmp$1204;
        tag_1$106 = _cursor$97;
        tag_2$110 = _cursor$97;
        tag_3$109 = _cursor$97;
        _tmp$1204 = _cursor$97;
        if (_tmp$1204 < _end$96) {
          int32_t _p$809 = _cursor$97;
          int32_t next_char$158 = _data$94[_p$809];
          int32_t _tmp$1205 = _cursor$97;
          _cursor$97 = _tmp$1205 + 1;
          if (next_char$158 < 59) {
            if (next_char$158 < 46) {
              if (next_char$158 < 45) {
                goto $join$157;
              } else {
                goto $join$135;
              }
            } else if (next_char$158 > 47) {
              if (next_char$158 < 58) {
                dispatch_19$134 = 6;
                goto $loop_label_19$137;
              } else {
                dispatch_19$134 = 3;
                goto $loop_label_19$137;
              }
            } else {
              goto $join$157;
            }
          } else if (next_char$158 > 55295) {
            if (next_char$158 < 57344) {
              if (next_char$158 < 56319) {
                dispatch_19$134 = 7;
                goto $loop_label_19$137;
              } else {
                dispatch_19$134 = 8;
                goto $loop_label_19$137;
              }
            } else if (next_char$158 > 65535) {
              goto $join$113;
            } else {
              goto $join$157;
            }
          } else {
            goto $join$157;
          }
          $join$157:;
          dispatch_19$134 = 0;
          goto $loop_label_19$137;
        } else {
          goto $join$113;
        }
        break;
      }
      
      case 7: {
        int32_t _tmp$1206 = _cursor$97;
        if (_tmp$1206 < _end$96) {
          int32_t _p$812 = _cursor$97;
          int32_t next_char$159 = _data$94[_p$812];
          int32_t _tmp$1207 = _cursor$97;
          _cursor$97 = _tmp$1207 + 1;
          if (next_char$159 < 56320) {
            goto $join$113;
          } else if (next_char$159 > 65535) {
            goto $join$113;
          } else {
            dispatch_19$134 = 0;
            goto $loop_label_19$137;
          }
        } else {
          goto $join$113;
        }
        break;
      }
      
      case 1: {
        int32_t _tmp$1208;
        tag_1_1$107 = tag_1$106;
        tag_1$106 = _cursor$97;
        _tmp$1208 = _cursor$97;
        if (_tmp$1208 < _end$96) {
          int32_t _p$815 = _cursor$97;
          int32_t next_char$161 = _data$94[_p$815];
          int32_t _tmp$1209 = _cursor$97;
          _cursor$97 = _tmp$1209 + 1;
          if (next_char$161 < 55296) {
            if (next_char$161 < 58) {
              if (next_char$161 < 48) {
                goto $join$160;
              } else {
                dispatch_19$134 = 2;
                goto $loop_label_19$137;
              }
            } else if (next_char$161 > 58) {
              goto $join$160;
            } else {
              dispatch_19$134 = 1;
              goto $loop_label_19$137;
            }
          } else if (next_char$161 > 56318) {
            if (next_char$161 < 57344) {
              dispatch_19$134 = 8;
              goto $loop_label_19$137;
            } else if (next_char$161 > 65535) {
              goto $join$113;
            } else {
              goto $join$160;
            }
          } else {
            dispatch_19$134 = 7;
            goto $loop_label_19$137;
          }
          $join$160:;
          dispatch_19$134 = 0;
          goto $loop_label_19$137;
        } else {
          goto $join$113;
        }
        break;
      }
      default: {
        goto $join$113;
        break;
      }
    }
    $join$139:;
    tag_1$106 = tag_1_2$108;
    tag_2$110 = tag_2_1$111;
    match_tag_saver_0$100 = tag_0$105;
    match_tag_saver_1$101 = tag_1$106;
    match_tag_saver_2$102 = tag_2$110;
    match_tag_saver_3$103 = tag_3$109;
    match_tag_saver_4$104 = tag_4$112;
    accept_state$98 = 0;
    match_end$99 = _cursor$97;
    goto $join$113;
    $join$135:;
    tag_1_1$107 = tag_1_2$108;
    tag_1$106 = _cursor$97;
    tag_2$110 = tag_2_1$111;
    _tmp$1184 = _cursor$97;
    if (_tmp$1184 < _end$96) {
      int32_t _p$818 = _cursor$97;
      int32_t next_char$138 = _data$94[_p$818];
      int32_t _tmp$1185 = _cursor$97;
      _cursor$97 = _tmp$1185 + 1;
      if (next_char$138 < 55296) {
        if (next_char$138 < 58) {
          if (next_char$138 < 48) {
            goto $join$136;
          } else {
            dispatch_19$134 = 4;
            continue;
          }
        } else if (next_char$138 > 58) {
          goto $join$136;
        } else {
          dispatch_19$134 = 1;
          continue;
        }
      } else if (next_char$138 > 56318) {
        if (next_char$138 < 57344) {
          dispatch_19$134 = 8;
          continue;
        } else if (next_char$138 > 65535) {
          goto $join$113;
        } else {
          goto $join$136;
        }
      } else {
        dispatch_19$134 = 7;
        continue;
      }
      $join$136:;
      dispatch_19$134 = 0;
      continue;
    } else {
      goto $join$113;
    }
    break;
  }
  $join$113:;
  switch (accept_state$98) {
    case 0: {
      void* _try_err$116;
      struct $StringView start_line$114;
      int32_t _tmp$1181 = match_tag_saver_1$101;
      int32_t _tmp$1180 = _tmp$1181 + 1;
      int64_t _tmp$1177 = (int64_t)_tmp$1180;
      int32_t _tmp$1179 = match_tag_saver_2$102;
      int64_t _tmp$1178 = (int64_t)_tmp$1179;
      struct moonbit_result_0 _tmp$2259;
      void* _try_err$119;
      struct $StringView start_column$117;
      int32_t _tmp$1174;
      int32_t _tmp$1173;
      int64_t _tmp$1170;
      int32_t _tmp$1172;
      int64_t _tmp$1171;
      struct moonbit_result_0 _tmp$2261;
      void* _try_err$122;
      struct $StringView pkg$120;
      int32_t _tmp$1167;
      int64_t _tmp$1164;
      int32_t _tmp$1166;
      int64_t _tmp$1165;
      struct moonbit_result_0 _tmp$2263;
      void* _try_err$125;
      struct $StringView filename$123;
      int32_t _tmp$1161;
      int32_t _tmp$1160;
      int64_t _tmp$1157;
      int32_t _tmp$1159;
      int64_t _tmp$1158;
      struct moonbit_result_0 _tmp$2265;
      void* _try_err$128;
      struct $StringView end_line$126;
      int32_t _tmp$1154;
      int32_t _tmp$1153;
      int64_t _tmp$1150;
      int32_t _tmp$1152;
      int64_t _tmp$1151;
      struct moonbit_result_0 _tmp$2267;
      void* _try_err$131;
      struct $StringView end_column$129;
      int32_t _tmp$1147;
      int32_t _tmp$1146;
      int64_t _tmp$1143;
      int32_t _tmp$1145;
      int64_t _tmp$1144;
      struct moonbit_result_0 _tmp$2269;
      struct $$moonbitlang$core$builtin$SourceLocRepr* _block$2270;
      moonbit_incref(_data$94);
      _tmp$2259 = $String$$sub(_data$94, _tmp$1177, _tmp$1178);
      if (_tmp$2259.tag) {
        struct $StringView const _ok$1182 = _tmp$2259.data.ok;
        start_line$114 = _ok$1182;
      } else {
        void* const _err$1183 = _tmp$2259.data.err;
        _try_err$116 = _err$1183;
        goto $join$115;
      }
      goto $joinlet$2258;
      $join$115:;
      moonbit_decref(_try_err$116);
      moonbit_panic();
      $joinlet$2258:;
      _tmp$1174 = match_tag_saver_2$102;
      _tmp$1173 = _tmp$1174 + 1;
      _tmp$1170 = (int64_t)_tmp$1173;
      _tmp$1172 = match_tag_saver_3$103;
      _tmp$1171 = (int64_t)_tmp$1172;
      moonbit_incref(_data$94);
      _tmp$2261 = $String$$sub(_data$94, _tmp$1170, _tmp$1171);
      if (_tmp$2261.tag) {
        struct $StringView const _ok$1175 = _tmp$2261.data.ok;
        start_column$117 = _ok$1175;
      } else {
        void* const _err$1176 = _tmp$2261.data.err;
        _try_err$119 = _err$1176;
        goto $join$118;
      }
      goto $joinlet$2260;
      $join$118:;
      moonbit_decref(_try_err$119);
      moonbit_panic();
      $joinlet$2260:;
      _tmp$1167 = _start$95 + 1;
      _tmp$1164 = (int64_t)_tmp$1167;
      _tmp$1166 = match_tag_saver_0$100;
      _tmp$1165 = (int64_t)_tmp$1166;
      moonbit_incref(_data$94);
      _tmp$2263 = $String$$sub(_data$94, _tmp$1164, _tmp$1165);
      if (_tmp$2263.tag) {
        struct $StringView const _ok$1168 = _tmp$2263.data.ok;
        pkg$120 = _ok$1168;
      } else {
        void* const _err$1169 = _tmp$2263.data.err;
        _try_err$122 = _err$1169;
        goto $join$121;
      }
      goto $joinlet$2262;
      $join$121:;
      moonbit_decref(_try_err$122);
      moonbit_panic();
      $joinlet$2262:;
      _tmp$1161 = match_tag_saver_0$100;
      _tmp$1160 = _tmp$1161 + 1;
      _tmp$1157 = (int64_t)_tmp$1160;
      _tmp$1159 = match_tag_saver_1$101;
      _tmp$1158 = (int64_t)_tmp$1159;
      moonbit_incref(_data$94);
      _tmp$2265 = $String$$sub(_data$94, _tmp$1157, _tmp$1158);
      if (_tmp$2265.tag) {
        struct $StringView const _ok$1162 = _tmp$2265.data.ok;
        filename$123 = _ok$1162;
      } else {
        void* const _err$1163 = _tmp$2265.data.err;
        _try_err$125 = _err$1163;
        goto $join$124;
      }
      goto $joinlet$2264;
      $join$124:;
      moonbit_decref(_try_err$125);
      moonbit_panic();
      $joinlet$2264:;
      _tmp$1154 = match_tag_saver_3$103;
      _tmp$1153 = _tmp$1154 + 1;
      _tmp$1150 = (int64_t)_tmp$1153;
      _tmp$1152 = match_tag_saver_4$104;
      _tmp$1151 = (int64_t)_tmp$1152;
      moonbit_incref(_data$94);
      _tmp$2267 = $String$$sub(_data$94, _tmp$1150, _tmp$1151);
      if (_tmp$2267.tag) {
        struct $StringView const _ok$1155 = _tmp$2267.data.ok;
        end_line$126 = _ok$1155;
      } else {
        void* const _err$1156 = _tmp$2267.data.err;
        _try_err$128 = _err$1156;
        goto $join$127;
      }
      goto $joinlet$2266;
      $join$127:;
      moonbit_decref(_try_err$128);
      moonbit_panic();
      $joinlet$2266:;
      _tmp$1147 = match_tag_saver_4$104;
      _tmp$1146 = _tmp$1147 + 1;
      _tmp$1143 = (int64_t)_tmp$1146;
      _tmp$1145 = match_end$99;
      _tmp$1144 = (int64_t)_tmp$1145;
      _tmp$2269 = $String$$sub(_data$94, _tmp$1143, _tmp$1144);
      if (_tmp$2269.tag) {
        struct $StringView const _ok$1148 = _tmp$2269.data.ok;
        end_column$129 = _ok$1148;
      } else {
        void* const _err$1149 = _tmp$2269.data.err;
        _try_err$131 = _err$1149;
        goto $join$130;
      }
      goto $joinlet$2268;
      $join$130:;
      moonbit_decref(_try_err$131);
      moonbit_panic();
      $joinlet$2268:;
      _block$2270
      = (struct $$moonbitlang$core$builtin$SourceLocRepr*)moonbit_malloc(
          sizeof(struct $$moonbitlang$core$builtin$SourceLocRepr)
        );
      Moonbit_object_header(_block$2270)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $$moonbitlang$core$builtin$SourceLocRepr, $0_0) >> 2,
          6,
          0
      );
      _block$2270->$0_0 = pkg$120.$0;
      _block$2270->$0_1 = pkg$120.$1;
      _block$2270->$0_2 = pkg$120.$2;
      _block$2270->$1_0 = filename$123.$0;
      _block$2270->$1_1 = filename$123.$1;
      _block$2270->$1_2 = filename$123.$2;
      _block$2270->$2_0 = start_line$114.$0;
      _block$2270->$2_1 = start_line$114.$1;
      _block$2270->$2_2 = start_line$114.$2;
      _block$2270->$3_0 = start_column$117.$0;
      _block$2270->$3_1 = start_column$117.$1;
      _block$2270->$3_2 = start_column$117.$2;
      _block$2270->$4_0 = end_line$126.$0;
      _block$2270->$4_1 = end_line$126.$1;
      _block$2270->$4_2 = end_line$126.$2;
      _block$2270->$5_0 = end_column$129.$0;
      _block$2270->$5_1 = end_column$129.$1;
      _block$2270->$5_2 = end_column$129.$2;
      return _block$2270;
      break;
    }
    default: {
      moonbit_decref(_data$94);
      moonbit_panic();
      break;
    }
  }
}

int32_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$91
) {
  int32_t* _field$2105 = self$91->$0;
  int32_t _cnt$2164 = Moonbit_object_header(self$91)->rc;
  if (_cnt$2164 > 1) {
    int32_t _new_cnt$2165 = _cnt$2164 - 1;
    Moonbit_object_header(self$91)->rc = _new_cnt$2165;
    moonbit_incref(_field$2105);
  } else if (_cnt$2164 == 1) {
    moonbit_free(self$91);
  }
  return _field$2105;
}

struct $$example$gen$ListUsersRow** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$example$gen$ListUsersRow$3e$* self$90
) {
  struct $$example$gen$ListUsersRow** _field$2106 = self$90->$0;
  int32_t _cnt$2166 = Moonbit_object_header(self$90)->rc;
  if (_cnt$2166 > 1) {
    int32_t _new_cnt$2167 = _cnt$2166 - 1;
    Moonbit_object_header(self$90)->rc = _new_cnt$2167;
    moonbit_incref(_field$2106);
  } else if (_cnt$2166 == 1) {
    moonbit_free(self$90);
  }
  return _field$2106;
}

int32_t $moonbitlang$core$builtin$code_point_of_surrogate_pair(
  int32_t leading$88,
  int32_t trailing$89
) {
  int32_t _tmp$1142 = leading$88 - 55296;
  int32_t _tmp$1141 = _tmp$1142 * 1024;
  int32_t _tmp$1140 = _tmp$1141 + trailing$89;
  int32_t _tmp$1139 = _tmp$1140 - 56320;
  int32_t _tmp$1138 = _tmp$1139 + 65536;
  return _tmp$1138;
}

int32_t $String$$unsafe_charcode_at(moonbit_string_t self$86, int32_t idx$87) {
  int32_t _tmp$2107 = self$86[idx$87];
  moonbit_decref(self$86);
  return _tmp$2107;
}

int32_t $Int$$is_trailing_surrogate(int32_t self$85) {
  return 56320 <= self$85 && self$85 <= 57343;
}

int32_t $Int$$is_leading_surrogate(int32_t self$84) {
  return 55296 <= self$84 && self$84 <= 56319;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
  struct $$moonbitlang$core$builtin$StringBuilder* self$81,
  int32_t ch$83
) {
  int32_t len$1133 = self$81->$1;
  int32_t _tmp$1132 = len$1133 + 4;
  moonbit_bytes_t _field$2108;
  moonbit_bytes_t data$1136;
  int32_t len$1137;
  int32_t inc$82;
  int32_t len$1135;
  int32_t _tmp$1134;
  moonbit_incref(self$81);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$81, _tmp$1132
  );
  _field$2108 = self$81->$0;
  data$1136 = _field$2108;
  len$1137 = self$81->$1;
  moonbit_incref(data$1136);
  inc$82 = $FixedArray$$set_utf16le_char(data$1136, len$1137, ch$83);
  len$1135 = self$81->$1;
  _tmp$1134 = len$1135 + inc$82;
  self$81->$1 = _tmp$1134;
  moonbit_decref(self$81);
  return 0;
}

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$76,
  int32_t required$77
) {
  moonbit_bytes_t _field$2112 = self$76->$0;
  moonbit_bytes_t data$1131 = _field$2112;
  int32_t _tmp$2111 = Moonbit_array_length(data$1131);
  int32_t current_len$75 = _tmp$2111;
  int32_t enough_space$78;
  int32_t _tmp$1130;
  moonbit_bytes_t new_data$80;
  moonbit_bytes_t _field$2110;
  moonbit_bytes_t data$1128;
  int32_t len$1129;
  moonbit_bytes_t _old$2109;
  if (required$77 <= current_len$75) {
    moonbit_decref(self$76);
    return 0;
  }
  enough_space$78 = current_len$75;
  while (1) {
    int32_t _tmp$1126 = enough_space$78;
    if (_tmp$1126 < required$77) {
      int32_t _tmp$1127 = enough_space$78;
      enough_space$78 = _tmp$1127 * 2;
      continue;
    }
    break;
  }
  _tmp$1130 = enough_space$78;
  new_data$80 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1130, 0);
  _field$2110 = self$76->$0;
  data$1128 = _field$2110;
  len$1129 = self$76->$1;
  moonbit_incref(data$1128);
  moonbit_incref(new_data$80);
  $FixedArray$$unsafe_blit$0(new_data$80, 0, data$1128, 0, len$1129);
  _old$2109 = self$76->$0;
  moonbit_decref(_old$2109);
  self$76->$0 = new_data$80;
  moonbit_decref(self$76);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Default$$Byte$$default() {
  return 0;
}

int32_t $FixedArray$$set_utf16le_char(
  moonbit_bytes_t self$70,
  int32_t offset$71,
  int32_t value$69
) {
  int32_t _tmp$1125 = value$69;
  uint32_t code$68 = *(uint32_t*)&_tmp$1125;
  if (code$68 < 65536u) {
    uint32_t _p$741 = code$68 & 255u;
    int32_t _tmp$1108 = *(int32_t*)&_p$741;
    int32_t _tmp$1107 = _tmp$1108 & 0xff;
    int32_t _tmp$1109;
    uint32_t _p$744;
    int32_t _tmp$1111;
    int32_t _tmp$1110;
    if (offset$71 < 0 || offset$71 >= Moonbit_array_length(self$70)) {
      moonbit_panic();
    }
    self$70[offset$71] = _tmp$1107;
    _tmp$1109 = offset$71 + 1;
    _p$744 = code$68 >> 8;
    _tmp$1111 = *(int32_t*)&_p$744;
    _tmp$1110 = _tmp$1111 & 0xff;
    if (_tmp$1109 < 0 || _tmp$1109 >= Moonbit_array_length(self$70)) {
      moonbit_panic();
    }
    self$70[_tmp$1109] = _tmp$1110;
    moonbit_decref(self$70);
    return 2;
  } else if (code$68 < 1114112u) {
    uint32_t hi$72 = code$68 - 65536u;
    uint32_t _tmp$1124 = hi$72 >> 10;
    uint32_t lo$73 = _tmp$1124 | 55296u;
    uint32_t _tmp$1123 = hi$72 & 1023u;
    uint32_t hi$74 = _tmp$1123 | 56320u;
    uint32_t _p$747 = lo$73 & 255u;
    int32_t _tmp$1113 = *(int32_t*)&_p$747;
    int32_t _tmp$1112 = _tmp$1113 & 0xff;
    int32_t _tmp$1114;
    uint32_t _p$750;
    int32_t _tmp$1116;
    int32_t _tmp$1115;
    int32_t _tmp$1117;
    uint32_t _p$753;
    int32_t _tmp$1119;
    int32_t _tmp$1118;
    int32_t _tmp$1120;
    uint32_t _p$756;
    int32_t _tmp$1122;
    int32_t _tmp$1121;
    if (offset$71 < 0 || offset$71 >= Moonbit_array_length(self$70)) {
      moonbit_panic();
    }
    self$70[offset$71] = _tmp$1112;
    _tmp$1114 = offset$71 + 1;
    _p$750 = lo$73 >> 8;
    _tmp$1116 = *(int32_t*)&_p$750;
    _tmp$1115 = _tmp$1116 & 0xff;
    if (_tmp$1114 < 0 || _tmp$1114 >= Moonbit_array_length(self$70)) {
      moonbit_panic();
    }
    self$70[_tmp$1114] = _tmp$1115;
    _tmp$1117 = offset$71 + 2;
    _p$753 = hi$74 & 255u;
    _tmp$1119 = *(int32_t*)&_p$753;
    _tmp$1118 = _tmp$1119 & 0xff;
    if (_tmp$1117 < 0 || _tmp$1117 >= Moonbit_array_length(self$70)) {
      moonbit_panic();
    }
    self$70[_tmp$1117] = _tmp$1118;
    _tmp$1120 = offset$71 + 3;
    _p$756 = hi$74 >> 8;
    _tmp$1122 = *(int32_t*)&_p$756;
    _tmp$1121 = _tmp$1122 & 0xff;
    if (_tmp$1120 < 0 || _tmp$1120 >= Moonbit_array_length(self$70)) {
      moonbit_panic();
    }
    self$70[_tmp$1120] = _tmp$1121;
    moonbit_decref(self$70);
    return 4;
  } else {
    moonbit_decref(self$70);
    return $moonbitlang$core$builtin$abort$5(
             (moonbit_string_t)moonbit_string_literal_7.data,
               (moonbit_string_t)moonbit_string_literal_28.data
           );
  }
}

int32_t $UInt$$to_byte(uint32_t self$67) {
  int32_t _tmp$1106 = *(int32_t*)&self$67;
  return _tmp$1106 & 0xff;
}

uint32_t $Char$$to_uint(int32_t self$66) {
  int32_t _tmp$1105 = self$66;
  return *(uint32_t*)&_tmp$1105;
}

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$65
) {
  moonbit_bytes_t _field$2114 = self$65->$0;
  moonbit_bytes_t data$1104 = _field$2114;
  moonbit_bytes_t _tmp$1101;
  int32_t _field$2113;
  int32_t len$1103;
  int64_t _tmp$1102;
  moonbit_incref(data$1104);
  _tmp$1101 = data$1104;
  _field$2113 = self$65->$1;
  moonbit_decref(self$65);
  len$1103 = _field$2113;
  _tmp$1102 = (int64_t)len$1103;
  return $Bytes$$to_unchecked_string$inner(_tmp$1101, 0, _tmp$1102);
}

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$60,
  int32_t offset$64,
  int64_t length$62
) {
  int32_t len$59 = Moonbit_array_length(self$60);
  int32_t length$61;
  int32_t _if_result$2272;
  if (length$62 == 4294967296ll) {
    length$61 = len$59 - offset$64;
  } else {
    int64_t _Some$63 = length$62;
    length$61 = (int32_t)_Some$63;
  }
  if (offset$64 >= 0) {
    if (length$61 >= 0) {
      int32_t _tmp$1100 = offset$64 + length$61;
      _if_result$2272 = _tmp$1100 <= len$59;
    } else {
      _if_result$2272 = 0;
    }
  } else {
    _if_result$2272 = 0;
  }
  if (_if_result$2272) {
    return $moonbitlang$core$builtin$unsafe_sub_string(
             self$60, offset$64, length$61
           );
  } else {
    moonbit_decref(self$60);
    moonbit_panic();
  }
}

struct $$moonbitlang$core$builtin$StringBuilder* $$moonbitlang$core$builtin$StringBuilder$$new$inner(
  int32_t size_hint$57
) {
  int32_t initial$56;
  moonbit_bytes_t data$58;
  struct $$moonbitlang$core$builtin$StringBuilder* _block$2273;
  if (size_hint$57 < 1) {
    initial$56 = 1;
  } else {
    initial$56 = size_hint$57;
  }
  data$58 = (moonbit_bytes_t)moonbit_make_bytes(initial$56, 0);
  _block$2273
  = (struct $$moonbitlang$core$builtin$StringBuilder*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$StringBuilder)
    );
  Moonbit_object_header(_block$2273)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$StringBuilder, $0) >> 2, 1, 0
  );
  _block$2273->$0 = data$58;
  _block$2273->$1 = 0;
  return _block$2273;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
  int32_t* dst$51,
  int32_t dst_offset$52,
  int32_t* src$53,
  int32_t src_offset$54,
  int32_t len$55
) {
  $FixedArray$$unsafe_blit$2(
    dst$51, dst_offset$52, src$53, src_offset$54, len$55
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
  struct $$example$gen$ListUsersRow** dst$46,
  int32_t dst_offset$47,
  struct $$example$gen$ListUsersRow** src$48,
  int32_t src_offset$49,
  int32_t len$50
) {
  $FixedArray$$unsafe_blit$1(
    dst$46, dst_offset$47, src$48, src_offset$49, len$50
  );
  return 0;
}

int32_t $FixedArray$$unsafe_blit$2(
  int32_t* dst$37,
  int32_t dst_offset$39,
  int32_t* src$38,
  int32_t src_offset$40,
  int32_t len$42
) {
  if (dst$37 == src$38 && dst_offset$39 < src_offset$40) {
    int32_t i$41 = 0;
    while (1) {
      if (i$41 < len$42) {
        int32_t _tmp$1091 = dst_offset$39 + i$41;
        int32_t _tmp$1093 = src_offset$40 + i$41;
        int32_t _tmp$1092;
        int32_t _tmp$1094;
        if (_tmp$1093 < 0 || _tmp$1093 >= Moonbit_array_length(src$38)) {
          moonbit_panic();
        }
        _tmp$1092 = (int32_t)src$38[_tmp$1093];
        if (_tmp$1091 < 0 || _tmp$1091 >= Moonbit_array_length(dst$37)) {
          moonbit_panic();
        }
        dst$37[_tmp$1091] = _tmp$1092;
        _tmp$1094 = i$41 + 1;
        i$41 = _tmp$1094;
        continue;
      } else {
        moonbit_decref(src$38);
        moonbit_decref(dst$37);
      }
      break;
    }
  } else {
    int32_t _tmp$1099 = len$42 - 1;
    int32_t i$44 = _tmp$1099;
    while (1) {
      if (i$44 >= 0) {
        int32_t _tmp$1095 = dst_offset$39 + i$44;
        int32_t _tmp$1097 = src_offset$40 + i$44;
        int32_t _tmp$1096;
        int32_t _tmp$1098;
        if (_tmp$1097 < 0 || _tmp$1097 >= Moonbit_array_length(src$38)) {
          moonbit_panic();
        }
        _tmp$1096 = (int32_t)src$38[_tmp$1097];
        if (_tmp$1095 < 0 || _tmp$1095 >= Moonbit_array_length(dst$37)) {
          moonbit_panic();
        }
        dst$37[_tmp$1095] = _tmp$1096;
        _tmp$1098 = i$44 - 1;
        i$44 = _tmp$1098;
        continue;
      } else {
        moonbit_decref(src$38);
        moonbit_decref(dst$37);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$1(
  struct $$example$gen$ListUsersRow** dst$28,
  int32_t dst_offset$30,
  struct $$example$gen$ListUsersRow** src$29,
  int32_t src_offset$31,
  int32_t len$33
) {
  if (dst$28 == src$29 && dst_offset$30 < src_offset$31) {
    int32_t i$32 = 0;
    while (1) {
      if (i$32 < len$33) {
        int32_t _tmp$1082 = dst_offset$30 + i$32;
        int32_t _tmp$1084 = src_offset$31 + i$32;
        struct $$example$gen$ListUsersRow* _tmp$2116;
        struct $$example$gen$ListUsersRow* _tmp$1083;
        struct $$example$gen$ListUsersRow* _old$2115;
        int32_t _tmp$1085;
        if (_tmp$1084 < 0 || _tmp$1084 >= Moonbit_array_length(src$29)) {
          moonbit_panic();
        }
        _tmp$2116 = (struct $$example$gen$ListUsersRow*)src$29[_tmp$1084];
        _tmp$1083 = _tmp$2116;
        if (_tmp$1082 < 0 || _tmp$1082 >= Moonbit_array_length(dst$28)) {
          moonbit_panic();
        }
        _old$2115 = (struct $$example$gen$ListUsersRow*)dst$28[_tmp$1082];
        if (_tmp$1083) {
          moonbit_incref(_tmp$1083);
        }
        if (_old$2115) {
          moonbit_decref(_old$2115);
        }
        dst$28[_tmp$1082] = _tmp$1083;
        _tmp$1085 = i$32 + 1;
        i$32 = _tmp$1085;
        continue;
      } else {
        moonbit_decref(src$29);
        moonbit_decref(dst$28);
      }
      break;
    }
  } else {
    int32_t _tmp$1090 = len$33 - 1;
    int32_t i$35 = _tmp$1090;
    while (1) {
      if (i$35 >= 0) {
        int32_t _tmp$1086 = dst_offset$30 + i$35;
        int32_t _tmp$1088 = src_offset$31 + i$35;
        struct $$example$gen$ListUsersRow* _tmp$2118;
        struct $$example$gen$ListUsersRow* _tmp$1087;
        struct $$example$gen$ListUsersRow* _old$2117;
        int32_t _tmp$1089;
        if (_tmp$1088 < 0 || _tmp$1088 >= Moonbit_array_length(src$29)) {
          moonbit_panic();
        }
        _tmp$2118 = (struct $$example$gen$ListUsersRow*)src$29[_tmp$1088];
        _tmp$1087 = _tmp$2118;
        if (_tmp$1086 < 0 || _tmp$1086 >= Moonbit_array_length(dst$28)) {
          moonbit_panic();
        }
        _old$2117 = (struct $$example$gen$ListUsersRow*)dst$28[_tmp$1086];
        if (_tmp$1087) {
          moonbit_incref(_tmp$1087);
        }
        if (_old$2117) {
          moonbit_decref(_old$2117);
        }
        dst$28[_tmp$1086] = _tmp$1087;
        _tmp$1089 = i$35 - 1;
        i$35 = _tmp$1089;
        continue;
      } else {
        moonbit_decref(src$29);
        moonbit_decref(dst$28);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$0(
  moonbit_bytes_t dst$19,
  int32_t dst_offset$21,
  moonbit_bytes_t src$20,
  int32_t src_offset$22,
  int32_t len$24
) {
  if (dst$19 == src$20 && dst_offset$21 < src_offset$22) {
    int32_t i$23 = 0;
    while (1) {
      if (i$23 < len$24) {
        int32_t _tmp$1073 = dst_offset$21 + i$23;
        int32_t _tmp$1075 = src_offset$22 + i$23;
        int32_t _tmp$1074;
        int32_t _tmp$1076;
        if (_tmp$1075 < 0 || _tmp$1075 >= Moonbit_array_length(src$20)) {
          moonbit_panic();
        }
        _tmp$1074 = (int32_t)src$20[_tmp$1075];
        if (_tmp$1073 < 0 || _tmp$1073 >= Moonbit_array_length(dst$19)) {
          moonbit_panic();
        }
        dst$19[_tmp$1073] = _tmp$1074;
        _tmp$1076 = i$23 + 1;
        i$23 = _tmp$1076;
        continue;
      } else {
        moonbit_decref(src$20);
        moonbit_decref(dst$19);
      }
      break;
    }
  } else {
    int32_t _tmp$1081 = len$24 - 1;
    int32_t i$26 = _tmp$1081;
    while (1) {
      if (i$26 >= 0) {
        int32_t _tmp$1077 = dst_offset$21 + i$26;
        int32_t _tmp$1079 = src_offset$22 + i$26;
        int32_t _tmp$1078;
        int32_t _tmp$1080;
        if (_tmp$1079 < 0 || _tmp$1079 >= Moonbit_array_length(src$20)) {
          moonbit_panic();
        }
        _tmp$1078 = (int32_t)src$20[_tmp$1079];
        if (_tmp$1077 < 0 || _tmp$1077 >= Moonbit_array_length(dst$19)) {
          moonbit_panic();
        }
        dst$19[_tmp$1077] = _tmp$1078;
        _tmp$1080 = i$26 - 1;
        i$26 = _tmp$1080;
        continue;
      } else {
        moonbit_decref(src$20);
        moonbit_decref(dst$19);
      }
      break;
    }
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$abort$5(
  moonbit_string_t string$17,
  moonbit_string_t loc$18
) {
  moonbit_string_t _tmp$1071 =
    moonbit_add_string(
      string$17, (moonbit_string_t)moonbit_string_literal_29.data
    );
  moonbit_string_t _tmp$1072 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(loc$18);
  moonbit_string_t _tmp$1070 = moonbit_add_string(_tmp$1071, _tmp$1072);
  moonbit_string_t _tmp$1069 =
    moonbit_add_string(
      _tmp$1070, (moonbit_string_t)moonbit_string_literal_30.data
    );
  return $moonbitlang$core$abort$abort$5(_tmp$1069);
}

int32_t $moonbitlang$core$builtin$abort$4(
  moonbit_string_t string$15,
  moonbit_string_t loc$16
) {
  moonbit_string_t _tmp$1067 =
    moonbit_add_string(
      string$15, (moonbit_string_t)moonbit_string_literal_29.data
    );
  moonbit_string_t _tmp$1068 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(loc$16);
  moonbit_string_t _tmp$1066 = moonbit_add_string(_tmp$1067, _tmp$1068);
  moonbit_string_t _tmp$1065 =
    moonbit_add_string(
      _tmp$1066, (moonbit_string_t)moonbit_string_literal_30.data
    );
  return $moonbitlang$core$abort$abort$4(_tmp$1065);
}

struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ $moonbitlang$core$builtin$abort$3(
  moonbit_string_t string$13,
  moonbit_string_t loc$14
) {
  moonbit_string_t _tmp$1063 =
    moonbit_add_string(
      string$13, (moonbit_string_t)moonbit_string_literal_29.data
    );
  moonbit_string_t _tmp$1064 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(loc$14);
  moonbit_string_t _tmp$1062 = moonbit_add_string(_tmp$1063, _tmp$1064);
  moonbit_string_t _tmp$1061 =
    moonbit_add_string(
      _tmp$1062, (moonbit_string_t)moonbit_string_literal_30.data
    );
  return $moonbitlang$core$abort$abort$3(_tmp$1061);
}

struct $BytesView $moonbitlang$core$builtin$abort$2(
  moonbit_string_t string$11,
  moonbit_string_t loc$12
) {
  moonbit_string_t _tmp$1059 =
    moonbit_add_string(
      string$11, (moonbit_string_t)moonbit_string_literal_29.data
    );
  moonbit_string_t _tmp$1060 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(loc$12);
  moonbit_string_t _tmp$1058 = moonbit_add_string(_tmp$1059, _tmp$1060);
  moonbit_string_t _tmp$1057 =
    moonbit_add_string(
      _tmp$1058, (moonbit_string_t)moonbit_string_literal_30.data
    );
  return $moonbitlang$core$abort$abort$2(_tmp$1057);
}

struct $$3c$$moonbitlang$core$buffer$Buffer$2a$Char$3e$$3d$$3e$Unit* $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$9,
  moonbit_string_t loc$10
) {
  moonbit_string_t _tmp$1055 =
    moonbit_add_string(
      string$9, (moonbit_string_t)moonbit_string_literal_29.data
    );
  moonbit_string_t _tmp$1056 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(loc$10);
  moonbit_string_t _tmp$1054 = moonbit_add_string(_tmp$1055, _tmp$1056);
  moonbit_string_t _tmp$1053 =
    moonbit_add_string(
      _tmp$1054, (moonbit_string_t)moonbit_string_literal_30.data
    );
  return $moonbitlang$core$abort$abort$1(_tmp$1053);
}

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$7,
  moonbit_string_t loc$8
) {
  moonbit_string_t _tmp$1051 =
    moonbit_add_string(
      string$7, (moonbit_string_t)moonbit_string_literal_29.data
    );
  moonbit_string_t _tmp$1052 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(loc$8);
  moonbit_string_t _tmp$1050 = moonbit_add_string(_tmp$1051, _tmp$1052);
  moonbit_string_t _tmp$1049 =
    moonbit_add_string(
      _tmp$1050, (moonbit_string_t)moonbit_string_literal_30.data
    );
  $moonbitlang$core$abort$abort$0(_tmp$1049);
  return 0;
}

int32_t $moonbitlang$core$abort$abort$5(moonbit_string_t msg$6) {
  moonbit_println(msg$6);
  moonbit_decref(msg$6);
  moonbit_panic();
}

int32_t $moonbitlang$core$abort$abort$4(moonbit_string_t msg$5) {
  moonbit_println(msg$5);
  moonbit_decref(msg$5);
  moonbit_panic();
}

struct $$moonbitlang$core$builtin$ArrayView$3c$Byte$3e$ $moonbitlang$core$abort$abort$3(
  moonbit_string_t msg$4
) {
  moonbit_println(msg$4);
  moonbit_decref(msg$4);
  moonbit_panic();
}

struct $BytesView $moonbitlang$core$abort$abort$2(moonbit_string_t msg$3) {
  moonbit_println(msg$3);
  moonbit_decref(msg$3);
  moonbit_panic();
}

struct $$3c$$moonbitlang$core$buffer$Buffer$2a$Char$3e$$3d$$3e$Unit* $moonbitlang$core$abort$abort$1(
  moonbit_string_t msg$2
) {
  moonbit_println(msg$2);
  moonbit_decref(msg$2);
  moonbit_panic();
}

int32_t $moonbitlang$core$abort$abort$0(moonbit_string_t msg$1) {
  moonbit_println(msg$1);
  moonbit_decref(msg$1);
  moonbit_panic();
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1028,
  int32_t _param$1027
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1026 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1028;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$1026, _param$1027
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1025,
  struct $StringView _param$1024
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1023 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1025;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    _self$1023, _param$1024
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1022,
  moonbit_string_t _param$1019,
  int32_t _param$1020,
  int32_t _param$1021
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1018 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1022;
  $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
    _self$1018, _param$1019, _param$1020, _param$1021
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1017,
  moonbit_string_t _param$1016
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1015 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1017;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    _self$1015, _param$1016
  );
  return 0;
}

void moonbit_init() {
  $moonbitlang$x$encoding$t_decode_utf_16le_lo$clo
  = (struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*)&$moonbitlang$x$encoding$t_decode_utf_16le_lo$dyncall$closure.data;
  $$moonbitlang$x$encoding$Decoder$$decode_utf_16le$clo
  = (struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*)&$$moonbitlang$x$encoding$Decoder$$decode_utf_16le$dyncall$closure.data;
  $moonbitlang$x$encoding$write_utf8_char$clo
  = (struct $$3c$$moonbitlang$core$buffer$Buffer$2a$Char$3e$$3d$$3e$Unit*)&$moonbitlang$x$encoding$write_utf8_char$dyncall$closure.data;
  $$moonbitlang$x$encoding$Decoder$$t_decode_utf_16be$clo
  = (struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*)&$$moonbitlang$x$encoding$Decoder$$t_decode_utf_16be$dyncall$closure.data;
  $$moonbitlang$x$encoding$Decoder$$t_decode_utf_16le$clo
  = (struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*)&$$moonbitlang$x$encoding$Decoder$$t_decode_utf_16le$dyncall$closure.data;
  $moonbitlang$x$encoding$t_fill$clo
  = (struct $$3c$$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*)&$moonbitlang$x$encoding$t_fill$dyncall$closure.data;
  $$moonbitlang$x$encoding$Decoder$$decode_utf_16be$clo
  = (struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*)&$$moonbitlang$x$encoding$Decoder$$decode_utf_16be$dyncall$closure.data;
  $$moonbitlang$x$encoding$Decoder$$decode_utf_8$clo
  = (struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*)&$$moonbitlang$x$encoding$Decoder$$decode_utf_8$dyncall$closure.data;
  $moonbitlang$x$encoding$t_decode_utf_16be_lo$clo
  = (struct $$3c$Int$2a$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*)&$moonbitlang$x$encoding$t_decode_utf_16be_lo$dyncall$closure.data;
  $$moonbitlang$x$encoding$Decoder$$t_decode_utf_8$clo
  = (struct $$3c$$moonbitlang$x$encoding$Decoder$3e$$3d$$3e$$moonbitlang$x$encoding$Decode*)&$$moonbitlang$x$encoding$Decoder$$t_decode_utf_8$dyncall$closure.data;
  $moonbitlang$x$encoding$write_utf16be_char$clo
  = (struct $$3c$$moonbitlang$core$buffer$Buffer$2a$Char$3e$$3d$$3e$Unit*)&$moonbitlang$x$encoding$write_utf16be_char$dyncall$closure.data;
  $moonbitlang$x$encoding$malformed_pair$constr$532 = (int64_t)2;
}

int main(int argc, char** argv) {
  moonbit_bytes_t _tmp$1047;
  moonbit_bytes_t _tmp$1048;
  void* _tmp$2125;
  void* db$714;
  moonbit_bytes_t _tmp$1046;
  void* _tmp$2124;
  void* stmt$715;
  int32_t _tmp$1029;
  struct $$example$gen$CreateUserParams* params1$716;
  struct $$example$gen$CreateUserParams* params2$717;
  struct $$moonbitlang$core$builtin$Array$3c$$example$gen$ListUsersRow$3e$* users$718;
  int32_t len$1032;
  moonbit_string_t _tmp$1031;
  moonbit_string_t _tmp$1030;
  int32_t _len$719;
  int32_t _i$720;
  struct $$example$gen$GetUserParams* get_params$723;
  struct $$example$gen$GetUserRow* _bind$724;
  moonbit_runtime_init(argc, argv);
  moonbit_init();
  _tmp$1047
  = $example$src$main$cstring(
    (moonbit_string_t)moonbit_string_literal_31.data
  );
  _tmp$1048 = $Bytes$$new(0);
  _tmp$2125 = $mizchi$sqlite$sqlite_open_v2(_tmp$1047, 134, _tmp$1048);
  moonbit_decref(_tmp$1047);
  moonbit_decref(_tmp$1048);
  db$714 = _tmp$2125;
  moonbit_incref($example$src$main$_init$2a$$create_table_sql$7c$2);
  _tmp$1046
  = $example$src$main$cstring(
    $example$src$main$_init$2a$$create_table_sql$7c$2
  );
  _tmp$2124 = $mizchi$sqlite$sqlite_prepare(db$714, _tmp$1046);
  moonbit_decref(_tmp$1046);
  stmt$715 = _tmp$2124;
  _tmp$1029 = $mizchi$sqlite$sqlite_step(stmt$715);
  $mizchi$sqlite$sqlite_finalize(stmt$715);
  if (stmt$715) {
    moonbit_decref(stmt$715);
  }
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_32.data
  );
  params1$716
  = $$example$gen$CreateUserParams$$new(
    (moonbit_string_t)moonbit_string_literal_33.data,
      (moonbit_string_t)moonbit_string_literal_34.data
  );
  if (db$714) {
    moonbit_incref(db$714);
  }
  $example$gen$create_user(db$714, params1$716);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_35.data
  );
  params2$717
  = $$example$gen$CreateUserParams$$new(
    (moonbit_string_t)moonbit_string_literal_36.data,
      (moonbit_string_t)moonbit_string_literal_37.data
  );
  if (db$714) {
    moonbit_incref(db$714);
  }
  $example$gen$create_user(db$714, params2$717);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_38.data
  );
  if (db$714) {
    moonbit_incref(db$714);
  }
  users$718 = $example$gen$list_users(db$714);
  len$1032 = users$718->$1;
  _tmp$1031 = $Int$$to_string$inner(len$1032, 10);
  _tmp$1030
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_39.data, _tmp$1031
  );
  $moonbitlang$core$builtin$println$0(_tmp$1030);
  _len$719 = users$718->$1;
  _i$720 = 0;
  while (1) {
    if (_i$720 < _len$719) {
      struct $$example$gen$ListUsersRow** _field$2123 = users$718->$0;
      struct $$example$gen$ListUsersRow** buf$1042 = _field$2123;
      struct $$example$gen$ListUsersRow* _tmp$2122 =
        (struct $$example$gen$ListUsersRow*)buf$1042[_i$720];
      struct $$example$gen$ListUsersRow* user$721 = _tmp$2122;
      int64_t id$1041 = user$721->$0;
      moonbit_string_t _tmp$1040;
      moonbit_string_t _tmp$1039;
      moonbit_string_t _tmp$1037;
      moonbit_string_t _field$2121;
      moonbit_string_t name$1038;
      moonbit_string_t _tmp$1036;
      moonbit_string_t _tmp$1034;
      moonbit_string_t _field$2120;
      int32_t _cnt$2168;
      moonbit_string_t email$1035;
      moonbit_string_t _tmp$1033;
      int32_t _tmp$1043;
      moonbit_incref(user$721);
      _tmp$1040 = $Int64$$to_string$inner(id$1041, 10);
      _tmp$1039
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_40.data, _tmp$1040
      );
      _tmp$1037
      = moonbit_add_string(
        _tmp$1039, (moonbit_string_t)moonbit_string_literal_41.data
      );
      _field$2121 = user$721->$1;
      name$1038 = _field$2121;
      moonbit_incref(name$1038);
      _tmp$1036 = moonbit_add_string(_tmp$1037, name$1038);
      _tmp$1034
      = moonbit_add_string(
        _tmp$1036, (moonbit_string_t)moonbit_string_literal_42.data
      );
      _field$2120 = user$721->$2;
      _cnt$2168 = Moonbit_object_header(user$721)->rc;
      if (_cnt$2168 > 1) {
        int32_t _new_cnt$2170 = _cnt$2168 - 1;
        Moonbit_object_header(user$721)->rc = _new_cnt$2170;
        moonbit_incref(_field$2120);
      } else if (_cnt$2168 == 1) {
        moonbit_string_t _field$2169 = user$721->$1;
        moonbit_decref(_field$2169);
        moonbit_free(user$721);
      }
      email$1035 = _field$2120;
      _tmp$1033 = moonbit_add_string(_tmp$1034, email$1035);
      $moonbitlang$core$builtin$println$0(_tmp$1033);
      _tmp$1043 = _i$720 + 1;
      _i$720 = _tmp$1043;
      continue;
    } else {
      moonbit_decref(users$718);
    }
    break;
  }
  get_params$723 = $$example$gen$GetUserParams$$new(1ll);
  _bind$724 = $example$gen$get_user(db$714, get_params$723);
  if (_bind$724 == 0) {
    if (_bind$724) {
      moonbit_decref(_bind$724);
    }
    $moonbitlang$core$builtin$println$0(
      (moonbit_string_t)moonbit_string_literal_43.data
    );
  } else {
    struct $$example$gen$GetUserRow* _Some$725 = _bind$724;
    struct $$example$gen$GetUserRow* _user$726 = _Some$725;
    moonbit_string_t _field$2119 = _user$726->$1;
    int32_t _cnt$2171 = Moonbit_object_header(_user$726)->rc;
    moonbit_string_t name$1045;
    moonbit_string_t _tmp$1044;
    if (_cnt$2171 > 1) {
      int32_t _new_cnt$2173 = _cnt$2171 - 1;
      Moonbit_object_header(_user$726)->rc = _new_cnt$2173;
      moonbit_incref(_field$2119);
    } else if (_cnt$2171 == 1) {
      moonbit_string_t _field$2172 = _user$726->$2;
      moonbit_decref(_field$2172);
      moonbit_free(_user$726);
    }
    name$1045 = _field$2119;
    _tmp$1044
    = moonbit_add_string(
      (moonbit_string_t)moonbit_string_literal_44.data, name$1045
    );
    $moonbitlang$core$builtin$println$0(_tmp$1044);
  }
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_45.data
  );
  return 0;
}