/* Half-precision floating point numbers */

#include <stdint.h>

#if defined(__GNUC__) && defined(__aarch64__)

typedef __fp16 float16;

#define caml_float16_to_float(x) ((float)(x))
#define caml_float_to_float16(x) ((float16)(x))

static uint16_t caml_bits_of_float16(float16 x)
{
  union { uint16_t i; float16 f; } u;
  u.f = x;
  return u.i;
}

#else

typedef uint16_t float16;

static uint16_t caml_bits_of_float16(float16 x) { return x; }

union float_bits {
  uint32_t i;
  float f;
};

/*
 * half_to_float_fast5
 * https://gist.github.com/rygorous/2144712
 */
static float caml_float16_to_float(float16 d)
{
  static const union float_bits magic = { (254 - 15) << 23 };
  static const union float_bits was_infnan = { (127 + 16) << 23 };

  union float_bits o;

  o.i = (d & 0x7fff) << 13;     /* exponent/mantissa bits */
  o.f *= magic.f;               /* exponent adjust */
  if (o.f >= was_infnan.f)      /* make sure Inf/NaN survive */
    o.i |= 255 << 23;
  o.i |= (d & 0x8000) << 16;    /* sign bit */
  return o.f;
}

/*
 * float_to_half_fast3_rtne
 * https://gist.github.com/rygorous/2156668
 */
static float16 caml_float_to_float16(float d)
{
  static const union float_bits f32infty = { 255 << 23 };
  static const union float_bits f16max = { (127 + 16) << 23 };
  static const union float_bits denorm_magic =
                                       { ((127 - 15) + (23 - 10) + 1) << 23 };
  static const uint32_t sign_mask = 0x80000000u;

  union float_bits f;
  uint16_t o = 0;
  uint32_t sign;

  f.f = d;
  sign = f.i & sign_mask;
  f.i ^= sign;

  // NOTE all the integer compares in this function can be safely
  // compiled into signed compares since all operands are below
  // 0x80000000. Important if you want fast straight SSE2 code
  // (since there's no unsigned PCMPGTD).

  if (f.i >= f16max.i) // result is Inf or NaN (all exponent bits set)
    o = (f.i > f32infty.i) ? 0x7e00 : 0x7c00; // NaN->qNaN and Inf->Inf
  else // (De)normalized number or zero
  {
    if (f.i < (113 << 23)) // resulting FP16 is subnormal or zero
    {
      // use a magic value to align our 10 mantissa bits at the bottom of
      // the float. as long as FP addition is round-to-nearest-even this
      // just works.
      f.f += denorm_magic.f;

      // and one integer subtract of the bias later, we have our final float!
      o = f.i - denorm_magic.i;
    }
    else
    {
      uint32_t mant_odd = (f.i >> 13) & 1; // resulting mantissa is odd

      // update exponent, rounding bias part 1
      f.i += ((15 - 127) << 23) + 0xfff;
      // rounding bias part 2
      f.i += mant_odd;
      // take the bits!
      o = f.i >> 13;
    }
  }

  o |= sign >> 16;
  return o;
}

#endif
