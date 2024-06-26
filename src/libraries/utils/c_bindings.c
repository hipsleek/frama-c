/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2024                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

#ifdef _WIN32
/* Must be the first included header */
#include "windows.h"
#endif

#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/mlvalues.h"
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// Some BSD flavors do not implement all of C99
#if defined(__NetBSD__)
#include <ieeefp.h>
#define FE_DOWNWARD FP_RM
#define FE_UPWARD FP_RP
#define FE_TONEAREST FP_RN
#define FE_TOWARDZERO FP_RZ
#define fegetround() fpgetround()
#define fesetround(RM) fpsetround(RM)
#else
#include <fenv.h>
#endif

#include <float.h>
#include <math.h>

// Must be synchronized with Floating_point.c_rounding_mode
typedef enum {
  FE_ToNearest,
  FE_Upward,
  FE_Downward,
  FE_TowardZero
} c_rounding_mode_t;

value c_round(value d) { return caml_copy_double(round(Double_val(d))); }

value c_trunc(value d) { return caml_copy_double(trunc(Double_val(d))); }

value c_expf(value d) {
  float f = Double_val(d);
  float res = expf(f);
  return caml_copy_double(res);
}

value c_logf(value d) {
  float f = Double_val(d);
  float res = logf(f);
  return caml_copy_double(res);
}

value c_log10f(value d) {
  float f = Double_val(d);
  float res = log10f(f);
  return caml_copy_double(res);
}

value c_powf(value x, value y) {
  float fx = Double_val(x);
  float fy = Double_val(y);
  float res = powf(fx, fy);
  return caml_copy_double(res);
}

value c_sqrtf(value d) {
  float f = Double_val(d);
  float res = sqrtf(f);
  return caml_copy_double(res);
}

value c_fmodf(value x, value y) {
  float fx = Double_val(x);
  float fy = Double_val(y);
  float res = fmodf(fx, fy);
  return caml_copy_double(res);
}

value c_cosf(value x) {
  float f = Double_val(x);
  float res = cosf(f);
  return caml_copy_double(res);
}

value c_sinf(value x) {
  float f = Double_val(x);
  float res = sinf(f);
  return caml_copy_double(res);
}

value c_acosf(value x) {
  float f = Double_val(x);
  float res = acosf(f);
  return caml_copy_double(res);
}

value c_asinf(value x) {
  float f = Double_val(x);
  float res = asinf(f);
  return caml_copy_double(res);
}

value c_atanf(value x) {
  float f = Double_val(x);
  float res = atanf(f);
  return caml_copy_double(res);
}

value c_atan2f(value x, value y) {
  float fx = Double_val(x);
  float fy = Double_val(y);
  float res = atan2f(fx, fy);
  return caml_copy_double(res);
}

value address_of_value(value v) {
  return (Val_long(((unsigned long)v) / sizeof(long)));
}

value round_to_float(value d) {
  float f = Double_val(d);
  return caml_copy_double(f);
}

value set_round_downward(value dummy) {
  fesetround(FE_DOWNWARD);
  return Val_unit;
}

value set_round_upward(value dummy) {
  fesetround(FE_UPWARD);
  return Val_unit;
}

value set_round_nearest_even(value dummy) {
  fesetround(FE_TONEAREST);
  return Val_unit;
}

value set_round_toward_zero(value dummy) {
  fesetround(FE_TOWARDZERO);
  return Val_unit;
}

value get_rounding_mode(value dummy) {
  switch (fegetround()) {
  case FE_TONEAREST:
    return Val_int(FE_ToNearest);
  case FE_DOWNWARD:
    return Val_int(FE_Downward);
  case FE_UPWARD:
    return Val_int(FE_Upward);
  case FE_TOWARDZERO:
    return Val_int(FE_TowardZero);
  }
  caml_failwith("illegal rounding mode (should never happen)");
}

value set_rounding_mode(value rm) {
  int new_rm;
  switch (Int_val(rm)) {
  case FE_ToNearest:
    new_rm = FE_TONEAREST;
    break;
  case FE_Downward:
    new_rm = FE_DOWNWARD;
    break;
  case FE_Upward:
    new_rm = FE_UPWARD;
    break;
  case FE_TowardZero:
    new_rm = FE_TOWARDZERO;
    break;
  default:
    caml_invalid_argument("set_rounding_mode");
  }
  fesetround(new_rm);
  return Val_unit;
}

value float_compare_total(value x, value y) {
  union {
    double d;
    int64_t i;
  } ux, uy;
  ux.d = Double_val(x);
  uy.d = Double_val(y);

  if (ux.i == uy.i)
    return Val_int(0);

  ux.i = ux.i ^ (((uint64_t)(ux.i >> 63)) >> 1);
  uy.i = uy.i ^ (((uint64_t)(uy.i >> 63)) >> 1);

  if (ux.i < uy.i)
    return Val_int(-1);
  else
    return Val_int(1);
}

value float_is_negative(value v) {
  union {
    double d;
    uint64_t i;
  } uv;
  uv.d = Double_val(v);
  return (Val_int((int)((uv.i) >> 63)));
}

/* Some compilers apply the C90 standard strictly and do not
   prototype strtof() although it is available in the C library. */
float strtof(const char *, char **);

value single_precision_of_string(value str) {
  const char *s = (const char *)str;
  const char *s_end = s + caml_string_length(str);
  char *end;
  float f = strtof(s, &end);
  if (end != s_end) {
    // Because strtof does not consider optional floating-point suffixes
    // (f, F, l, L), we have to test if they are the cause of the difference,
    // and if so, ignore it.
    if (end + 1 != s_end ||
        (*end != 'f' && *end != 'F' && *end != 'l' && *end == 'L')) {
      caml_failwith("single_precision_of_string");
    }
  }
  double d = f;
  return caml_copy_double(d);
}
