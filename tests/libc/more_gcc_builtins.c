/* run.config
   STDOPT: +"-machdep gcc_x86_32"
 */

volatile int v;
#include <limits.h>

int main() {
  int res;
  _Bool r = __builtin_sadd_overflow(42, 43, &res);
  //@ assert res == 42 + 43;
  //@ assert r == 0;
  r = __builtin_sadd_overflow(42, INT_MAX, &res);
  //@ assert res == (int)(42 + INT_MAX);
  //@ assert r == 1;
  long lres;
  r = __builtin_saddl_overflow(42, LONG_MAX, &lres);
  //@ assert lres == (long)(42 + LONG_MAX);
  //@ assert r == 1;
  r = __builtin_saddl_overflow(-2, -LONG_MAX, &lres);
  //@ assert lres == (long)(-2 - LONG_MAX);
  //@ assert r == 1;
  long long llres;
  r = __builtin_saddll_overflow(-5, -LLONG_MAX, &llres);
  //@ assert llres == (long long)(-5 - LLONG_MAX);
  //@ assert r == 1;
  unsigned ures;
  r = __builtin_uadd_overflow(9, UINT_MAX, &ures);
  //@ assert ures == (unsigned)(9 + UINT_MAX);
  //@ assert r == 1;
  unsigned long ulres;
  r = __builtin_uaddl_overflow(9, ULONG_MAX, &ulres);
  //@ assert ulres == (unsigned long)(9 + ULONG_MAX);
  //@ assert r == 1;
  unsigned long long ullres;
  r = __builtin_uaddll_overflow(9, ULLONG_MAX, &ullres);
  //@ assert ullres == (unsigned long long)(9 + ULLONG_MAX);
  //@ assert r == 1;
  r = __builtin_usubll_overflow(-5, ULLONG_MAX, &ullres);
  //@ assert ullres == (unsigned long long)(-5 - ULLONG_MAX);
  //@ assert r == 1;
  r = __builtin_smulll_overflow(-1, LLONG_MIN, &llres);
  //@ assert llres == (long long)(-1 * LLONG_MIN);
  //@ assert r == 1;
  if (v) {
    __builtin_clz(0);
    //@ assert unreachable:\false;
  }
  res = __builtin_clz(1);
  //@ assert 0 <= res < CHAR_BIT * sizeof(int);
  res = __builtin_clzl(ULONG_MAX);
  //@ assert 0 <= res < CHAR_BIT * sizeof(long);
  res = __builtin_clzll(ULLONG_MAX);
  //@ assert 0 <= res < CHAR_BIT * sizeof(long long);
  if (v) {
    __builtin_ctz(0);
    //@ assert unreachable:\false;
  }
  res = __builtin_ctz(42);
  //@ assert 0 <= res < CHAR_BIT * sizeof(int);
  res = __builtin_ctzl(1234567);
  //@ assert 0 <= res < CHAR_BIT * sizeof(long);
  res = __builtin_ctzll(1);
  //@ assert 0 <= res < CHAR_BIT * sizeof(long long);

  res = __builtin_popcount(0);
  //@ assert 0 <= res <= CHAR_BIT * sizeof(int);
  res = __builtin_popcountl(ULONG_MAX);
  //@ assert 0 <= res <= CHAR_BIT * sizeof(long);
  res = __builtin_popcountll(ULLONG_MAX);
  //@ assert 0 <= res <= CHAR_BIT * sizeof(long long);

  // Atomic builtins
  __UINT8_T u8_1, u8_2 = 42;
  u8_1 = __atomic_load_1(&u8_2, __ATOMIC_RELAXED);
  __UINT16_T u16_1, u16_2 = 4200;
  __atomic_store(&u16_1, u16_2, __ATOMIC_ACQ_REL);
  __UINT32_T u32_1, u32_2 = 1234567, u32_3 = 420000;
  u32_1 = __atomic_exchange_4(&u32_2, u32_3, __ATOMIC_RELEASE);
  __UINT64_T u64_1 = 12345678901UL, u64_2 = 42000000000UL, u64_3 = 42000000001UL;
  r = __atomic_compare_exchange(&u64_1, &u64_2, u64_3, 1, __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE);
  u8_2 = __atomic_add_fetch(&u8_1, u8_2, __ATOMIC_CONSUME);
  u16_2 = __atomic_fetch_sub(&u16_1, u16_2, __ATOMIC_RELAXED);
  u32_2 = __atomic_fetch_xor(&u32_1, u32_2, __ATOMIC_RELAXED);
  u64_2 = __atomic_fetch_nand(&u64_1, u64_2, __ATOMIC_RELAXED);
  r = __atomic_test_and_set(&u32_3, __ATOMIC_ACQUIRE);
  __atomic_clear(&r, __ATOMIC_ACQ_REL);
  r = __atomic_always_lock_free(sizeof(long long), 0);
  r = __atomic_is_lock_free(sizeof(long), &u32_1);
  return 0;
}
