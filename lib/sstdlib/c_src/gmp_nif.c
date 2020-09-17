#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdbool.h>
#include <time.h>
#include <gmp.h>
#include <erl_nif.h>
#include "dloglib.h"

#ifdef DEBUG
FILE *_log_file;
char _temp_str[4096];
#define GET_STR(_value) mpz_get_str(_temp_str, 10, _value)
#define LOG(...) \
do { \
  fprintf(_log_file, ##__VA_ARGS__); \
  fflush(_log_file); \
} while (0)
#define OPEN_LOG \
do { \
  if ((_log_file = fopen("/tmp/gmp.log", "a")) == NULL) { \
    return enif_make_tuple2(env, \
                            enif_make_atom(env, "error"), \
                            enif_make_string(env, strerror(errno), \
                                             ERL_NIF_LATIN1)); \
  } \
} while (0)
#define CLOSE_LOG fclose(_log_file)
#else
#define GET_STR(value) (void)0
#define LOG(...) (void)0
#define OPEN_LOG (void)0
#define CLOSE_LOG (void)0
#endif

bool export_binary(ErlNifBinary *bin, mpz_t value);
void import_binary(mpz_t value, ErlNifBinary *bin);

gmp_randstate_t rand_state;

/*
 * dlog
 */

static ERL_NIF_TERM _dlog(ErlNifEnv* env, int argc,
                          const ERL_NIF_TERM argv[]) {
  OPEN_LOG;
  LOG("**** dlog\n");

  ErlNifBinary h_bin;
  if (!enif_inspect_binary(env, argv[0], &h_bin)) {
    CLOSE_LOG;
    return enif_make_badarg(env);
  }

  ErlNifBinary g_bin;
  if (!enif_inspect_binary(env, argv[1], &g_bin)) {
    CLOSE_LOG;
    return enif_make_badarg(env);
  }

  ErlNifBinary p_bin;
  if (!enif_inspect_binary(env, argv[2], &p_bin)) {
    CLOSE_LOG;
    return enif_make_badarg(env);
  }

  mpz_t h;
  mpz_init(h);
  import_binary(h, &h_bin);
  LOG("h = %s\n", GET_STR(h));

  mpz_t g;
  mpz_init(g);
  import_binary(g, &g_bin);
  LOG("g = %s\n", GET_STR(g));

  mpz_t p;
  mpz_init(p);
  import_binary(p, &p_bin);
  LOG("p = %s\n", GET_STR(p));

  mpz_t rop;
  mpz_init(rop);
  dlog(rop, h, g, p);
  LOG("rop = %s\n", GET_STR(rop));

  ErlNifBinary rop_bin;
  if (!export_binary(&rop_bin, rop)) {
    enif_release_binary(&h_bin);
    enif_release_binary(&g_bin);
    enif_release_binary(&p_bin);
    mpz_clear(rop);
    CLOSE_LOG;
    return enif_make_badarg(env);
  } else {
    enif_release_binary(&h_bin);
    enif_release_binary(&g_bin);
    enif_release_binary(&p_bin);
    mpz_clear(rop);
    CLOSE_LOG;
    return enif_make_binary(env, &rop_bin);
  }
}

/*
 * generate_safe_prime
 */

// http://cs.uccs.edu/~gsc/pub/master/cmccullo/src/sources/Common/Common.cpp

static ERL_NIF_TERM _generate_safe_prime(ErlNifEnv* env, int argc,
                                         const ERL_NIF_TERM argv[]) {

  OPEN_LOG;
  LOG("**** generate_safe_prime\n");

  long len;
  if (!enif_get_long(env, argv[0], &len)) {
    CLOSE_LOG;
    return enif_make_badarg(env);
  }

  LOG("len = %ld\n", len);

  mpz_t p;
  mpz_init(p);
  mpz_t q;
  mpz_init(q);
  mpz_t size; // 2 ^ len
  mpz_init(size);
  mpz_set_ui(size, 1 << len);
  mpz_t rop;
  mpz_init(rop);

  do {
    LOG(".");
    mpz_urandomb(p, rand_state, len);
    LOG("RANDOM: %s\n", GET_STR(p));
    mpz_add(p, p, size);
    LOG("RANDOM1: %s\n", GET_STR(p));
    mpz_nextprime(q, p);
    LOG("RANDOM2: %s\n", GET_STR(p));
    mpz_mul_ui(p, q, 2);
    LOG("RANDOM3: %s\n", GET_STR(p));
    mpz_add_ui(p, p, 1);

    if (mpz_probab_prime_p(p, 10) > 0) {
       LOG("rop = %s is prime\n", GET_STR(rop));
       mpz_set(rop, p);
       break;
    } else {
      LOG("rop = %s is not prime\n", GET_STR(rop));
    }
    mpz_sub_ui(p, q, 1);
    mpz_tdiv_q_ui(p, p, 2);
    if (mpz_probab_prime_p(p, 10) > 0) {
       LOG("rop = %s is prime\n", GET_STR(rop));
       mpz_set(rop, q);
       break;
    } else {
      LOG("rop = %s is not prime\n", GET_STR(rop));
    }
   } while (true);

  LOG("rop = %s\n", GET_STR(rop));

  ErlNifBinary rop_bin;
  if (!export_binary(&rop_bin, rop)) {
    mpz_clear(p);
    mpz_clear(q);
    mpz_clear(size);
    mpz_clear(rop);
    CLOSE_LOG;
    return enif_make_badarg(env);
  } else {
    mpz_clear(p);
    mpz_clear(q);
    mpz_clear(size);
    mpz_clear(rop);
    CLOSE_LOG;
    return enif_make_binary(env, &rop_bin);
  }
}

/*
 * mpz_gcd
 */

static ERL_NIF_TERM _mpz_gcd(ErlNifEnv* env, int argc,
                             const ERL_NIF_TERM argv[]) {
  OPEN_LOG;
  LOG("**** mpz_gcd\n");

  ErlNifBinary op1_bin;
  if (!enif_inspect_binary(env, argv[0], &op1_bin)) {
    CLOSE_LOG;
    return enif_make_badarg(env);
  }

  ErlNifBinary op2_bin;
  if (!enif_inspect_binary(env, argv[1], &op2_bin)) {
    CLOSE_LOG;
    return enif_make_badarg(env);
  }

  mpz_t op1;
  mpz_init(op1);
  import_binary(op1, &op1_bin);
  LOG("op1 = %s\n", GET_STR(op1));

  mpz_t op2;
  mpz_init(op2);
  import_binary(op2, &op2_bin);
  LOG("op2 = %s\n", GET_STR(op2));

  mpz_t rop;
  mpz_init(rop);
  mpz_gcd(rop, op1, op2);
  LOG("rop = %s\n", GET_STR(rop));

  ErlNifBinary rop_bin;
  if (!export_binary(&rop_bin, rop)) {
    enif_release_binary(&op1_bin);
    enif_release_binary(&op2_bin);
    mpz_clear(rop);
    CLOSE_LOG;
    return enif_make_badarg(env);
  } else {
    enif_release_binary(&op1_bin);
    enif_release_binary(&op2_bin);
    mpz_clear(rop);
    CLOSE_LOG;
    return enif_make_binary(env, &rop_bin);
  }
}

/*
 * mpz_invert
 */

static ERL_NIF_TERM _mpz_invert(ErlNifEnv* env, int argc,
                                const ERL_NIF_TERM argv[]) {
  OPEN_LOG;
  LOG("**** mpz_invert\n");

  ErlNifBinary op1_bin;
  if (!enif_inspect_binary(env, argv[0], &op1_bin)) {
    CLOSE_LOG;
    return enif_make_badarg(env);
  }

  ErlNifBinary op2_bin;
  if (!enif_inspect_binary(env, argv[1], &op2_bin)) {
    CLOSE_LOG;
    return enif_make_badarg(env);
  }

  mpz_t op1;
  mpz_init(op1);
  import_binary(op1, &op1_bin);
  LOG("op1 = %s\n", GET_STR(op1));

  mpz_t op2;
  mpz_init(op2);
  import_binary(op2, &op2_bin);
  LOG("op2 = %s\n", GET_STR(op2));

  mpz_t rop;
  mpz_init(rop);
  mpz_invert(rop, op1, op2);
  LOG("rop = %s\n", GET_STR(rop));

  ErlNifBinary rop_bin;
  if (!export_binary(&rop_bin, rop)) {
    enif_release_binary(&op1_bin);
    enif_release_binary(&op2_bin);
    mpz_clear(rop);
    CLOSE_LOG;
    return enif_make_badarg(env);
  } else {
    enif_release_binary(&op1_bin);
    enif_release_binary(&op2_bin);
    mpz_clear(rop);
    CLOSE_LOG;
    return enif_make_binary(env, &rop_bin);
  }
}

/*
 * mpz_lcm
 */

static ERL_NIF_TERM _mpz_lcm(ErlNifEnv* env, int argc,
                             const ERL_NIF_TERM argv[]) {
  OPEN_LOG;
  LOG("**** mpz_lcm\n");

  ErlNifBinary op1_bin;
  if (!enif_inspect_binary(env, argv[0], &op1_bin)) {
    CLOSE_LOG;
    return enif_make_badarg(env);
  }

  ErlNifBinary op2_bin;
  if (!enif_inspect_binary(env, argv[1], &op2_bin)) {
    CLOSE_LOG;
    return enif_make_badarg(env);
  }

  mpz_t op1;
  mpz_init(op1);
  import_binary(op1, &op1_bin);
  LOG("op1 = %s\n", GET_STR(op1));

  mpz_t op2;
  mpz_init(op2);
  import_binary(op2, &op2_bin);
  LOG("op2 = %s\n", GET_STR(op2));

  mpz_t rop;
  mpz_init(rop);
  mpz_lcm(rop, op1, op2);
  LOG("rop = %s\n", GET_STR(rop));

  ErlNifBinary rop_bin;
  if (!export_binary(&rop_bin, rop)) {
    enif_release_binary(&op1_bin);
    enif_release_binary(&op2_bin);
    mpz_clear(rop);
    CLOSE_LOG;
    return enif_make_badarg(env);
  } else {
    enif_release_binary(&op1_bin);
    enif_release_binary(&op2_bin);
    mpz_clear(rop);
    CLOSE_LOG;
    return enif_make_binary(env, &rop_bin);
  }
}

/*
 * mpz_powm
 */

static ERL_NIF_TERM _mpz_powm(ErlNifEnv* env, int argc,
                              const ERL_NIF_TERM argv[]) {
  OPEN_LOG;
  LOG("**** mpz_powm\n");

  ErlNifBinary base_bin;
  if (!enif_inspect_binary(env, argv[0], &base_bin)) {
    CLOSE_LOG;
    return enif_make_badarg(env);
  }

  ErlNifBinary exp_bin;
  if (!enif_inspect_binary(env, argv[1], &exp_bin)) {
    CLOSE_LOG;
    return enif_make_badarg(env);
  }

  ErlNifBinary mod_bin;
  if (!enif_inspect_binary(env, argv[2], &mod_bin)) {
    CLOSE_LOG;
    return enif_make_badarg(env);
  }

  mpz_t base;
  mpz_init(base);
  import_binary(base, &base_bin);
  LOG("base = %s\n", GET_STR(base));

  mpz_t exp;
  mpz_init(exp);
  import_binary(exp, &exp_bin);
  LOG("exp = %s\n", GET_STR(exp));

  mpz_t mod;
  mpz_init(mod);
  import_binary(mod, &mod_bin);
  LOG("mod = %s\n", GET_STR(mod));

  mpz_t rop;
  mpz_init(rop);
  mpz_powm(rop, base, exp, mod);
  LOG("rop = %s\n", GET_STR(rop));

  ErlNifBinary rop_bin;
  if (!export_binary(&rop_bin, rop)) {
    enif_release_binary(&base_bin);
    enif_release_binary(&exp_bin);
    enif_release_binary(&mod_bin);
    mpz_clear(rop);
    CLOSE_LOG;
    return enif_make_badarg(env);
  } else {
    enif_release_binary(&base_bin);
    enif_release_binary(&exp_bin);
    enif_release_binary(&mod_bin);
    mpz_clear(rop);
    CLOSE_LOG;
    return enif_make_binary(env, &rop_bin);
  }
}

/*
 * mpz_pow_ui
 */

static ERL_NIF_TERM _mpz_pow_ui(ErlNifEnv* env, int argc,
                                const ERL_NIF_TERM argv[]) {
  OPEN_LOG;
  LOG("**** mpz_probab_prime_p\n");

  ErlNifBinary base_bin;
  if (!enif_inspect_binary(env, argv[0], &base_bin)) {
    CLOSE_LOG;
    return enif_make_badarg(env);
  }

  mpz_t base;
  mpz_init(base);
  import_binary(base, &base_bin);
  LOG("base = %s\n", GET_STR(base));

  long exp;
  if (!enif_get_long(env, argv[1], &exp)) {
    CLOSE_LOG;
    return enif_make_badarg(env);
  }
  LOG("exp = %ld\n", exp);

  mpz_t rop;
  mpz_init(rop);
  mpz_pow_ui(rop, base, exp);
  LOG("rop = %s\n", GET_STR(rop));

  ErlNifBinary rop_bin;
  if (!export_binary(&rop_bin, rop)) {
    enif_release_binary(&base_bin);
    mpz_clear(rop);
    CLOSE_LOG;
    return enif_make_badarg(env);
  } else {
    enif_release_binary(&base_bin);
    mpz_clear(rop);
    CLOSE_LOG;
    return enif_make_binary(env, &rop_bin);
  }
}

/*
 * mpz_probab_prime_p
 */

static ERL_NIF_TERM _mpz_probab_prime_p(ErlNifEnv* env, int argc,
                                        const ERL_NIF_TERM argv[]) {
  OPEN_LOG;
  LOG("**** mpz_probab_prime_p\n");

  ErlNifBinary n_bin;
  if (!enif_inspect_binary(env, argv[0], &n_bin)) {
    CLOSE_LOG;
    return enif_make_badarg(env);
  }

  int reps;
  if (!enif_get_int(env, argv[1], &reps)) {
    CLOSE_LOG;
    return enif_make_badarg(env);
  }

  mpz_t n;
  mpz_init(n);
  import_binary(n, &n_bin);
  LOG("n = %s\n", GET_STR(n));

  LOG("mpz_probab_prime_p(_, %d)\n", reps);
  int result = mpz_probab_prime_p(n, reps);
  LOG("result = %d\n", result);

  enif_release_binary(&n_bin);
  mpz_clear(n);
  CLOSE_LOG;
  return enif_make_int(env, result);
}

static ErlNifFunc nif_funcs[] = {
  {"dlog", 3, _dlog, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  {"generate_safe_prime", 1, _generate_safe_prime, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  {"mpz_gcd", 2, _mpz_gcd},
  {"mpz_invert", 2, _mpz_invert},
  {"mpz_lcm", 2, _mpz_lcm},
  {"mpz_powm", 3, _mpz_powm},
  {"mpz_pow_ui", 2, _mpz_pow_ui},
  {"mpz_probab_prime_p", 2, _mpz_probab_prime_p}
};

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
  time_t timeseed;
  time(&timeseed);
  gmp_randinit_default(rand_state);
  gmp_randseed_ui(rand_state, timeseed);
  return 0;
}

ERL_NIF_INIT(gmp_nif, nif_funcs, &load, NULL, NULL, NULL)

// Export and export of binaries

bool export_binary(ErlNifBinary *bin, mpz_t n) {
  size_t size = (mpz_sizeinbase(n, 2) + 7) / 8;
  LOG("size = %zu\n", size);
  if (!enif_alloc_binary(size, bin)) {
    return false;
  }
  size_t countp;
  mpz_export((void *)bin->data, &countp, 1, 1, 0, 0, n);
  for (int i = 0; i < countp; i++) {
    LOG("[%d] -> %d\n", i, bin->data[i]);
  }
  return true;
}

void import_binary(mpz_t n, ErlNifBinary *bin) {
  mpz_import(n, bin->size, 1, 1, 0, 0, bin->data);
}
