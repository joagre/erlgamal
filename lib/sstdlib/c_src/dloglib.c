#include <stdio.h>
#include <time.h>
#include <gmp.h>

// Source: https://www.luke.maurits.id.au/files/misc/honours_thesis.pdf
// https://www.alpertron.com.ar/DILOG.HTM

/* Function prototypes */

void pollardrho(mpz_t result, mpz_t pminus1, mpz_t h, mpz_t g, mpz_t p);
void iterate(mpz_t *x, mpz_t *a, mpz_t *b, mpz_t pminus1, mpz_t h, mpz_t g,
             mpz_t p);

/* Global variables */

extern gmp_randstate_t rand_state;

/*******
 * API *
 *******/

void dlog(mpz_t rop, mpz_t h, mpz_t g, mpz_t p) {
  mpz_t pminus1;
  mpz_init(pminus1);
  mpz_sub_ui(pminus1, p, 1);
  pollardrho(rop, pminus1, h, g, p);
}

/*********************************
 * Pollard’s rho-method function *
 *********************************/

void pollardrho(mpz_t rop, mpz_t pminus1, mpz_t h, mpz_t g, mpz_t p) {

  /* Randomly set initial sequence terms */

  mpz_t a1, b1, x1, a2, b2, x2, temp;
  mpz_init(a1);
  mpz_init(b1), mpz_init(x1);
  mpz_init(temp);
  mpz_urandomm(a1, rand_state, p);
  mpz_urandomm(b1, rand_state, p);
  mpz_powm(x1, g, a1, p);
  mpz_powm(temp, h, b1, p);
  mpz_mul(x1, x1, temp);
  mpz_mod(x1, x1, p);
  mpz_clear(temp);
  mpz_init(a2);
  mpz_init(b2);
  mpz_init(x2);
  mpz_set(a2, a1);
  mpz_set(b2, b1);
  mpz_set(x2, x1);

  /* Use Floyd’s algorithm to find sequence collision */

  do {
    iterate(&x1, &a1, &b1, pminus1, h, g, p);
    iterate(&x2, &a2, &b2, pminus1, h, g, p);
    iterate(&x2, &a2, &b2, pminus1, h, g, p);
  } while (mpz_cmp(x1, x2) != 0);

  /* Construct linear congruence */

  mpz_t a, b;    /* a(log) = b (mod p-1) */
  mpz_init(a);
  mpz_init(b);
  mpz_sub(b, a2, a1);
  mpz_mod(b, b, pminus1);
  mpz_sub(a, b1, b2);
  mpz_mod(a, a, pminus1);
  mpz_clear(a1);
  mpz_clear(a2);
  mpz_clear(b1);
  mpz_clear(b2);

  /* Ensure congruence is non-trivial and retry if not */

  if (mpz_cmp_ui(a, 0) == 0) {
    pollardrho(rop, pminus1, h, g, p);
  }

  /* Solve linear congruence if possible and retry otherwise */

  mpz_t gcd, soln;
  mpz_init(gcd);
  mpz_init(soln);
  mpz_gcdext(gcd, soln, NULL, a, pminus1);
  if (mpz_divisible_p(b, gcd)) {
    /* Congruence has a solution, solve it */
    mpz_mul(soln, b, soln);
    mpz_div(soln, soln, gcd);
    mpz_mod(soln, soln, pminus1);
  } else {
    /* Congruence has no solution, try again */
    pollardrho(rop, pminus1, h, g, p);
  }

  /* Perform trial exponentiation on candidates */

  mpz_t step, trial;
  mpz_init(step);
  mpz_init(trial);
  mpz_div(step, pminus1, gcd);
  mpz_mod(soln, soln, step);

  while (1) {
    mpz_powm(trial, g, soln, p);
    if (mpz_cmp(trial, h) == 0)
      break;
    else
      mpz_add(soln, soln, step);
  }

  /* Store rop */

  mpz_set(rop, soln);
}

/************************************
 * Rho sequence updating subroutine *
 ************************************/

void iterate(mpz_t *x, mpz_t *a, mpz_t *b, mpz_t pminus1, mpz_t h, mpz_t g,
             mpz_t p) {
  if (mpz_congruent_ui_p(*x, 0, 3)) {
    mpz_mul(*x, *x, *x);
    mpz_mod(*x, *x, p);
    mpz_mul_ui(*a, *a, 2);
    mpz_mod(*a, *a, pminus1);
    mpz_mul_ui(*b, *b, 2);
    mpz_mod(*b, *b, pminus1);
  } else if (mpz_congruent_ui_p(*x, 1, 3)) {
    mpz_mul(*x, g, *x);
    mpz_mod(*x, *x, p);
    mpz_add_ui(*a, *a, 1);
    mpz_mod(*a, *a, pminus1);
  } else if (mpz_congruent_ui_p(*x, 2, 3)) {
    mpz_mul(*x, h, *x);
    mpz_mod(*x, *x, p);
    mpz_add_ui(*b, *b, 1);
    mpz_mod(*b, *b, pminus1);
  }
}
