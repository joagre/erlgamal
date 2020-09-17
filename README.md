# Quick start

Pre-requisists: *Install GNU's Multiple Precision Arithmetic Library (GMP)*

```
$ git clone git@github.com:joagre/erlgamal.git
...
$ make
...
$ erl -pa lib/sstdlib/ebin -pa lib/elgamal/ebin -pa lib/elgamal/test
Erlang/OTP 22 [erts-10.7] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe]

Eshell V10.7  (abort with ^G)
1> unit_test_elgamal:start().
Randomized ciphertext size: 1325
Randomized ciphertext size: 1837
Randomized ciphertext size: 2348
Randomized ciphertext size: 2859
Randomized ciphertext size: 3370
Randomized ciphertext size: 3881
Randomized ciphertext size: 4392
Randomized ciphertext size: 4904
Randomized ciphertext size: 5416
Randomized ciphertext size: 5928
ok
2> 
```

# Files

```
./lib/elgamal/src/elgamal.erl
./lib/elgamal/src/elgamal.hrl
```

The multiplicative and additive ElGamal encryption library. More
info is availble in elgamal.erl and elgamal.hrl.

```
./lib/elgamal/test/unit_test_elgamal.erl
```

Test suite which tests the elgamal module. More info is availble in
unit_test_suite.erl.

```
./lib/sstdlib/src/mpz.erl
./lib/sstdlib/src/gmp_nif.erl
./lib/sstdlib/c_src/gmp_nif.c
./lib/sstdlib/c_src/dloglib.c
```

The mpz module makes a number of functions available from GMP:

* generate_safe_prime
* mpz_gcd
* mpz_invert
* mpz_lcm
* mpz_powm
* mpz_pow_ui
* mpz_probab_prime_p
 
Additionally a dlog function is made available to perform fast
calculation of the discrete logarithm using Pollard’s rho-method as
described in https://www.luke.maurits.id.au/files/misc/honours_thesis.pdf

Note: To debug the dlog stuff use the standalone lib/sstdlib/c_src/dlog.c
