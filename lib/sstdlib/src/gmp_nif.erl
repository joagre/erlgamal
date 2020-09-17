-module(gmp_nif).
-export([dlog/3, generate_safe_prime/1, mpz_gcd/2, mpz_invert/2, mpz_lcm/2,
         mpz_powm/3, mpz_pow_ui/2, mpz_probab_prime_p/2]).
-on_load(init/0).

%% Exported: init

init() ->
    ok = erlang:load_nif(filename:join(code:priv_dir(sstdlib), ?MODULE), 0).

%% Exported: dlog

dlog(_H, _G, _P) ->
    exit(nif_library_not_loaded).

%% generate_safe_prime

generate_safe_prime(_Len) ->
    exit(nif_library_not_loaded).

%% Exported: mpz_gcd

mpz_gcd(_Op1, _Op2) ->
    exit(nif_library_not_loaded).

%% Exported: mpz_invert

mpz_invert(_Op1, _Op2) ->
    exit(nif_library_not_loaded).

%% Exported: mpz_lcm

mpz_lcm(_Op1, _Op2) ->
    exit(nif_library_not_loaded).

%% Exported: mpz_powm

mpz_powm(_Base, _Exp, _Mod) ->
    exit(nif_library_not_loaded).

%% Exported: mpz_pow_ui

mpz_pow_ui(_Base, _Exp) ->
    exit(nif_library_not_loaded).

%% Exported: mpz_probab_prime_p

mpz_probab_prime_p(_N, _Reps) ->
    exit(nif_library_not_loaded).
