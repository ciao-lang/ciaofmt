:- module(reformat_types, [
    functorproc_t/1,
    functordesc_t/1,
    token_t/1],
   [assertions, regtypes]).

:- use_package(ciaofmt(reformat_argnames)).

:- regtype functorproc_t/1.
functorproc_t(functorproc${}).

:- regtype functordesc_t/1.
functordesc_t(functordesc${arg => Arg}) :- list(functordesc_t, Arg).

:- regtype argdesc_t/1.
argdesc_t(argdesc${arg => Arg}) :- list(functordesc_t, Arg).

:- regtype token_t/1.
token_t(token${}).
