:- module(_, [], [ciaobld(bundlehooks)]).

:- use_module(ciaobld(ciaoc_aux), [runtests_dir/2]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).

'$builder_hook'(test) :- !,
	% unit tests
	runtests_dir(ciaofmt, 'src'),
	% TODO: implememt in Prolog
	F = ~bundle_path(ciaofmt, 'tests/runme.sh'),
	process_call(F, [], []).

