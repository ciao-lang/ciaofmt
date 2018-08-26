:- module(ciaofmt, [main/1], [assertions, fsyntax]).

:- doc(title, "Automatic source code formatting for Ciao").
:- doc(author, "The Ciao Development Team").

:- doc(stability, alpha).

:- doc(module, "This command offers source code formatting and style
   checking operations for Ciao (and Prolog) source code.

@begin{verbatim}
@includefact{usage_message/1}
@end{verbatim}
").

:- use_module(library(messages), [show_message/3]).
:- use_module(library(stream_utils)).
:- use_module(engine(stream_basic)).
:- use_module(engine(io_aux), [display_string/1]).

:- use_package(ciaofmt(reformat_argnames)).
:- use_module(ciaofmt(reformat)).
:- use_module(ciaofmt(checklines)).
:- use_module(ciaofmt(fmt_style)).

% ---------------------------------------------------------------------------

show_help :-
	usage_message(M),
	display_string(M).

usage_message("
Usage: ciaofmt [<opts>] [<in>] [<out>]

Reformat or check syntax of the input source file. Use '-' to read
from standard input.

Options:
  -h|--help  show this message
  -c         check formatting (do not write output)
  -w         replace the input file (equivalent to <in> <in>)

").

% ---------------------------------------------------------------------------

main(Args) :-
	process_args(Args, Action),
	!,
	( do_process(Action) -> true
	; throw(bug(failed, Action))
	).
main(Args) :-
	show_message(error, "Unknown arguments ~w", [Args]).

process_args(['-h'], help) :- !,
	show_help.
process_args(['--help'], help) :- !,
	show_help.
process_args(['-c', Source], Action) :- !,
	Action = check(Source).
process_args(['-w', Source], Action) :- !,
	Action = reformat(Source, Source).
process_args([], Action) :- !,
	Action = reformat('-', '-').
process_args([Source], Action) :- !,
	Action = reformat(Source, '-').
process_args([Source, Target], Action) :- !,
	Action = reformat(Source, Target).

default_length(80).

do_process(help) :- !,
	show_help.
do_process(check(Source)) :- !, % read from stdin
	Length = ~max_line_length,
	SourceS = ~read_source(Source),
	checklines_string(Source, SourceS, Length). % TODO: passing '-' here is OK?
do_process(reformat(Source, Target)) :- !,
	SourceS = ~read_source(Source),
	reformat(Source, SourceS, TargetS),
	write_target(TargetS, Target).

read_source(Source, String) :-
	( Source = '-' -> % read from stdin
	    current_input(CI),
	    read_to_end(CI, String),
	    close(CI) % TODO: sure?
	; file_to_string(Source, String)
	).

write_target(String, Target) :-
	( Target = '-' -> % write to stdout
	    write_string(String)
	; string_to_file(String, Target)
	).

:- use_module(library(stream_utils)).
:- export(reformat_file/1).
:- export(reformat_file/2).
reformat_file(Source, Target) :-
	file_to_string(Source, SourceS),
	reformat(Source, SourceS, TargetS),
	string_to_file(TargetS, Target).

reformat_file(File) :- reformat_file(File, File).

% ---------------------------------------------------------------------------

:- doc(bug, "@tt{whitespace-mode} for @apl{emacs} offers a similar
   functionality as checklines; use in ciao_emacs?").

