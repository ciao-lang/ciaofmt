:- module(ciaofmt, [main/1], [assertions]).

:- doc(title, "Automatic source code formatting for Ciao").
:- doc(author, "Ciao Development Team").

:- doc(module, "This command offers source code formatting and style
   checking operations for Ciao (and Prolog) source code.

@begin{verbatim}
@includefact{usage_message/1}
@end{verbatim}
").

:- use_module(library(strings),  [write_string/1]).
:- use_module(library(messages), [show_message/3]).
:- use_module(library(file_utils)).

:- use_package(ciaofmt(reformat_argnames)).
:- use_module(ciaofmt(reformat)).
:- use_module(ciaofmt(checklines)).

% ---------------------------------------------------------------------------

show_help :-
	usage_message(M),
	display_string(M).

usage_message("
Usage: ciaofmt [<opts>] [<cmd>] [[<input>] <output>]|[--help]

Reformat the input source code into the given output file. If input is
omitted, program is read from standard input. Use '-' to emit output
to standard error.

Options:
  --help    show this message

Commands:
  -f        reformat (default command)
  -w        do not reformat, just show formatting warnings
            (e.g., line length)

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

process_args(['--help'], help) :- !,
	show_help.
process_args(['-w', Source], Action) :- !,
	default_length(Length), % TODO: extract from fmt_style
	Action = check(Source, Length).
process_args(['-w'], Action) :- !,
	default_length(Length), % TODO: extract from fmt_style
	Action = check('', Length).
process_args([Source, '-'], Action) :- !,
	Action = reformat(Source, '').
process_args([Source, Target], Action) :- !,
	Action = reformat(Source, Target).
process_args([Source], Action) :- !,
	Action = reformat(Source, Source).
process_args([], Action) :-
	Action = reformat('', '').

default_length(80).

do_process(help) :- !,
	show_help.
do_process(check('', Length)) :- !, % read from stdin
	current_input(CI),
	stream_to_string(CI, String),
	checklines_string('-', String, Length). % TODO: passing '-' here is OK?
do_process(check(FileName, Length)) :- !, % read from file
	file_to_string(FileName, String),
	checklines_string(FileName, String, Length).
do_process(reformat('', '')) :- !, % read from stdin, write to stdout
	current_input(CI),
	stream_to_string(CI, SourceS),
	reformat(_Source, SourceS, Target),
	write_string(Target).
do_process(reformat(Source, '')) :- !, % write to stdout
	file_to_string(Source, SourceS),
	reformat(Source, SourceS, Target),
	write_string(Target).
do_process(reformat(Source, Source)) :- !, % write to same file
	reformat_file(Source).
do_process(reformat(Source, Target)) :- !, % write to other file
	reformat_file(Source, Target).

% ---------------------------------------------------------------------------

:- doc(bug, "@tt{whitespace-mode} for @apl{emacs} offers a similar
   functionality as checklines; use in ciao_emacs?").

