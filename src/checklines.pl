:- module(checklines, [checklines/1, checklines/2, checklines_string/3],
        [assertions, hiord, fsyntax]).

:- use_module(library(stream_utils)).
:- use_module(library(lists)).
:- use_module(library(hiordlib), [maplist/3]).
:- use_module(engine(stream_basic)).
:- use_module(engine(messages_basic), [messages/1]).

:- doc(author, "Edison Mera").

:- doc(module, "This module helps to verify that a file does not
    have lines with a length higher than a given number of
    characters.").

checklines(Alias) :-
    checklines(Alias, 80).

checklines(Alias, Length) :-
    absolute_file_name(Alias, FileName),
    file_to_string(FileName, String),
    checklines_string(FileName, String, Length).

checklines_string(FileName, String, Length) :-
    lines_too_long(String, Length, LinesTooLong),
    maplist(message_line_too_long(FileName), LinesTooLong, Messages),
    messages(Messages).

message_line_too_long(FileName, Line,
    message_lns(FileName, Line, Line, warning, ['Line too long'])).

lines_too_long(String, Length, LinesTooLong) :-
    lines_too_long_(1, String, Length, [], LinesTooLong).

lines_too_long_(N, String, Length, L0, L) :-
    append(Pre, "\n"|| Post, String), !,
%       list_concat([Pre, "\n", Post], String),!,
    checkline(N, Pre, Length, L0, L1),
    N2 is N + 1,
    lines_too_long_(N2, Post, Length, L1, L).
lines_too_long_(N, String, Length, L0, L) :-
    checkline(N, String, Length, L0, L).

checkline(N, Pre, Length, L0, L) :-
    length(Pre, M),
    ( Length < M ->
        L = [N|L0]
    ;
        L = L0
    ).
