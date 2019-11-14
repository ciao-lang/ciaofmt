:- module(idtokens, [], [assertions, dcg, fsyntax]).

:- doc(title, "Tokenizer").

% TODO: Merge with the system tokenizer; make sure it is robust on invalid syntax

:- use_module(library(messages)).
:- use_module(library(lists)).
:- use_module(library(llists)).

:- use_module(ciaofmt(fmt_style)).

% ---------------------------------------------------------------------------

% Predicate to get the position of the last char in a line
:- export(pos_last_char/3).
pos_last_char(Chars, Pos, Pos) :-
    var(Chars), % this is approximated, because Chars could change later
    !.
pos_last_char([],           Pos,  Pos).
pos_last_char([Char|Chars], Pos0, Pos) :-
    pos_last_char_each(Char, Pos0, Pos1),
    !,
    pos_last_char(Chars, Pos1, Pos).

pos_last_char_each(0'\n, pos(_, Line0), pos(0, Line1)) :-
    Line1 is Line0 + 1.
pos_last_char_each(0'\t, pos(Col0, Line0), pos(Col1, Line0)) :-
    Col1 is (Col0 // 8 + 1) * 8.
pos_last_char_each(_, pos(Col0, Line0), pos(Col1, Line0)) :-
    Col1 is Col0 + 1.

% ---------------------------------------------------------------------------

:- discontiguous(chartype/2).

chartype(lowchar, A) :- lowchar(A).
lowchar(A) :- A >= 0'a, A =< 0'z, !.
lowchar(A) :- A > 127. % TODO: assume anything else is lowchar

chartype(uppchar, A) :- uppchar(A).
uppchar(A) :- A >= 0'A, A =< 0'Z, !.

chartype(expchar, A) :- expchar(A).
expchar(0'e).
expchar(0'E).

chartype(sign, A) :- sign(A).
sign(0'+).
sign(0'-).

alpha(A) :- lowchar(A), !.
alpha(A) :- uppchar(A).

chartype(digit, A) :- digit(A).
digit(A) :- A >= 0'0, A =< 0'9, !.

chartype(hexdigit, A) :- hexdigit(A).
hexdigit(A) :- digit(A).
hexdigit(A) :- A >= 0'a, A =< 0'f, !.
hexdigit(A) :- A >= 0'A, A =< 0'F, !.

chartype(octaldigit, A) :- octaldigit(A).
octaldigit(A) :- 0'0 =< A, A =< 0'7, !.

chartype(bindigit, A) :- bindigit(A).
bindigit(0'0).
bindigit(0'1).

alphanum(A) :- alpha(A), !.
alphanum(A) :- digit(A).

chartype(labelchar, A) :- labelchar(A).
labelchar(A) :- alphanum(A), !.
labelchar(A) :- underscore(A).

underscore(0'_).

chartype(cutchar, A) :- cutchar(A).
cutchar(0'!).

chartype(floatsep, A) :- floatsep(A).
floatsep(0'.).

chartype(endclausechar, A) :- endclausechar(A).
endclausechar(0'.).

chartype(space, A) :- space(A).
space(0' ).
space(0'\n).
space(0'\t).
space(0').
space(0'\r).

chartype(opchar, A) :- opchar(A).
opchar(0'+).
opchar(0'-).
opchar(0'*).
opchar(0'/).
opchar(0'\\).
opchar(0'=).
opchar(0'<).
opchar(0'>).
opchar(0':).
opchar(0'&).
opchar(0'|).
opchar(0'$).
opchar(0'.).
opchar(0'?).
opchar(0';).
opchar(0'~).
opchar(0'#).
opchar(0'^).
opchar(0'@).
opchar(0'`).

chartype(var0, A) :- var0(A).
var0(A) :- uppchar(A), !.
var0(A) :- underscore(A).

chartype(openpar, A) :- openpar(A).
openpar(0'().
openpar(0'[).
openpar(0'{).

chartype(closepar, A) :- closepar(A).
closepar(0')).
closepar(0']).
closepar(0'}).

% ----------------------------------------------------------------------------

:- export(identify_tokens/4).
identify_tokens(Tokens, Source, String0, String) :-
    identify_tokens_(Tokens, String0, String),
    ( member(token(unknown, Value), Tokens) ->
        append(InitString, Value, String0),
        pos_last_char(InitString, pos(0, 1), pos(_Col, Line)),
        (
            append(Value0, "\n" || _, Value) -> true
        ;
            Value0 = Value
        ),
        show_message(error, loc(Source, Line, Line), "Unknown token ~n~s",
            [Value0]),
        fail
    ; true
    ).

:- export(identify_tokens_/3). % TODO: only for tests
identify_tokens_([Token|Tokens]) --> get_token(Token), !, identify_tokens_(Tokens).
identify_tokens_([]) --> "".

get_token(token(TokenType, Value), String0, String) :-
    parse_token(TokenType, String0, String),
    append(Value, String, String0).

parse_token(spaces) -->
    parse_spaces,
    !.
parse_token(string) -->
    parse_string,
    !.
parse_token(openfunc) -->
    parse_atom,
    parse_openpar,
    !.
parse_token(openmeta) -->
    parse_var,
    parse_openpar,
    !.
parse_token(openoper) -->
    parse_operator,
    parse_openpar,
    !.
parse_token(atom) -->
    parse_atom,
    !.
parse_token(number) -->
    parse_number,
    !.
parse_token(cut) -->
    parse_cut,
    !.
parse_token(var) -->
    parse_var,
    !.
parse_token(endclause) -->
    parse_endclause,
    !.
parse_token(comment1) -->
    parse_comment1,
    !.
parse_token(commentn) -->
    parse_commentn,
    !.
parse_token(operator) -->
    parse_operator,
    !.
parse_token(separator) -->
    parse_separator,
    !.
parse_token(openpar) -->
    parse_openpar,
    !.
parse_token(closepar) -->
    parse_closepar,
    !.
parse_token(unknown) -->
    parse_unknown.

parse_spaces -->
    parse_char(space),
    parse_chars(space).

parse_comment1 -->
    "%",
    !,
    parse_comment1_.

parse_comment1_(String, String) :-
    (String == [] ; \+ \+ (String = "\n" || _)),
    !.
parse_comment1_ -->
    [_C],
    !,
    parse_comment1_.

parse_commentn -->
    "/*",
    !,
    parse_commentn_.

parse_commentn_ -->
    "*/",
    !.
parse_commentn_ -->
    [_C],
    parse_commentn_.

parse_unknown -->
    [_C],
    !,
    parse_unknown_.

parse_unknown_ -->
    [_C],
    !,
    parse_unknown_.
parse_unknown_ --> "".

parse_string -->
    parse_enclosed(0'\").

parse_cut -->
    parse_char(cutchar).

parse_atom -->
    "[]",
    !.
parse_atom -->
    parse_aatom,
    !.
parse_atom -->
    parse_qatom,
    !.

parse_aatom -->
    parse_char(lowchar),
    parse_chars(labelchar).

parse_qatom -->
    parse_enclosed(0'\').

parse_number -->
    parse_float.

parse_number -->
    parse_int.

parse_float -->
    parse_char(sign),
    parse_nnegfloat.
parse_float -->
    parse_nnegfloat.

parse_nnegfloat -->
    "0.Nan",
    !.
parse_nnegfloat -->
    "0.Inf",
    !.
parse_nnegfloat -->
    parse_int,
    parse_char(floatsep),
    parse_nnegint,
    optional_exp.

optional_exp -->
    parse_char(expchar),
    parse_int,
    !.
optional_exp --> "".

parse_nnegint -->
    parse_ascii.
parse_nnegint -->
    parse_octal.
parse_nnegint -->
    parse_bin.
parse_nnegint -->
    parse_hex.
parse_nnegint -->
    parse_decimal.

parse_decimal -->
    parse_char(digit),
    parse_chars(digit).

parse_ascii -->
    "0\'\'\'",
    !.
parse_ascii -->
    "0\'\\",
    [_C],
    !.
parse_ascii -->
    "0\'",
    [_C].

parse_octal -->
    "0o",
    parse_octaldigits.

parse_hex -->
    "0x",
    parse_hexdigits.

parse_bin -->
    "2\'",
    parse_bindigits.

parse_bin -->
    "0b",
    parse_bindigits.

parse_hexdigits -->
    parse_char(hexdigit),
    parse_chars(hexdigit).

parse_octaldigits -->
    parse_char(octaldigit),
    parse_chars(octaldigit).

parse_bindigits -->
    parse_char(bindigit),
    parse_chars(bindigit).

parse_int -->
    parse_nnegint,
    !.
parse_int -->
    parse_char(sign),
    parse_nnegint.

parse_var -->
    parse_char(var0),
    parse_chars(labelchar).

%:- meta_predicate parse_chars(pred(1), ?, ?).

parse_chars(CharType) -->
    parse_char(CharType),
    !,
    parse_chars(CharType).
parse_chars(_) --> "".

parse_char(CharType, [C|T], T) :-
    chartype(CharType, C),
    !.

parse_operator -->
    parse_char(opchar),
    parse_chars(opchar).

parse_endclause -->
    parse_char(endclausechar),
    \+ parse_char(opchar),
    \+ parse_char(openpar).

parse_separator --> ",".

parse_openpar -->
    parse_char(openpar).

parse_closepar -->
    parse_char(closepar).

parse_enclosed(EncloseChar) -->
    [EncloseChar],
    parse_enclosed_(EncloseChar).

parse_enclosed_(EncloseChar) -->
    [EncloseChar, EncloseChar],
    !,
    parse_enclosed_(EncloseChar).
parse_enclosed_(EncloseChar) -->
    "\\",
    [C], { C >= 0'0, C =< 0'7 },
    ( [C2], { C2 >= 0'0, C2 =< 0'7 } ; [] ),
    ( "\\" ; [] ), % (optional)
    !,
    parse_enclosed_(EncloseChar).
parse_enclosed_(EncloseChar) -->
    "\\",
    [_C],
    !,
    parse_enclosed_(EncloseChar).
parse_enclosed_(EncloseChar) -->
    [EncloseChar],
    !.
parse_enclosed_(EncloseChar) -->
    [_C],
    parse_enclosed_(EncloseChar).

% ---------------------------------------------------------------------------
% Tests

:- test identify_tokens_(Tokens, String, Tail) : (
        String =
        ":- module(_, _, [assertions,language]).\nmain :- p(a).\n",
        Tail = []
    ) =>
    (
        Tokens =
        [
            token(operator, ":-"),
            token(spaces, " "),
            token(openfunc, "module("),
            token(var, "_"),
            token(separator, ","),
            token(spaces, " "),
            token(var, "_"),
            token(separator, ","),
            token(spaces, " "),
            token(openpar, "["),
            token(atom, "assertions"),
            token(separator, ","),
            token(atom, "language"),
            token(closepar, "]"),
            token(closepar, ")"),
            token(endclause, "."),
            token(spaces, "\n"),
            token(atom, "main"),
            token(spaces, " "),
            token(operator, ":-"),
            token(spaces, " "),
            token(openfunc, "p("),
            token(atom, "a"),
            token(closepar, ")"),
            token(endclause, "."),
            token(spaces, "\n")
        ]
    ).

