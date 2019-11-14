:- module(fmt_style, [], [assertions, basicmodes, nortchecks, regtypes, fsyntax]).

:- doc(title, "Formatting style").

:- use_package(ciaofmt(reformat_argnames)).

:- export(indentation_style_t/1).
:- regtype indentation_style_t/1.

indentation_style_t := full_spaced|norm_spaced|left_spaced|save_spaces.

:- export(fmtconfig_t/1).
:- regtype fmtconfig_t/1.
fmtconfig_t(fmtconfig${max_length_line => MLL, indentation_style => IS}) :-
    nnegint(MLL),
    indentation_style_t(IS).

% ---------------------------------------------------------------------------
% Default style

:- doc(bug, "Not the official style!").
:- doc(bug, "Allow customization here").

:- export(init_fmtconfig/1).
:- pred init_fmtconfig(?fmtconfig_t) # "Get default configuration values".
init_fmtconfig(fmtconfig${
            max_length_line => ~max_line_length, % TODO: ignored for reformatting
            indentation_style => norm_spaced
        }).

:- export(max_line_length/1).
max_line_length := 80.

% ---------------------------------------------------------------------------
% used in normspaced.pl

:- export(remove_currspace/3).
remove_currspace(comment1, _,         _) :- !, fail.
remove_currspace(_,        separator, _) :- !.
remove_currspace(_,        endclause, _).

:- export(not_autospace/4).
% not_autospace(operator, atom,      ":",  _).
% not_autospace(operator, atom,      "~",  _).
% not_autospace(atom,     operator,  _,    ":").
% not_autospace(atom,     operator,  _,    "/").
% not_autospace(atom,     operator,  _,    "$").

not_autospace(atom,     operator,  _,    _) :- !.
not_autospace(operator, atom,      _,    _) :- !.
not_autospace(var,      operator,  _,    ":") :- !.
not_autospace(operator, var,       _,    _) :- !.
not_autospace(operator, openfunc,  _,    _) :- !.
not_autospace(operator, openmeta,  _,    _) :- !.
not_autospace(operator, openpar,   _,    _) :- !.
not_autospace(operator, number,    _,    _) :- !.
not_autospace(atom,     openpar,   _,    _) :- !.
not_autospace(var,      openpar,   _,    _) :- !.
not_autospace(var,      operator,  _,    _) :- !.
not_autospace(var,      number,    _,    _) :- !.
not_autospace(number,   operator,  _,    _) :- !.
not_autospace(string,   operator,  _,    "||") :- !.
not_autospace(operator, string,    "||", _) :- !.
not_autospace(operator, var,       "||", _) :- !.
not_autospace(spaces,   _,         _,    _) :- !.
not_autospace(_,        separator, _,    _) :- !.
not_autospace(_,        endclause, _,    _) :- !.

:- export(autospace_style/6).
autospace_style(full_spaced, _,          _,          _,      _,      " ").
autospace_style(norm_spaced, TokenType0, TokenType1, Value0, Value1, Space) :-
    autospace_norm_spaced(TokenType0, TokenType1, Value0, Value1, Space).
autospace_style(save_spaces, TokenType0, TokenType1, Value0, Value1, Space) :-
    autospace_save_spaces(TokenType0, TokenType1, Value0, Value1, Space).
autospace_style(left_spaced, TokenType0, TokenType1, Value0, Value1, Space) :-
    autospace_left_spaced(TokenType0, TokenType1, Value0, Value1, Space).

autospace_norm_spaced(openpar,   _,         _,   _,    "") :- !.
autospace_norm_spaced(openoper,  _,         _,   _,    "") :- !.
autospace_norm_spaced(openfunc,  _,         _,   _,    "") :- !.
autospace_norm_spaced(openmeta,  _,         _,   _,    "") :- !.
autospace_norm_spaced(_,         closepar,  _,   _,    "") :- !.
autospace_norm_spaced(operator,  _,         "|", _,    "") :- !.
autospace_norm_spaced(operator,  operator,  _,   _,    " ") :- !.
autospace_norm_spaced(operator,  openoper,  _,   _,    " ") :- !.
autospace_norm_spaced(_,         operator,  _,   "|",  "") :- !.
autospace_norm_spaced(separator, openoper,  _,   _,    " ") :- !.
autospace_norm_spaced(atom,      openoper,  _,   "${", "") :- !.
autospace_norm_spaced(atom,      openoper,  _,   _,    " ") :- !.
autospace_norm_spaced(_,         openoper,  _,   _,    "") :- !.
autospace_norm_spaced(_,         separator, _,   _,    "") :- !.
autospace_norm_spaced(_,         _,         _,   _,    " ") :- !.

autospace_save_spaces(atom,     openfunc, _, _, " ") :- !.
autospace_save_spaces(atom,     openmeta, _, _, " ") :- !.
autospace_save_spaces(operator, operator, _, _, " ") :- !.
autospace_save_spaces(operator, openoper, _, _, " ") :- !.
autospace_save_spaces(_,        _,        _, _, "") :- !.

autospace_left_spaced(openpar,   _,        _,   _,    " ") :- !.
autospace_left_spaced(openoper,  _,        _,   _,    " ") :- !.
autospace_left_spaced(openfunc,  _,        _,   _,    " ") :- !.
autospace_left_spaced(openmeta,  _,        _,   _,    " ") :- !.
autospace_left_spaced(_,         closepar, _,   _,    "") :- !.
autospace_left_spaced(operator,  _,        "|", _,    "") :- !.
autospace_left_spaced(operator,  operator, _,   _,    " ") :- !.
autospace_left_spaced(operator,  openoper, _,   _,    " ") :- !.
autospace_left_spaced(_,         operator, _,   "|",  "") :- !.
autospace_left_spaced(separator, openoper, _,   _,    " ") :- !.
autospace_left_spaced(atom,      openoper, _,   "${", "") :- !.
autospace_left_spaced(atom,      openoper, _,   _,    " ") :- !.
autospace_left_spaced(_,         openoper, _,   _,    "") :- !.
autospace_left_spaced(_,         _,        _,   _,    " ") :- !.

% ---------------------------------------------------------------------------
% used in idfunctors.pl:

:- export(not_auto_tabuled/2).
not_auto_tabuled(norm_spaced) := closepar.

:- export(is_opener/1).
is_opener := openpar|openfunc|openmeta|openoper.

:- export(is_end_argument/1).
is_end_argument := closepar|separator.

:- export(indent_level_t/1).
:- regtype indent_level_t/1.
indent_level_t(operator).
indent_level_t(openpar(N, S)) :-
    nnegint(N),
    list(S).

:- export(token_type_t/1).
:- regtype token_type_t/1.

token_type_t := spaces|string|openfunc|openmeta|openoper|atom|number|cut|var|
    endclause|comment1|commentn|operator|separator|openpar|closepar|unknown.

:- export(update_indent/3).
:- pred update_indent(+token_type_t, ?list(indent_level_t),
        ?list(indent_level_t)).

update_indent(operator,  [],             [operator]) :- !.
update_indent(endclause, [operator],     []) :- !.
update_indent(closepar,  [openpar(_)|I], I) :- !.
update_indent(openpar,   I,              [openpar(_)|I]) :- !.
update_indent(openfunc,  I,              [openpar("" -_)|I]) :- !.
update_indent(openmeta,  I,              [openpar("" -_)|I]) :- !.
update_indent(openoper,  I,              [openpar("" -_)|I]) :- !.
update_indent(_,         I,              I).

:- export(update_alines/3).
update_alines(closepar,  L0, L) :- !, (L0 > 0 -> L is L0 - 1 ; L is 0).
update_alines(endclause, _,  0) :- !.
update_alines(_,         L,  L).

:- export(bookmark_begin/2).
bookmark_begin(separator, _).
bookmark_begin(endclause, _).
bookmark_begin(operator) := "-->"|":-"|";"|"->"|":=".

% ---------------------------------------------------------------------------
% used in reformat.pl

:- export(requires_previous_indent_level/2).
requires_previous_indent_level(operator, A) :-
    requires_previous_indent_level_op(A).
requires_previous_indent_level(closepar, _).

requires_previous_indent_level_op := ";"|"->"|"#".
