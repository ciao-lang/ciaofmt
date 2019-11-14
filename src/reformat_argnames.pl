:- package(reformat_argnames).
:- use_package(argnames).

:- argnames functordesc(name, tokentype, arg, indentlevel, pos, argdescs,
        parlevel).

:- argnames functorproc(name, tokentype, arg, argt, indentlevel, pos,
        argdescs, argdescst, parlevel).

:- argnames argdesc(arg, pos, bookmark).

:- argnames token(type, value).

:- argnames fmtconfig(max_length_line, indentation_style).
