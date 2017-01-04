#!/bin/sh

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

cd "$_base"

ciaofmt indent_example.pl indent_example.pl-out
if diff indent_example.pl-out indent_example.pl-good; then
    echo "[OK] Files are the same"
else    
    echo "[??] Please check the differences"
fi
