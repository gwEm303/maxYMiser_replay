@echo off

rem assumes that "vasm" is in your PATH
vasm -no-opt -Fbin -o replayer\MYM_REPL.BIN replay.s\stubrepl.s

rem Probably a bug in vasm? The output file "MYM_REPL.PRG" is created instead of "MYM_REPL.BIN"
ren replayer\MYM_REPL.PRG MYM_REPL.BIN

cd replayer
vasm -no-opt -Fbin -o MULTSNDH.SND MULTSNDH.S
cd ..

