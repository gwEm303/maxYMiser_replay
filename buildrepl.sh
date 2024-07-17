unameOut="$(uname -s)"

# detect if "vasm" is in your PATH
# (http://sun.hasenbraten.de/vasm/index.php?view=relsrc)
if command -v vasm >&2; then
    echo vasm detected

    vasm -no-opt -Fbin -o SOURCE/MYM_REPL.BIN replay.s/stubrepl.s
    # Probably a bug in vasm? The output file "MYM_REPL.PRG" is created instead of "MYM_REPL.BIN"
    mv SOURCE/MYM_REPL.PRG SOURCE/MYM_REPL.BIN

    cd SOURCE
    vasm -no-opt -Fbin -o MULTSNDH.SND MULTSNDH.S
    cd ..
    exit 0
fi

# detect if crosstos/dist/devpac is in your PATH
# (https://bitbucket.org/pep-entral/crosstos/src/master/)
if command -v m68k-atari-tos-devpac-gen >&2; then
    echo crosstos devpac detected

    m68k-atari-tos-devpac-gen ./replay.s/stubrepl.s
    tail -c +29 ./replay.s/stubrepl.PRG > ./replay.s/stubrepl.BIN
    if [ "$unameOut" = "Darwin" ]; then
        newfsize=$(expr $(stat -f '%z' ./replay.s/stubrepl.BIN) - 4)
        dd if=./replay.s/stubrepl.BIN of=./SOURCE/MYM_REPL.BIN bs=1 count=$newfsize
    else
        head -c -4 ./replay.s/stubrepl.BIN > ./SOURCE/MYM_REPL.BIN
    fi
    rm -f ./replay.s/stubrepl.BIN ./replay.s/stubrepl.PRG

    cd ./SOURCE/
    rm -f ./MULTSNDH.SND
    m68k-atari-tos-devpac-gen ./MULTSNDH.S
    tail -c +29 MULTSNDH.PRG > MULTSNDH.SND
    rm -f MULTSNDH.PRG
    cd ..
    exit 0
fi

echo no supported assembler on path
exit 1
