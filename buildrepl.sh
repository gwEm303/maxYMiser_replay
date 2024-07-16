# detect if "vasm" is in your PATH
if command -v vasm >&2; then
    echo vasm detected

    vasm -no-opt -Fbin -o replayer/MYM_REPL.BIN replay.s/stubrepl.s
    # Probably a bug in vasm? The output file "MYM_REPL.PRG" is created instead of "MYM_REPL.BIN"
    mv replayer/MYM_REPL.PRG replayer/MYM_REPL.BIN

    cd replayer
    vasm -no-opt -Fbin -o MULTSNDH.SND MULTSNDH.S
    cd ..
    exit 0
fi

# detect if crosstos/dist/devpac is in your PATH    
if command -v m68k-atari-tos-devpac-gen >&2; then
    echo crosstos devpac detected

    unameOut="$(uname -s)"

    m68k-atari-tos-devpac-gen ./replay.s/stubrepl.s
    tail -c +29 ./replay.s/stubrepl.PRG > ./replay.s/stubrepl.BIN
    if [ "$unameOut" = "Darwin" ]; then
        newfsize=$(expr $(stat -f '%z' ./replay.s/stubrepl.BIN) - 4)    
        dd if=./replay.s/stubrepl.BIN of=./replay.s/MYM_REPL.BIN bs=1 count=$newfsize
    else
        head -c -4 ./replay.s/stubrepl.BIN > ./replay.s/MYM_REPL.BIN
    fi
    rm ./replay.s/stubrepl.BIN ./replay.s/stubrepl.PRG
    mv ./replay.s/MYM_REPL.BIN ./replayer/

    cd ./replayer/
    rm ./MULTSNDH.SND
    m68k-atari-tos-devpac-gen ./MULTSNDH.S
    cat MULTSNDH.PRG | tail -c +29 > MULTSNDH.SND
    rm MULTSNDH.PRG
    cd ..
    exit 0
fi

echo no supported assembler on path
exit 1
