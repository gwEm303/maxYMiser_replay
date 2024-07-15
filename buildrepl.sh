/Users/gareth/crosstos/dist/devpac/m68k-atari-tos-devpac-gen ./replay.s/stubrepl.s
dd if=./replay.s/stubrepl.PRG of=./replay.s/stubrepl.BIN bs=1 skip=28
newfsize=$(expr $(stat -f '%z' ./replay.s/stubrepl.BIN) - 16)
dd if=./replay.s/stubrepl.BIN of=./replay.s/MYM_REPL.BIN bs=1 count=$newfsize
rm ./replay.s/stubrepl.BIN ./replay.s/stubrepl.PRG
mv ./replay.s/MYM_REPL.BIN ./replayer/

cd ./replayer/
rm ./MULTSNDH.SND
/Users/gareth/crosstos/dist/devpac/m68k-atari-tos-devpac-gen ./MULTSNDH.S
dd if=MULTSNDH.PRG of=MULTSNDH.SND bs=1 skip=28
rm MULTSNDH.PRG