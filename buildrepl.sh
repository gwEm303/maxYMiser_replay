# assumes that "vasm" is in your PATH
vasm -no-opt -Fbin -o replayer/MYM_REPL.BIN replay.s/stubrepl.s

# Probably a bug in vasm? The output file "MYM_REPL.PRG" is created instead of "MYM_REPL.BIN"
mv replayer/MYM_REPL.PRG replayer/MYM_REPL.BIN

cd replayer
vasm -no-opt -Fbin -o MULTSNDH.SND MULTSNDH.S
cd ..

exit 0

# Version that builds the files using crosstos devpac
# assumes that crosstos/dist/devpac is in your PATH

m68k-atari-tos-devpac-gen ./replay.s/stubrepl.s
tail -c +29 ./replay.s/stubrepl.PRG | head -c -4 > ./replayer/MYM_REPL.BIN
rm ./replay.s/stubrepl.PRG

cd ./replayer/
m68k-atari-tos-devpac-gen ./MULTSNDH.S
cat MULTSNDH.PRG | tail -c +29 > MULTSNDH.SND
rm MULTSNDH.PRG
