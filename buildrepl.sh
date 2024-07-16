#assumes that crosstos/dist/devpac is in your $PATH

m68k-atari-tos-devpac-gen ./replay.s/stubrepl.s
tail -c +29 ./replay.s/stubrepl.PRG | head -c -16 > ./replayer/MYM_REPL.BIN
rm ./replay.s/stubrepl.PRG

cd ./replayer/
m68k-atari-tos-devpac-gen ./MULTSNDH.S
cat MULTSNDH.PRG | tail -c +29 > MULTSNDH.SND
rm MULTSNDH.PRG
