;
; gwEditor
;
; 2024
; Gareth Morris / gwEm
;
; stubrepl.s
          OUTPUT    .PRG
          comment   HEAD=%111       ; bit0=ttram(mem) bit1=ttram(prg) bit2=fastload

BUILD_BIN equ       1               ; build stand-alone binary version
          include   replay.s/sndh.s
