; Changes by GGN to enable assembling this source with vasm and still be 1:1 binary compatible with
; what devpac 3 outputs.
; Notable changes:
; - add/sub/and.X #constant,Dn -> addi/subi/andi.X #constant,Dn
; - lea 0(an),am -> lea (an),am (there are various permuatations of this around the source,
;                               sometimes it was solved by an ifeq/else/endif block. Lines commented with "ggn"
;                               are the changes)
; - A couple of "even" directives added to force alignment as vasm is more strict
; - Pointer offsets replaced with "RS.X" statements, so binary trim is not needed any more

                opt         CHKPC                   ;make sure PC relative code
                opt         o+                      ;optimisations on
                opt         p=68040                 ;we have some 030 and 040 instructions for compatability purposes


;..................................................................................
                IFNE    BUILD_BIN
HALF_MEG        equ         0
;SEQ_LENGTH      equ         31+1                    ; 31 word steps + byte length + byte repeat
;SEQ_LEN_LOG2    equ         5
SEQ_LENGTH      equ         63+1                    ; 63 word steps + byte length + byte repeat
SEQ_LEN_LOG2    equ         6
                ENDC

                IFEQ    HALF_MEG
SID_STEPS       equ         16                      ;maximum number of steps for timer effects       default = 16
SYNCBUZZ_STEPS  equ         8                       ;                                                default = 8
FM_STEPS        equ         8                       ;                                                default = 8
FMB_STEPS       equ         8                       ;                                                default = 8
FMSB_STEPS      equ         8                       ;                                                default = 8
SYNSQU_STEPS    equ         4                       ;due to size of routine 4 is maximum             default = 4
                ELSE
SID_STEPS       equ         4                       ; reduced number of steps to fit in 520 machines
SYNCBUZZ_STEPS  equ         2                       ;
FM_STEPS        equ         0                       ;
FMB_STEPS       equ         2                       ;
FMSB_STEPS      equ         2                       ;
SYNSQU_STEPS    equ         0                       ;
                ENDC

EFFECTCODES     equ         1                       ;parse tracker effect codes

PULWID_TUNE     equ         4
PULWID_TUNE_020 equ         2
YM_FREQ_RESET   equ         2

                IFEQ    HALF_MEG
INCLUDE_020     equ         1                       ;include non-SMC timer routines
                ELSE
INCLUDE_020     equ         0
                ENDC

                IFEQ    HALF_MEG
STE_DMA         equ         1                       ;include STe DMA digidrums
STE_DMA_VOLUME  equ         1                       ;include STe DMA volume control (**recommended** for 2 DMA channels)
                ELSE
STE_DMA         equ         0
STE_DMA_VOLUME  equ         0
                ENDC

;timer effect routine enables
SID_A           equ         1
SYNCBUZZER_A    equ         1
FMB_A           equ         1
FMSB_A          equ         1
PWM_A           equ         1
                IFEQ    HALF_MEG
FM_A            equ         1
SYNCSQUARE_A    equ         1
DIGIDRUMS_A     equ         1
                ELSE
FM_A            equ         0
SYNCSQUARE_A    equ         0
DIGIDRUMS_A     equ         0
                ENDC

;timer effect routine enables
SID_B           equ         1
SYNCBUZZER_B    equ         1
FMB_B           equ         1
FMSB_B          equ         1
PWM_B           equ         1
                IFEQ    HALF_MEG
FM_B            equ         1
SYNCSQUARE_B    equ         1
DIGIDRUMS_B     equ         1
                ELSE
FM_B            equ         0
SYNCSQUARE_B    equ         0
DIGIDRUMS_B     equ         0
                ENDC

;timer effect routine enables
SID_D           equ         1
SYNCBUZZER_D    equ         1
FMB_D           equ         1
FMSB_D          equ         1
PWM_D           equ         1
                IFEQ    HALF_MEG
FM_D            equ         1
SYNCSQUARE_D    equ         1
DIGIDRUMS_D     equ         1
                ELSE
FM_D            equ         0
SYNCSQUARE_D    equ         0
DIGIDRUMS_D     equ         0
                ENDC

TIMER_A         equ         SYNCSQUARE_A+SID_A+SYNCBUZZER_A+DIGIDRUMS_A+FM_A+FMB_A+FMSB_A+PWM_A
TIMER_B         equ         SYNCSQUARE_B+SID_B+SYNCBUZZER_B+DIGIDRUMS_B+FM_B+FMB_B+FMSB_B+PWM_B
TIMER_D         equ         SYNCSQUARE_D+SID_D+SYNCBUZZER_D+DIGIDRUMS_D+FM_D+FMB_D+FMSB_D+PWM_D

INSTAOFFSET     equ         0
INSTBOFFSET     equ         INSTAOFFSET+128
INSTCOFFSET     equ         INSTBOFFSET+128
INSTDOFFSET     equ         INSTCOFFSET+128

;..................................................................................
                section     text

SNDHstart:      bra.w       playnow                 ;+$00
                bra.w       stopnow                 ;+$04
                bra.w       replayrout              ;+$08
                IFNE    BUILD_BIN
                bra.w       globalvolume            ;+$0C
                bra.w       manualplaynote          ;+$10
                bra.w       setnextsongpos          ;+$14
                ENDC

                IFEQ    BUILD_BIN
                dc.b        "SNDH"                  ;+$0C    SNDH start tag

                dc.b        "COMM"
artistname:     ds.b        27                      ;26 letters, 1 NULL
                even

                dc.b        "TITL"
songname:       ds.b        27                      ;26 letters, 1 NULL
                even

                dc.b        "RIPP"
                dc.b        "Not ripped (original track)",0
                even

                dc.b        "CONV"
converterstring dc.b        "maxYMiser by Gareth Morris (c) 20"
converterstrend dc.b        "24",0                  ;desensitise to year digits.... assume i won't be doing an update in 2100 ;)
                even

                dc.b        "YEAR"
sndhyearstring: dc.b        "2024",0
                even

                dc.b        "##01",0                ;single tune
sndhinterrupt:  dc.b        "TCxxx",0               ;timer C frequency
                even

                dc.b        "TIME"
sndhtime:       dc.w        0                       ;track time in seconds 0=infinite looping
                even

                dc.b        "HDNS"                  ;SNDHv2 end tag
                ENDC

demozyncroniser ds.b        1                       ; +$18 (binary replayer)
                even

;..................................................................................
                dc.b        " -*- maxYMiser replay routine by gwEm/PHF (Gareth Morris) (c) 2024 -*- "
                dc.b        0
                incbin      replay.s/aboutv.txt
                dc.b        0
                even

;..................................................................................
                IFNE    BUILD_BIN
                ; pass values of d0.w from 0 to 127
globalvolume:   movem.l     d0-d1/a0,-(sp)
                andi.l      #$7F,d0             ; protection
                move.w      d0,d1
                lsr.b       #3,d1
                neg.b       d1
                addi.b      #15,d1
                lea         globalvolume(pc),a0
                add.l       #instrumentstart-globalvolume,a0
                move.b      d1,94+INSTAOFFSET(a0)
                move.b      d1,94+INSTBOFFSET(a0)
                move.b      d1,94+INSTCOFFSET(a0)
                IFNE    STE_DMA
                IFNE    STE_DMA_VOLUME
                move.w      d0,d1
                divu        #15,d1
                neg.b       d1
                addq.b      #8,d1
                move.b      d1,24+INSTDOFFSET(a0)
                move.b      d1,25+INSTDOFFSET(a0)
                ENDC
                ENDC
                movem.l     (sp)+,d0-d1/a0
                rts
                ENDC

;..................................................................................

manualplaynote: movem.l   d0-d7/a0-a6,-(sp)                   ;d0.b=note d1.b=instrument d2.b=channel
                move.w    sr,-(sp)
                move.w    #$2700,sr                           ;interrupts off when modifying instruments

                lea       manualplaynote(pc),a0
                add.l     #trakerdatapoint-manualplaynote,a0
                add.l     (a0),a0
                addq.l    #8,a0                               ;a0 points to tracker data

                tst.b     25(a0)                              ;dont play if scroll=off 11(a0) and play=on 25(a0)and record=on 26(a0)
                beq.s     .start
                tst.b     26(a0)
                beq.s     .start
                tst.b     11(a0)
                beq       .end

.start          andi.l    #$FF,d0                             ;protection
                andi.l    #$FF,d1
                andi.l    #$FF,d2

                bsr       setup_dma                           ;dma setup needed for dma and ym voices

                lea       manualplaynote(pc),a1
                tst.b     d2
                beq.s     .setA
                cmpi.b    #1,d2
                beq.s     .setB
                cmpi.b    #2,d2
                beq.s     .setC
                cmpi.b    #3,d2
                beq.s     .setD
                cmpi.b    #4,d2
                beq.s     .setE
                bra       .end

.setA           add.l     #instrumentA-manualplaynote,a1
                bsr       setuptimerA            ;assume a0 points to tracker data
                bra       .cont
.setB           add.l     #instrumentB-manualplaynote,a1
                bsr       setuptimerB            ;assume a0 points to tracker data
                bra       .cont
.setC           add.l     #instrumentC-manualplaynote,a1
                bsr       setuptimerD            ;assume a0 points to tracker data
                bra.s     .cont
.setD           add.l     #instrumentD-manualplaynote,a1
                cmpi.b    #4,49(a0)
                beq.s     .setDnote
.clipDnote      cmpi.b    #68,d0
                blo.s     .setDnote
                subi.b    #12,d0
                bra.s     .clipDnote
.setDnote       move.b    d0,(a1)                ; note
                cmpi.b    #8,d1                  ; maximum sample number
                bls.s     .setDsamp
                moveq     #0,d1
.setDsamp       move.b    d1,1(a1)               ; sample
                clr.b     8(a1)                  ; playing
                move.b    58(a0),d0              ; editor volume
                andi.b    #$F,d0                 ; ensure in range
                move.b    d0,2(a1)               ; volume
                bra       .end
.setE           add.l     #instrumentD-manualplaynote,a1
                cmpi.b    #4,49(a0)
                beq.s    .setEnote
.clipEnote      cmpi.b    #68,d0
                blo.s     .setEnote
                subi.b    #12,d0
                bra.s     .clipEnote
.setEnote       move.b    d0,3(a1)               ; note
                cmpi.b    #8,d1                  ; maximum sample number
                bls.s     .setEsamp
                moveq     #0,d1
.setEsamp       move.b    d1,4(a1)               ; sample
                clr.b     9(a1)                  ; playing
                move.b    58(a0),d0              ; editor volume
                andi.b    #$F,d0                 ; ensure in range
                move.b    d0,5(a1)               ; volume
                bra.s     .end

.cont           bsr       setup_vecr_spi         ; assume a0 points to tracker data
                move.b    d0,127(a1)             ; set current note

                tst.b     d1                     ; if inst zero, just modify playing note
                beq.s     .end                   ; and dont change instrument dara

                move.b    d1,122(a1)             ; set current instr

                subq.b    #1,d1
                lsl.w     #6,d1                  ; 64 bytes per inst

                lea       .cont(pc),a6
                add.l     #voicedatapoint-.cont,a6
                add.l     (a6),a6                ; a6 is digi0point
                lea       40+16(a6),a6           ; +40 takes us to instrumentdata +16 takes us to portamento flag
                add.l     d1,a6                  ; a0 points to end of inst

                movem.l   (a6),d0-d7/a2-a5
                movem.l   d0-d7/a2-a5,(a1)       ; write instrument to buffer (12 longs)

                lea       4*12(a1),a1

                moveq     #0,d0
                moveq     #0,d1
                moveq     #0,d2
                moveq     #0,d3
                moveq     #0,d4
                moveq     #0,d5
                moveq     #0,d6
                moveq     #0,d7
                move.l    d0,a2
                move.l    d0,a3
                move.l    d0,a4
                movem.l   d0-d7/a2-a4,(a1)       ; clear instrument specific data (11 longs)

                move.b    58(a0),d0              ; editor volume
                beq.s     .end                   ; if editor volume not set
                andi.b    #$F,d0                 ; ensure in range
                move.b    d0,51-12*4(a1)         ; set volume as editor volume

.end            move.w    (sp)+,sr
                movem.l   (sp)+,d0-d7/a0-a6
                rts

;..................................................................................
setnextsongpos: move.l    a0,-(sp)               ; d0.b = song position

                lea       setnextsongpos(pc),a0
                add.l     #trakerdatapoint-setnextsongpos,a0
                add.l     (a0),a0
                addq.l    #8,a0                  ; a0 points to tracker data

                move.b    d0,(a0)                ; set current song position
                move.b    d0,38(a0)              ; set next song position

                move.l    (sp)+,a0
                rts

;..................................................................................
playnow:        movem.l     d0-d1/a0-a1,-(sp)

                lea         demozyncroniser(pc),a0
                clr.b       (a0)                    ;clear demo syncroniser

                lea         playnow(pc),a0          ;lea trakerdatapoint(pc),a0
                add.l       #trakerdatapoint-playnow,a0
                add.l       (a0),a0
                addq.l      #8,a0                   ;a0 points to tracker data

                bsr         gen_patterntab          ;generate pattern table

                IFNE    TIMER_A+TIMER_B+TIMER_D
                bsr         cpu_dep_reloc           ;cpu dependant relocation
.noreloc
                ENDC
                IFNE    TIMER_A
                bsr         setuptimerA             ;assume a0 points to tracker data
                ENDC
                IFNE    TIMER_B
                bsr         setuptimerB
                ENDC
                IFNE    TIMER_D
                bsr         setuptimerD
                ENDC
                IFNE    TIMER_A+TIMER_B+TIMER_D
                bsr         setup_vecr_spi
                ENDC

                bsr         setup_dma
                bsr         save_ym

                clr.b       1(a0)                   ;current pattern position
                clr.l       44(a0)                  ;clear track pattern position offsets
                clr.b       48(a0)                  ;song speed counter

                lea         playnow(pc),a1          ;instrumentstart(pc),a1
                add.l       #instrumentstart-playnow,a1
                clr.l       INSTAOFFSET+122(a1)     ;reinit instruments all round (fix for editor)
                clr.l       INSTBOFFSET+122(a1)
                clr.l       INSTCOFFSET+122(a1)

                IFEQ    BUILD_BIN
                IFEQ    HALF_MEG
                lea         editortag(pc),a1
                cmpi.l      #"EDIT",(a1)
                bne.s       .nomidistart
                bsr         sendmidistart
                ENDC
                ENDC

.nomidistart    tst.b       11(a0)                  ;scrolling
                beq.s       .notscrolling
                clr.b       2(a0)                   ;current pattern position editor

.notscrolling   tst.b       27(a0)                  ;test if in pattern play
                bne.s       .yespatternplay

.songplay
                IFEQ    BUILD_BIN
                ;lea        editortag(pc),a1        ;if song play, we change to the current song position
                cmpi.l      #"EDIT",(a1)
                beq.s       .noresetsongpos
                ENDC

                clr.b       (a0)                    ;start replay from song position 0
                ;clr.b       39(a0)                  ;turn off live mode
.noresetsongpos moveq       #0,d0
                move.b      (a0),d0                 ;current song position
                move.b      d0,38(a0)               ;next song position
                addq.b      #1,38(a0)
                lsl.w       #2,d0                   ;4 bytes per song entry
                lea         64(a0),a1               ;song data
                add.l       d0,a1
                move.l      (a1),d0

                move.l      d0,d1                   ; check for special loop pattern
                cmpi.b      #$00FD,d1
                beq.s       .nextsongpos
                andi.w      #$FF00,d1
                cmpi.w      #$FD00,d1
                beq.s       .nextsongpos
                swap        d1
                cmpi.b      #$00FD,d1
                beq.s       .nextsongpos
                andi.w      #$FF00,d1
                cmpi.w      #$FD00,d1
                beq.s       .nextsongpos

.ignorerepeat   move.l      d0,4(a0)                ;modify 4 current patterns
                move.l      d0,52(a0)               ;modify 4 next patterns
                st.b        25(a0)                  ;set playing byte
                movem.l     (sp)+,d0-d1/a0-a1
                rts

.nextsongpos    move.b      (a0),d0
                beq.s       .ignorerepeat           ; 1st song pos? - if so ignore looping pattern
                addq.b      #1,d0
                cmp.b       64+1024(a0),d0          ;last song pos? - if so ignore looping pattern
                bhs.s       .ignorerepeat
.norepeat       move.b      d0,(a0)
                bra.s       .noresetsongpos


.yespatternplay st.b        25(a0)                  ;set playing byte

                movem.l     (sp)+,d0-d1/a0-a1
                rts

;..................................................................................

stopnow:        movem.l     d0/a0-a2,-(sp)
                lea         stopnow(pc),a0          ;trakerdatapoint(pc),a0
                add.l       #trakerdatapoint-stopnow,a0
                add.l       (a0),a0
                addq.l      #8,a0                   ;a0 points to tracker data
                clr.b       25(a0)                  ;clear playing byte

                lea         stopnow(pc),a1          ; clear instrument buffers and pattern table
                lea         stopnow(pc),a2
                add.l       #stopclearend-stopnow,a1
                add.l       #stopclearstart-stopnow,a2
.cleartop       clr.w       (a2)+
                cmpa.l      a2,a1
                bne.s       .cleartop

                IFNE    TIMER_A
                bsr         restoretimerA
                ENDC
                IFNE    TIMER_B
                bsr         restoretimerB
                ENDC
                IFNE    TIMER_D
                bsr         restoretimerD
                ENDC
                IFNE    TIMER_A+TIMER_B+TIMER_D
                IFEQ    BUILD_BIN
                lea         editortag(pc),a1        ;test if in editor
                cmpi.l      #"EDIT",(a1)
                beq.s       .skiprestoreints
                ENDC
                bsr         restore_ints
.skiprestoreints:
                bsr         rest_vecr_spi
                ENDC

                IFEQ    BUILD_BIN
                lea         editortag(pc),a1        ;test if in editor
                cmpi.l      #"EDIT",(a1)
                bne.s       .restoredmaym           ;if not restore dma and ym
                IFEQ    HALF_MEG
                bsr         sendmidistop
                ENDC
                bra.s       .ymoff                  ;in editor we dont restore the dma
                ENDC
.restoredmaym   bsr         restore_dma
                bsr.s       restore_ym

.ymoff          move.l      #$08000000,$ffff8800.w  ; ch A volume
                move.l      #$09000000,$ffff8800.w  ; ch B volume
                move.l      #$0A000000,$ffff8800.w  ; ch C volume

                IFEQ    BUILD_BIN
                IFEQ    HALF_MEG
.midioff        cmpi.b      #4,49(a0)               ;test if midi on ste dma channels
                bne.s       .end
                cmpi.l      #"EDIT",(a1)            ;if in editor and midi output in dma channels
                bne.s       .end
                bsr         midiallnoteoff
                ENDC
                ENDC

.end            movem.l     (sp)+,d0/a0-a2
                rts

;..........................................................................................
save_ym:        movem.l      d0/a0-a1,-(sp)
                lea          $ffff8800.w,a0
                lea          ymsavebuffer(pc),a1
                moveq        #13,d0
.ymtop          move.b       d0,(a0)                ;select port
                move.b       (a0),(a1)+
                dbra         d0,.ymtop
                movem.l      (sp)+,d0/a0-a1
                rts

restore_ym:     movem.l      d0/a0-a1,-(sp)
                lea          $ffff8800.w,a0
                lea          ymsavebuffer(pc),a1
                moveq        #13,d0
.ymtop          move.b       d0,(a0)                ;select port
                move.b       (a1)+,2(a0)
                dbra         d0,.ymtop
                movem.l      (sp)+,d0/a0-a1
                rts

ymsavebuffer    ds.b         14

;..........................................................................................
                IFNE    TIMER_A+TIMER_B+TIMER_D
restore_ints:   movem.l     d0/a0-a1,-(sp)
                lea         dummyinterrupt(pc),a0
                lea         restore_ints(pc),a1
                add.l       #interruptbackup-restore_ints,a1
                cmpi.w      #$4E73,(a1)
                bne.s       .end
.restoreinsts   move.l      a1,d0
                sub.l       a0,d0
                asr.w       #1,d0
                subq.w      #1,d0
.restoreintstop move.w      (a1)+,(a0)+
                dbra        d0,.restoreintstop
.end            movem.l     (sp)+,d0/a0-a1
                rts
                ENDC

                IFEQ    TIMER_A+TIMER_B+TIMER_D
restore_ints:   rts
                ENDC

                IFNE    TIMER_A+TIMER_B+TIMER_D
cpu_dep_reloc:  movem.l     d0-d1/a0-a3,-(sp)                   ;machine dependant setup

                lea         dummyinterrupt(pc),a0
                lea         cpu_dep_reloc(pc),a1
                add.l       #interruptbackup-cpu_dep_reloc,a1
                cmpi.w      #$4E73,(a1)
                beq.s       .skipbackupints
.backupints     move.l      a1,d0
                sub.l       a0,d0
                asr.w       #1,d0
                subq.w      #1,d0
.backupintstop  move.w      (a0)+,(a1)+
                dbra        d0,.backupintstop

.skipbackupints
                IFNE    INCLUDE_020
                bsr         get_cpu_type
                cmpi.l      #20,d0
                blo         .mc68000

.mc68020plus
                IFNE    DIGIDRUMS_A
                lea         digiinterruptA(pc),a0               ;setup pointers to system friendly digidrum routines
                lea         digiinterA_020(pc),a1
                lea         digiinterA_SMC+8(pc),a2
                move.l      a1,(a0)                             ;pointer
                move.l      a2,20(a1)                           ;relocation
                ENDC
                IFNE    DIGIDRUMS_B
                lea         digiinterruptB(pc),a0
                lea         digiinterB_020(pc),a1
                lea         digiinterB_SMC+8(pc),a2
                move.l      a1,(a0)                             ;pointer
                move.l      a2,20(a1)                           ;relocation
                ENDC
                IFNE    DIGIDRUMS_D
                lea         .mc68020plus(pc),a0
                add.l       #digiinterruptD-.mc68020plus,a0
                lea         .mc68020plus(pc),a1
                add.l       #digiinterD_020-.mc68020plus,a1
                lea         .mc68020plus(pc),a2
                add.l       #digiinterD_SMC+8-.mc68020plus,a2
                move.l      a1,(a0)                             ;pointer
                move.l      a2,20(a1)                           ;relocation
                ENDC
                IFNE    DIGIDRUMS_A
                lea         digiinterrvolA(pc),a0               ;setup pointers to system friendly digidrum routines
                lea         digiinvolA_020(pc),a1
                lea         digiinvolA_SMC+12(pc),a2
                move.l      a1,(a0)                             ;pointer
                move.l      a2,32(a1)                           ;relocation
                ENDC
                IFNE    DIGIDRUMS_B
                lea         digiinterrvolB(pc),a0
                lea         digiinvolB_020(pc),a1
                lea         digiinvolB_SMC+12(pc),a2
                move.l      a1,(a0)                             ;pointer
                move.l      a2,32(a1)                           ;relocation
                ENDC
                IFNE    DIGIDRUMS_D
                lea         .mc68020plus(pc),a0
                add.l       #digiinterrvolD-.mc68020plus,a0
                lea         .mc68020plus(pc),a1
                add.l       #digiinvolD_020-.mc68020plus,a1
                lea         .mc68020plus(pc),a2
                add.l       #digiinvolD_SMC+12-.mc68020plus,a2
                move.l      a1,(a0)                             ;pointer
                move.l      a2,32(a1)                           ;relocation
                ENDC


                IFNE    SID_A
                lea         sidintersA_SMC(pc),a0
                move.l      a0,d0
                move.l      a0,d1
                andi.w      #%1111111000000000,d0
                sub.l       d0,d1
                move.l      d0,a3
                lea         sidinterruptsA(pc),a0               ;setup pointers to system friendly SIDA
                tst.l       (a0)
                bne.s       .sidAend020
                lea         sidinterA1_020(pc),a1
                lea         timlengthA_020(pc),a2
                moveq       #SID_STEPS-1,d0
.sidAtop020     move.l      a1,(a0)+
                sub.w       d1,2(a1)                            ;account for later shifting of SMC routines
                move.l      a2,10(a1)                           ;relocation
                lea.l       32(a1),a1
                move.w      #$0800,2(a3)
                lea         16(a3),a3
                dbra        d0,.sidAtop020
.sidAend020
                ENDC
                IFNE    SID_B
                lea         sidintersB_SMC(pc),a0
                move.l      a0,d0
                move.l      a0,d1
                andi.w      #%1111111000000000,d0
                sub.l       d0,d1
                move.l      d0,a3
                lea         sidinterruptsB(pc),a0               ;setup pointers to system friendly SIDA
                tst.l       (a0)
                bne.s       .sidBend020
                lea         sidinterB1_020(pc),a1
                lea         timlengthB_020(pc),a2
                moveq       #SID_STEPS-1,d0
.sidBtop020     move.l      a1,(a0)+
                sub.w       d1,2(a1)                            ;account for later shifting of SMC routines
                move.l      a2,10(a1)                           ;relocation
                lea.l       32(a1),a1
                move.w      #$0900,2(a3)
                lea         16(a3),a3
                dbra        d0,.sidBtop020
.sidBend020
                ENDC
                IFNE    SID_D
                lea         .sidDtop020(pc),a0
                add.l       #sidintersD_SMC-.sidDtop020,a0
                move.l      a0,d0
                move.l      a0,d1
                andi.w      #%1111111000000000,d0
                sub.l       d0,d1
                move.l      d0,a3
                lea         .sidDtop020(pc),a0                  ;setup pointers to system friendly SIDC
                add.l       #sidinterruptsD-.sidDtop020,a0
                tst.l       (a0)
                bne.s       .sidDend020
                lea         .sidDtop020(pc),a1
                add.l       #sidinterD1_020-.sidDtop020,a1
                lea         .sidDtop020(pc),a2
                add.l       #timlengthD_020-.sidDtop020,a2
                moveq       #SID_STEPS-1,d0
.sidDtop020     move.l      a1,(a0)+
                sub.w       d1,2(a1)                            ;account for later shifting of SMC routines
                move.l      a2,10(a1)                           ;relocation
                lea.l       32(a1),a1
                move.w      #$0a00,2(a3)
                lea         16(a3),a3
                dbra        d0,.sidDtop020
.sidDend020
                ENDC


                IFNE    SYNCSQUARE_A
                lea         synsqintA_SMC(pc),a0
                move.l      a0,d0
                move.l      a0,d1
                clr.b       d0                                  ;d0 holds location of relocated SS SMC routine
                sub.l       d0,d1                               ;d1 holds difference between relocated SS SMC routine and original routine
                move.l      d0,a3                               ;a3 holds location of relocated SS SMC routine
                lea         synsqinteruptA(pc),a0               ;setup pointers to system friendly SS A
                tst.l       (a0)                                ;already set?
                bne.s       .ssAend020
                lea         ssintA1_020(pc),a1
                lea         timlengthA_020(pc),a2
                moveq       #SYNSQU_STEPS-1,d0
.ssAtop020      move.l      a1,(a0)+
                sub.w       d1,32+2(a1)                         ;account for later shifting of SMC routines
                sub.w       d1,32+8(a1)                         ;account for later shifting of SMC routines
                move.l      a2,32+16(a1)                        ;relocation
                lea.l       32+38(a1),a1
                move.l      #$00000000,12(a3)
                move.l      #$0000DEAD,22(a3)
                move.l      #$0100DEAD,30(a3)
                lea         44(a3),a3
                dbra        d0,.ssAtop020
.ssAend020
                ENDC
                IFNE    SYNCSQUARE_B
                lea         synsqintB_SMC(pc),a0
                move.l      a0,d0
                move.l      a0,d1
                clr.b       d0                                  ;d0 holds location of relocated SS SMC routine
                sub.l       d0,d1                               ;d1 holds difference between relocated SS SMC routine and original routine
                move.l      d0,a3                               ;a3 holds location of relocated SS SMC routine
                lea         synsqinteruptB(pc),a0               ;setup pointers to system friendly SS B
                tst.l       (a0)                                ;already set?
                bne.s       .ssBend020
                lea         ssintB1_020(pc),a1
                lea         timlengthB_020(pc),a2
                moveq       #SYNSQU_STEPS-1,d0
.ssBtop020      move.l      a1,(a0)+
                sub.w       d1,32+2(a1)                         ;account for later shifting of SMC routines
                sub.w       d1,32+8(a1)                         ;account for later shifting of SMC routines
                move.l      a2,32+16(a1)                        ;relocation
                lea.l       32+38(a1),a1
                move.l      #$02000000,12(a3)
                move.l      #$0200DEAD,22(a3)
                move.l      #$0300DEAD,30(a3)
                lea         44(a3),a3
                dbra        d0,.ssBtop020
.ssBend020
                ENDC
                IFNE    SYNCSQUARE_D
                lea         .ssDtop020(pc),a0
                add.l       #synsqintD_SMC-.ssDtop020,a0
                move.l      a0,d0
                move.l      a0,d1
                clr.b       d0                                  ;d0 holds location of relocated SS SMC routine
                sub.l       d0,d1                               ;d1 holds difference between relocated SS SMC routine and original routine
                move.l      d0,a3                               ;a3 holds location of relocated SS SMC routine
                lea         .ssDtop020(pc),a0                   ;setup pointers to system friendly SS D
                add.l       #synsqinteruptD-.ssDtop020,a0
                tst.l       (a0)                                ;already set?
                bne.s       .ssDend020
                lea         .ssDtop020(pc),a1
                add.l       #ssintD1_020-.ssDtop020,a1
                lea         .ssDtop020(pc),a2
                add.l       #timlengthD_020-.ssDtop020,a2
                moveq       #SYNSQU_STEPS-1,d0
.ssDtop020      move.l      a1,(a0)+
                sub.w       d1,32+2(a1)                         ;account for later shifting of SMC routines
                sub.w       d1,32+8(a1)                         ;account for later shifting of SMC routines
                move.l      a2,32+16(a1)                        ;relocation
                lea.l       32+38(a1),a1
                move.l      #$04000000,12(a3)
                move.l      #$0400DEAD,22(a3)
                move.l      #$0500DEAD,30(a3)
                lea         44(a3),a3
                dbra        d0,.ssDtop020
.ssDend020
                ENDC


                IFNE    SYNCBUZZER_A
                lea         synbuzintA_SMC(pc),a0
                move.l      a0,d0
                move.l      a0,d1
                clr.b       d0
                sub.l       d0,d1
                move.l      d0,a3
                lea         synbuzinteruptA(pc),a0              ;setup pointers to system friendly sync buzz A
                tst.l       (a0)
                bne.s       .synbuzAend020
                lea         synbuzintA1_020(pc),a1
                lea         timlengthA_020(pc),a2
                moveq       #SYNCBUZZ_STEPS-1,d0
.synbuzAtop020  move.l      a1,(a0)+
                sub.w       d1,2(a1)                            ;account for later shifting of SMC routines
                move.l      a2,10(a1)                           ;relocation
                lea.l       32(a1),a1
                move.l      #$0D00D0AD,2(a3)
                lea         16(a3),a3
                dbra        d0,.synbuzAtop020
.synbuzAend020
                ENDC

                IFNE    SYNCBUZZER_B
                lea         synbuzintB_SMC(pc),a0
                move.l      a0,d0
                move.l      a0,d1
                clr.b       d0
                sub.l       d0,d1
                move.l      d0,a3
                lea         synbuzinteruptB(pc),a0              ;setup pointers to system friendly sync buzz B
                tst.l       (a0)
                bne.s       .synbuzBend020
                lea         synbuzintB1_020(pc),a1
                lea         timlengthB_020(pc),a2
                moveq       #SYNCBUZZ_STEPS-1,d0
.synbuzBtop020  move.l      a1,(a0)+
                sub.w       d1,2(a1)                            ;account for later shifting of SMC routines
                move.l      a2,10(a1)                           ;relocation
                lea.l       32(a1),a1
                move.l      #$0D00D0AD,2(a3)
                lea         16(a3),a3
                dbra        d0,.synbuzBtop020
.synbuzBend020
                ENDC

                IFNE    SYNCBUZZER_D
                lea         .synbuzDtop020(pc),a0
                add.l       #synbuzintD_SMC-.synbuzDtop020,a0
                move.l      a0,d0
                move.l      a0,d1
                clr.b       d0
                sub.l       d0,d1
                move.l      d0,a3
                lea         .synbuzDtop020(pc),a0               ;setup pointers to system friendly sync buzz D
                add.l       #synbuzinteruptD-.synbuzDtop020,a0
                tst.l       (a0)
                bne.s       .synbuzDend020
                lea         .synbuzDtop020(pc),a1
                add.l       #synbuzintD1_020-.synbuzDtop020,a1
                lea         .synbuzDtop020(pc),a2
                add.l       #timlengthD_020-.synbuzDtop020,a2
                moveq       #SYNCBUZZ_STEPS-1,d0
.synbuzDtop020  move.l      a1,(a0)+
                sub.w       d1,2(a1)                            ;account for later shifting of SMC routines
                move.l      a2,10(a1)                           ;relocation
                lea.l       32(a1),a1
                move.l      #$0D00D0AD,2(a3)
                lea         16(a3),a3
                dbra        d0,.synbuzDtop020
.synbuzDend020
                ENDC


                IFNE    FM_A
                lea         fmintA_SMC(pc),a0
                move.l      a0,d0
                move.l      a0,d1
                clr.b       d0                                  ;d0 holds location of relocated FM SMC routine
                sub.l       d0,d1                               ;d1 holds difference between relocated FM SMC routine and original routine
                move.l      d0,a3                               ;a3 holds location of relocated FM SMC routine
                lea         fminteruptA(pc),a0                  ;setup pointers to system friendly FM A
                tst.l       (a0)                                ;already set?
                bne.s       .fmAend020
                lea         fmintA1_020(pc),a1
                lea         timlengthA_020(pc),a2
                moveq       #FM_STEPS-1,d0
.fmAtop020      move.l      a1,(a0)+
                sub.w       d1,2(a1)                            ;account for later shifting of SMC routines
                sub.w       d1,8(a1)                            ;account for later shifting of SMC routines
                move.l      a2,16(a1)                           ;relocation
                lea.l       38(a1),a1
                move.l      #$0000DEAD,2(a3)
                move.l      #$0100DEAD,10(a3)
                lea         24(a3),a3
                dbra        d0,.fmAtop020
.fmAend020
                ENDC

                IFNE    FM_B
                lea         fmintB_SMC(pc),a0
                move.l      a0,d0
                move.l      a0,d1
                clr.b       d0
                sub.l       d0,d1
                move.l      d0,a3
                lea         fminteruptB(pc),a0                  ;setup pointers to system friendly FM B
                tst.l       (a0)
                bne.s       .fmBend020
                lea         fmintB1_020(pc),a1
                lea         timlengthB_020(pc),a2
                moveq       #FM_STEPS-1,d0
.fmBtop020      move.l      a1,(a0)+
                sub.w       d1,2(a1)                            ;account for later shifting of SMC routines
                sub.w       d1,8(a1)                            ;account for later shifting of SMC routines
                move.l      a2,16(a1)                           ;relocation
                lea.l       38(a1),a1
                move.l      #$0200DEAD,2(a3)
                move.l      #$0300DEAD,10(a3)
                lea         24(a3),a3
                dbra        d0,.fmBtop020
.fmBend020
                ENDC

                IFNE    FM_D
                lea         .fmDtop020(pc),a0
                add.l       #fmintD_SMC-.fmDtop020,a0
                move.l      a0,d0
                move.l      a0,d1
                clr.b       d0
                sub.l       d0,d1
                move.l      d0,a3
                lea         .fmDtop020(pc),a0
                add.l       #fminteruptD-.fmDtop020,a0          ;setup pointers to system friendly FM D
                tst.l       (a0)
                bne.s       .fmDend020
                lea         .fmDtop020(pc),a1
                add.l       #fmintD1_020-.fmDtop020,a1
                lea         .fmDtop020(pc),a2
                add.l       #timlengthD_020-.fmDtop020,a2
                moveq       #FM_STEPS-1,d0
.fmDtop020      move.l      a1,(a0)+
                sub.w       d1,2(a1)                            ;account for later shifting of SMC routines
                sub.w       d1,8(a1)                            ;account for later shifting of SMC routines
                move.l      a2,16(a1)                           ;relocation
                lea.l       38(a1),a1
                move.l      #$0400DEAD,2(a3)
                move.l      #$0500DEAD,10(a3)
                lea         24(a3),a3
                dbra        d0,.fmDtop020
.fmDend020
                ENDC


                IFNE    FMB_A
                lea         fmbintA_SMC(pc),a0
                move.l      a0,d0
                move.l      a0,d1
                clr.b       d0                                  ;d0 holds location of relocated FMB SMC routine
                sub.l       d0,d1                               ;d1 holds difference between relocated FMB SMC routine and original routine
                move.l      d0,a3                               ;a3 holds location of relocated FMB SMC routine
                lea         fmbinteruptA(pc),a0                 ;setup pointers to system friendly FMB A
                tst.l       (a0)                                ;already set?
                bne.s       .fmbAend020
                lea         fmbintA1_020(pc),a1
                lea         timlengthA_020(pc),a2
                moveq       #FMB_STEPS-1,d0
.fmbAtop020     move.l      a1,(a0)+
                sub.w       d1,2(a1)                            ;account for later shifting of SMC routines
                sub.w       d1,8(a1)                            ;account for later shifting of SMC routines
                move.l      a2,16(a1)                           ;relocation
                lea.l       38(a1),a1
                move.l      #$0B00DEAD,2(a3)
                move.l      #$0C00DEAD,10(a3)
                lea         24(a3),a3
                dbra        d0,.fmbAtop020
.fmbAend020
                ENDC

                IFNE    FMB_B
                lea         fmbintB_SMC(pc),a0
                move.l      a0,d0
                move.l      a0,d1
                clr.b       d0
                sub.l       d0,d1
                move.l      d0,a3
                lea         fmbinteruptB(pc),a0                 ;setup pointers to system friendly FMB B
                tst.l       (a0)
                bne.s       .fmbBend020
                lea         fmbintB1_020(pc),a1
                lea         timlengthB_020(pc),a2
                moveq       #FMB_STEPS-1,d0
.fmbBtop020     move.l      a1,(a0)+
                sub.w       d1,2(a1)                            ;account for later shifting of SMC routines
                sub.w       d1,8(a1)                            ;account for later shifting of SMC routines
                move.l      a2,16(a1)                           ;relocation
                lea.l       38(a1),a1
                move.l      #$0B00DEAD,2(a3)
                move.l      #$0C00DEAD,10(a3)
                lea         24(a3),a3
                dbra        d0,.fmbBtop020
.fmbBend020
                ENDC

                IFNE    FMB_D
                lea         .fmbDtop020(pc),a0
                add.l       #fmbintD_SMC-.fmbDtop020,a0
                move.l      a0,d0
                move.l      a0,d1
                clr.b       d0
                sub.l       d0,d1
                move.l      d0,a3
                lea         .fmbDtop020(pc),a0                  ;setup pointers to system friendly FMB D
                add.l       #fmbinteruptD-.fmbDtop020,a0
                tst.l       (a0)
                bne.s       .fmbDend020
                lea         .fmbDtop020(pc),a1
                add.l       #fmbintD1_020-.fmbDtop020,a1
                lea         .fmbDtop020(pc),a2
                add.l       #timlengthD_020-.fmbDtop020,a2
                moveq       #FMB_STEPS-1,d0
.fmbDtop020     move.l      a1,(a0)+
                sub.w       d1,2(a1)                            ;account for later shifting of SMC routines
                sub.w       d1,8(a1)                            ;account for later shifting of SMC routines
                move.l      a2,16(a1)                           ;relocation
                lea.l       38(a1),a1
                move.l      #$0B00DEAD,2(a3)
                move.l      #$0C00DEAD,10(a3)
                lea         24(a3),a3
                dbra        d0,.fmbDtop020
.fmbDend020
                ENDC


                IFNE    FMSB_A
                lea         fmsbintA_SMC(pc),a0
                move.l      a0,d0
                move.l      a0,d1
                clr.b       d0                                  ;d0 holds location of relocated FMSB SMC routine
                sub.l       d0,d1                               ;d1 holds difference between relocated FMSB SMC routine and original routine
                move.l      d0,a3                               ;a3 holds location of relocated FMSB SMC routine
                lea         fmsbinteruptA(pc),a0                ;setup pointers to system friendly FMSB A
                tst.l       (a0)                                ;already set?
                bne.s       .fmsbAend020
                lea         fmsbintA1_020(pc),a1
                lea         timlengthA_020(pc),a2
                moveq       #FMSB_STEPS-1,d0
.fmsbAtop020    move.l      a1,(a0)+
                sub.w       d1,2(a1)                            ;account for later shifting of SMC routines
                sub.w       d1,8(a1)                            ;account for later shifting of SMC routines
                sub.w       d1,14(a1)                           ;account for later shifting of SMC routines
                move.l      a2,22(a1)                           ;relocation
                lea.l       44(a1),a1
                move.l      #$0D00D0AD,2(a3)
                move.l      #$0B00DEAD,10(a3)
                move.l      #$0C00DEAD,18(a3)
                lea         32(a3),a3
                dbra        d0,.fmsbAtop020
.fmsbAend020
                ENDC

                IFNE    FMSB_B
                lea         fmsbintB_SMC(pc),a0
                move.l      a0,d0
                move.l      a0,d1
                clr.b       d0
                sub.l       d0,d1
                move.l      d0,a3
                lea         fmsbinteruptB(pc),a0                ;setup pointers to system friendly FMSB B
                tst.l       (a0)
                bne.s       .fmsbBend020
                lea         fmsbintB1_020(pc),a1
                lea         timlengthB_020(pc),a2
                moveq       #FMSB_STEPS-1,d0
.fmsbBtop020    move.l      a1,(a0)+
                sub.w       d1,2(a1)                            ;account for later shifting of SMC routines
                sub.w       d1,8(a1)                            ;account for later shifting of SMC routines
                sub.w       d1,14(a1)                           ;account for later shifting of SMC routines
                move.l      a2,22(a1)                           ;relocation
                lea.l       44(a1),a1
                move.l      #$0D00D0AD,2(a3)
                move.l      #$0B00DEAD,10(a3)
                move.l      #$0C00DEAD,18(a3)
                lea         32(a3),a3
                dbra        d0,.fmsbBtop020
.fmsbBend020
                ENDC

                IFNE    FMSB_D
                lea         .fmsbDtop020(pc),a0
                add.l       #fmsbintD_SMC-.fmsbDtop020,a0
                move.l      a0,d0
                move.l      a0,d1
                clr.b       d0
                sub.l       d0,d1
                move.l      d0,a3
                lea         .fmsbDtop020(pc),a0                 ;setup pointers to system friendly FMSB D
                add.l       #fmsbinteruptD-.fmsbDtop020,a0
                tst.l       (a0)
                bne.s       .fmsbDend020
                lea         .fmsbDtop020(pc),a1
                add.l       #fmsbintD1_020-.fmsbDtop020,a1
                lea         .fmsbDtop020(pc),a2
                add.l       #timlengthD_020-.fmsbDtop020,a2
                moveq       #FMSB_STEPS-1,d0
.fmsbDtop020    move.l      a1,(a0)+
                sub.w       d1,2(a1)                            ;account for later shifting of SMC routines
                sub.w       d1,8(a1)                            ;account for later shifting of SMC routines
                sub.w       d1,14(a1)                           ;account for later shifting of SMC routines
                move.l      a2,22(a1)                           ;relocation
                lea.l       44(a1),a1
                move.l      #$0D00D0AD,2(a3)
                move.l      #$0B00DEAD,10(a3)
                move.l      #$0C00DEAD,18(a3)
                lea         32(a3),a3
                dbra        d0,.fmsbDtop020
.fmsbDend020
                ENDC

                IFNE    PWM_A
                lea         pwminterA_SMC1(pc),a0
                move.l      a0,d0
                move.l      a0,d1
                clr.b       d0                                  ;d0 holds location of relocated PWM SMC routine
                sub.l       d0,d1                               ;d1 holds difference between relocated PWM SMC routine and original routine
                move.l      d0,a3                               ;a3 holds location of relocated PWM SMC routine
                lea         pwminterruptsA(pc),a0               ;setup pointers to system friendly PWM A
                tst.l       (a0)                                ;already set?
                bne.s       .pwmAend020
                lea         pwminterA_020_1(pc),a1
                moveq       #2-1,d0
.pwmAtop020     move.l      a1,(a0)+
                sub.w       d1,6(a1)                            ;account for later shifting of SMC routines
                sub.w       d1,12(a1)                           ;account for later shifting of SMC routines
                sub.w       d1,18(a1)                           ;account for later shifting of SMC routines
                add.l       #pwminterA_020_2-pwminterA_020_1,a1
                move.l      #$0800DEAD,20(a3)
                add.l       #pwminterA_SMC2-pwminterA_SMC1,a3
                dbra        d0,.pwmAtop020
.pwmAend020
                ENDC
                IFNE    PWM_B
                lea         .pwmBtop020(pc),a0
                add.l       #pwminterB_SMC1-.pwmBtop020,a0
                move.l      a0,d0
                move.l      a0,d1
                clr.b       d0                                  ;d0 holds location of relocated PWM SMC routine
                sub.l       d0,d1                               ;d1 holds difference between relocated PWM SMC routine and original routine
                move.l      d0,a3                               ;a3 holds location of relocated PWM SMC routine
                lea         pwminterruptsB(pc),a0               ;setup pointers to system friendly PWM B
                tst.l       (a0)                                ;already set?
                bne.s       .pwmBend020
                lea         pwminterB_020_1(pc),a1
                moveq       #2-1,d0
.pwmBtop020     move.l      a1,(a0)+
                sub.w       d1,6(a1)                            ;account for later shifting of SMC routines
                sub.w       d1,12(a1)                           ;account for later shifting of SMC routines
                sub.w       d1,18(a1)                           ;account for later shifting of SMC routines
                add.l       #pwminterB_020_2-pwminterB_020_1,a1
                move.l      #$0900DEAD,20(a3)
                add.l       #pwminterB_SMC2-pwminterB_SMC1,a3
                dbra        d0,.pwmBtop020
.pwmBend020
                ENDC
                IFNE    PWM_D
                lea         .pwmDtop020(pc),a0
                add.l       #pwminterD_SMC1-.pwmDtop020,a0
                move.l      a0,d0
                move.l      a0,d1
                clr.b       d0                                  ;d0 holds location of relocated PWM SMC routine
                sub.l       d0,d1                               ;d1 holds difference between relocated PWM SMC routine and original routine
                move.l      d0,a3                               ;a3 holds location of relocated PWM SMC routine
                lea         .pwmDtop020(pc),a0                  ;setup pointers to system friendly PWM D
                add.l       #pwminterruptsD-.pwmDtop020,a0
                tst.l       (a0)                                ;already set?
                bne.s       .pwmDend020
                lea         .pwmDtop020(pc),a1
                add.l       #pwminterD_020_1-.pwmDtop020,a1
                moveq       #2-1,d0
.pwmDtop020     move.l      a1,(a0)+
                sub.w       d1,10(a1)                           ;account for later shifting of SMC routines
                sub.w       d1,16(a1)                           ;account for later shifting of SMC routines
                sub.w       d1,24(a1)                           ;account for later shifting of SMC routines
                add.l       #pwminterD_020_2-pwminterD_020_1,a1
                move.l      #$0A00DEAD,20(a3)
                add.l       #pwminterD_SMC2-pwminterD_SMC1,a3
                dbra        d0,.pwmDtop020
.pwmDend020
                ENDC
                lea         pulsewidth_tune(pc),a0
                move.w      #PULWID_TUNE_020,(a0)

                bsr         flush_cache
                movem.l     (sp)+,d0-d1/a0-a3
                rts
                ENDC


.mc68000
                IFNE    DIGIDRUMS_A
                lea         digiinterruptA(pc),a0               ;setup pointers to SMC digidrum routines
                lea         digiinterA_SMC(pc),a1
                move.l      a1,(a0)
                ENDC
                IFNE    DIGIDRUMS_B
                lea         digiinterruptB(pc),a0
                lea         digiinterB_SMC(pc),a1
                move.l      a1,(a0)
                ENDC
                IFNE    DIGIDRUMS_D
                lea         .mc68000(pc),a0
                add.l       #digiinterruptD-.mc68000,a0
                lea         .mc68000(pc),a1
                add.l       #digiinterD_SMC-.mc68000,a1
                move.l      a1,(a0)
                ENDC
                IFNE    DIGIDRUMS_A
                lea         digiinterrvolA(pc),a0               ;setup pointers to SMC digidrum routines with volume
                lea         digiinvolA_SMC(pc),a1
                move.l      a1,(a0)
                ENDC
                IFNE    DIGIDRUMS_B
                lea         digiinterrvolB(pc),a0
                lea         digiinvolB_SMC(pc),a1
                move.l      a1,(a0)
                ENDC
                IFNE    DIGIDRUMS_D
                lea         .mc68000(pc),a0
                add.l       #digiinterrvolD-.mc68000,a0
                lea         .mc68000(pc),a1
                add.l       #digiinvolD_SMC-.mc68000,a1
                move.l      a1,(a0)
                ENDC


                IFNE    DIGIDRUMS_A
                lea         digiinterA_SMC+8(pc),a0
                move.l      a0,10(a0)                           ;relocate SMC digidrum routine A
                ENDC
                IFNE    DIGIDRUMS_B
                lea         digiinterB_SMC+8(pc),a0
                move.l      a0,10(a0)                           ;relocate SMC digidrum routine B
                ENDC
                IFNE    DIGIDRUMS_D
                lea         .mc68000(pc),a0
                add.l       #digiinterD_SMC+8-.mc68000,a0
                move.l      a0,10(a0)                           ;relocate SMC digidrum routine D
                ENDC
                IFNE    DIGIDRUMS_A
                lea         digiinvolA_SMC+12(pc),a0
                move.l      a0,30-12(a0)                        ;relocate SMC digidrum routine A
                ENDC
                IFNE    DIGIDRUMS_B
                lea         digiinvolB_SMC+12(pc),a0
                move.l      a0,30-12(a0)                        ;relocate SMC digidrum routine B
                ENDC
                IFNE    DIGIDRUMS_D
                lea         .mc68000(pc),a0
                add.l       #digiinvolD_SMC+12-.mc68000,a0
                move.l      a0,30-12(a0)                        ;relocate SMC digidrum routine D
                ENDC

                IFNE    SID_A
                lea         sidintersA_SMC(pc),a0
                lea         sidintersA_SMCend(pc),a1
                move.l      a0,d0
                andi.w      #%1111111000000000,d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skipsidAcopy
.copysidAtop    move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copysidAtop
.skipsidAcopy   lea         sidinterruptsA(pc),a0               ;setup pointers to SMC SID routines
                move.l      d0,a1
                moveq       #SID_STEPS-1,d0
.sidAtop        move.l      a1,(a0)+
                lea.l       16(a1),a1                           ;length of SID routine
                dbra        d0,.sidAtop
                ENDC

                IFNE    SID_B
                lea         sidintersB_SMC(pc),a0
                lea         sidintersB_SMCend(pc),a1
                move.l      a0,d0
                andi.w      #%1111111000000000,d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skipsidBcopy
.copysidBtop    move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copysidBtop
.skipsidBcopy   lea         sidinterruptsB(pc),a0               ;setup pointers to SMC SID routines
                move.l      d0,a1
                moveq       #SID_STEPS-1,d0
.sidBtop        move.l      a1,(a0)+
                lea.l       16(a1),a1                           ;length of SID routine
                dbra        d0,.sidBtop
                ENDC

                IFNE    SID_D
                lea         .copysidDtop(pc),a0
                add.l       #sidintersD_SMC-.copysidDtop,a0
                lea         .copysidDtop(pc),a1
                add.l       #sidintersD_SMCend-.copysidDtop,a1
                move.l      a0,d0
                andi.w      #%1111111000000000,d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skipsidDcopy
.copysidDtop    move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copysidDtop
.skipsidDcopy   lea         .skipsidDcopy(pc),a0               ;setup pointers to SMC SID routines
                add.l       #sidinterruptsD-.skipsidDcopy,a0
                move.l      d0,a1
                moveq       #SID_STEPS-1,d0
.sidDtop        move.l      a1,(a0)+
                lea.l       16(a1),a1                           ;length of SID routine
                dbra        d0,.sidDtop
                ENDC


                IFNE    SYNCSQUARE_A
                lea         synsqintA_SMC(pc),a0
                lea         synsqintA_SMCend(pc),a1
                move.l      a0,d0
                clr.b       d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skipssqAcopy
.copyssqAtop    move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copyssqAtop
.skipssqAcopy   lea         synsqinteruptA(pc),a0               ;setup pointers to sync square routines
                move.l      d0,a1
                moveq       #SYNSQU_STEPS-1,d0
.syncsquAtop    move.l      a1,(a0)+
                move.w      a1,38(a1)                           ;prevent crashing
                lea.l       44(a1),a1                           ;length of sync square routine
                dbra        d0,.syncsquAtop
                ENDC
                IFNE    SYNCSQUARE_B
                lea         synsqintB_SMC(pc),a0
                lea         synsqintB_SMCend(pc),a1
                move.l      a0,d0
                clr.b       d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skipssqBcopy
.copyssqBtop    move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copyssqBtop
.skipssqBcopy   lea         synsqinteruptB(pc),a0               ;setup pointers to sync square routines
                move.l      d0,a1
                moveq       #SYNSQU_STEPS-1,d0
.syncsquBtop    move.l      a1,(a0)+
                move.w      a1,38(a1)                           ;prevent crashing
                lea.l       44(a1),a1                           ;length of sync square routine
                dbra        d0,.syncsquBtop
                ENDC
                IFNE    SYNCSQUARE_D
                lea         .copyssqDtop(pc),a0
                add.l       #synsqintD_SMC-.copyssqDtop,a0
                lea         .copyssqDtop(pc),a1
                add.l       #synsqintD_SMCend-.copyssqDtop,a1
                move.l      a0,d0
                clr.b       d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skipssqDcopy
.copyssqDtop    move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copyssqDtop
.skipssqDcopy   lea         .skipssqDcopy(pc),a0               ;setup pointers to sync square routines
                add.l       #synsqinteruptD-.skipssqDcopy,a0
                move.l      d0,a1
                moveq       #SYNSQU_STEPS-1,d0
.syncsquDtop    move.l      a1,(a0)+
                move.w      a1,38(a1)                           ;prevent crashing
                lea.l       44(a1),a1                           ;length of sync square routine
                dbra        d0,.syncsquDtop
                ENDC


                IFNE    SYNCBUZZER_A
                lea         synbuzintA_SMC(pc),a0
                lea         synbuzintA_SMCend(pc),a1
                move.l      a0,d0
                clr.b       d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skipsbuAcopy
.copysbuAtop    move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copysbuAtop
.skipsbuAcopy   lea         synbuzinteruptA(pc),a0              ;setup pointers to syncbuzzer routines
                move.l      d0,a1
                moveq       #SYNCBUZZ_STEPS-1,d0
.syncbuzAtop    move.l      a1,(a0)+
                move.w      a1,10(a1)                           ;prevent crashing
                lea.l       16(a1),a1                           ;length of syncbuzzer routine
                dbra        d0,.syncbuzAtop
                ENDC

                IFNE    SYNCBUZZER_B
                lea         synbuzintB_SMC(pc),a0
                lea         synbuzintB_SMCend(pc),a1
                move.l      a0,d0
                clr.b       d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skipsbuBcopy
.copysbuBtop    move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copysbuBtop
.skipsbuBcopy   lea         synbuzinteruptB(pc),a0              ;setup pointers to syncbuzzer routines
                move.l      d0,a1
                moveq       #SYNCBUZZ_STEPS-1,d0
.syncbuzBtop    move.l      a1,(a0)+
                move.w      a1,10(a1)                           ;prevent crashing
                lea.l       16(a1),a1                           ;length of syncbuzzer routine
                dbra        d0,.syncbuzBtop
                ENDC

                IFNE    SYNCBUZZER_D
                lea         .copysbuDtop(pc),a0
                add.l       #synbuzintD_SMC-.copysbuDtop,a0
                lea         .copysbuDtop(pc),a1
                add.l       #synbuzintD_SMCend-.copysbuDtop,a1
                move.l      a0,d0
                clr.b       d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skipsbuDcopy
.copysbuDtop    move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copysbuDtop
.skipsbuDcopy   lea         synbuzinteruptD(pc),a0              ;setup pointers to syncbuzzer routines
                move.l      d0,a1
                moveq       #SYNCBUZZ_STEPS-1,d0
.syncbuzDtop    move.l      a1,(a0)+
                move.w      a1,10(a1)                           ;prevent crashing
                lea.l       16(a1),a1                           ;length of syncbuzzer routine
                dbra        d0,.syncbuzDtop
                ENDC


                IFNE    FM_A
                lea         fmintA_SMC(pc),a0
                lea         fmintA_SMCend(pc),a1
                move.l      a0,d0
                clr.b       d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skipfmAcopy
.copyfmAtop     move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copyfmAtop
.skipfmAcopy    lea         fminteruptA(pc),a0                  ;setup pointers to FM routines
                move.l      d0,a1
                moveq       #FM_STEPS-1,d0
.fmAtop         move.l      a1,(a0)+
                move.w      a1,18(a1)                           ;prevent crashing
                lea.l       24(a1),a1                           ;length of FM routine
                dbra        d0,.fmAtop
                ENDC

                IFNE    FM_B
                lea         fmintB_SMC(pc),a0
                lea         fmintB_SMCend(pc),a1
                move.l      a0,d0
                clr.b       d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skipfmBcopy
.copyfmBtop     move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copyfmBtop
.skipfmBcopy    lea         fminteruptB(pc),a0                  ;setup pointers to FM routines
                move.l      d0,a1
                moveq       #FM_STEPS-1,d0
.fmBtop         move.l      a1,(a0)+
                move.w      a1,18(a1)                           ;prevent crashing
                lea.l       24(a1),a1                           ;length of FM routine
                dbra        d0,.fmBtop
                ENDC

                IFNE    FM_D
                lea         .copyfmDtop(pc),a0
                add.l       #fmintD_SMC-.copyfmDtop,a0
                lea         .copyfmDtop(pc),a1
                add.l       #fmintD_SMCend-.copyfmDtop,a1
                move.l      a0,d0
                clr.b       d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skipfmDcopy
.copyfmDtop     move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copyfmDtop
.skipfmDcopy    lea         .skipfmDcopy(pc),a0                 ;setup pointers to FM routines
                add.l       #fminteruptD-.skipfmDcopy,a0
                move.l      d0,a1
                moveq       #FM_STEPS-1,d0
.fmDtop         move.l      a1,(a0)+
                move.w      a1,18(a1)                           ;prevent crashing
                lea.l       24(a1),a1                           ;length of FM routine
                dbra        d0,.fmDtop
                ENDC


                IFNE    FMB_A
                lea         fmbintA_SMC(pc),a0
                lea         fmbintA_SMCend(pc),a1
                move.l      a0,d0
                clr.b       d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skipfmbAcopy
.copyfmbAtop    move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copyfmbAtop
.skipfmbAcopy   lea         fmbinteruptA(pc),a0                 ;setup pointers to FMB routines
                move.l      d0,a1
                moveq       #FMB_STEPS-1,d0
.fmbAtop        move.l      a1,(a0)+
                move.w      a1,18(a1)                           ;prevent crashing
                lea.l       24(a1),a1                           ;length of FMB routine
                dbra        d0,.fmbAtop
                ENDC

                IFNE    FMB_B
                lea         fmbintB_SMC(pc),a0
                lea         fmbintB_SMCend(pc),a1
                move.l      a0,d0
                clr.b       d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skipfmbBcopy
.copyfmbBtop    move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copyfmbBtop
.skipfmbBcopy   lea         fmbinteruptB(pc),a0                 ;setup pointers to FMB routines
                move.l      d0,a1
                moveq       #FMB_STEPS-1,d0
.fmbBtop        move.l      a1,(a0)+
                move.w      a1,18(a1)                           ;prevent crashing
                lea.l       24(a1),a1                           ;length of FMB routine
                dbra        d0,.fmbBtop
                ENDC

                IFNE    FMB_D
                lea         .copyfmbDtop(pc),a0
                add.l       #fmbintD_SMC-.copyfmbDtop,a0
                lea         .copyfmbDtop(pc),a1
                add.l       #fmbintD_SMCend-.copyfmbDtop,a1
                move.l      a0,d0
                clr.b       d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skipfmbDcopy
.copyfmbDtop    move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copyfmbDtop
.skipfmbDcopy   lea         .skipfmbDcopy(pc),a0                ;setup pointers to FMB routines
                add.l       #fmbinteruptD-.skipfmbDcopy,a0
                move.l      d0,a1
                moveq       #FMB_STEPS-1,d0
.fmbDtop        move.l      a1,(a0)+
                move.w      a1,18(a1)                           ;prevent crashing
                lea.l       24(a1),a1                           ;length of FMB routine
                dbra        d0,.fmbDtop
                ENDC


                IFNE    FMSB_A
                lea         fmsbintA_SMC(pc),a0
                lea         fmsbintA_SMCend(pc),a1
                move.l      a0,d0
                clr.b       d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skipfmsbAcopy
.copyfmsbAtop   move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copyfmsbAtop
.skipfmsbAcopy  lea         fmsbinteruptA(pc),a0                 ;setup pointers to FMSB routines
                move.l      d0,a1
                moveq       #FMSB_STEPS-1,d0
.fmsbAtop       move.l      a1,(a0)+
                move.w      a1,26(a1)                           ;prevent crashing
                lea.l       32(a1),a1                           ;length of FMSB routine
                dbra        d0,.fmsbAtop
                ENDC

                IFNE    FMSB_B
                lea         fmsbintB_SMC(pc),a0
                lea         fmsbintB_SMCend(pc),a1
                move.l      a0,d0
                clr.b       d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skipfmsbBcopy
.copyfmsbBtop   move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copyfmsbBtop
.skipfmsbBcopy  lea         fmsbinteruptB(pc),a0                ;setup pointers to FMSB routines
                move.l      d0,a1
                moveq       #FMSB_STEPS-1,d0
.fmsbBtop       move.l      a1,(a0)+
                move.w      a1,26(a1)                           ;prevent crashing
                lea.l       32(a1),a1                           ;length of FMSB routine
                dbra        d0,.fmsbBtop
                ENDC

                IFNE    FMSB_D
                lea         .copyfmsbDtop(pc),a0
                add.l       #fmsbintD_SMC-.copyfmsbDtop,a0
                lea         .copyfmsbDtop(pc),a1
                add.l       #fmsbintD_SMCend-.copyfmsbDtop,a1
                move.l      a0,d0
                clr.b       d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skipfmsbDcopy
.copyfmsbDtop   move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copyfmsbDtop
.skipfmsbDcopy  lea         .skipfmsbDcopy(pc),a0               ;setup pointers to FMSB routines
                add.l       #fmsbinteruptD-.skipfmsbDcopy,a0
                move.l      d0,a1
                moveq       #FMSB_STEPS-1,d0
.fmsbDtop       move.l      a1,(a0)+
                move.w      a1,26(a1)                           ;prevent crashing
                lea.l       32(a1),a1                           ;length of FMSB routine
                dbra        d0,.fmsbDtop
                ENDC

                IFNE    PWM_A
                lea         pwminterA_SMC1(pc),a0
                lea         pwminterA_SMCend(pc),a1
                move.l      a0,d0
                clr.b       d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skippwmAcopy
.copypwmAtop    move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copypwmAtop
.skippwmAcopy   lea         pwminterruptsA(pc),a0                       ;setup pointers to SMC PWM routines
                move.l      d0,(a0)+
                addi.l      #pwminterA_SMC2-pwminterA_SMC1,d0
                move.l      d0,(a0)
                move.l      d0,a1
                move.w      2(a0),28-34(a1)                             ; new vector address in SMC1
                move.w      -2(a0),28(a1)                               ; new vector address in SMC2
                ENDC
                IFNE    PWM_B
                lea         pwminterB_SMC1(pc),a0
                lea         pwminterB_SMCend(pc),a1
                move.l      a0,d0
                clr.b       d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skippwmBcopy
.copypwmBtop    move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copypwmBtop
.skippwmBcopy   lea         pwminterruptsB(pc),a0
                move.l      d0,(a0)+
                addi.l      #pwminterB_SMC2-pwminterB_SMC1,d0
                move.l      d0,(a0)
                move.l      d0,a1
                move.w      2(a0),28-34(a1)                             ; new vector address in SMC1
                move.w      -2(a0),28(a1)                               ; new vector address in SMC2
                ENDC
                IFNE    PWM_D
                lea         .copypwmDtop(pc),a0
                add.l       #pwminterD_SMC1-.copypwmDtop,a0
                lea         .copypwmDtop(pc),a1
                add.l       #pwminterD_SMCend-.copypwmDtop,a1
                move.l      a0,d0
                clr.b       d0
                move.l      d0,a2
                tst.w       (a2)
                bne.s       .skippwmDcopy
.copypwmDtop    move.w      (a0)+,(a2)+
                cmpa.l      a0,a1
                bne.s       .copypwmDtop
.skippwmDcopy   lea         .skippwmDcopy(pc),a0
                add.l       #pwminterruptsD-.skippwmDcopy,a0
                move.l      d0,a1
                move.l      a1,(a0)+
                addi.l      #pwminterD_SMC2-pwminterD_SMC1,d0
                move.l      d0,(a0)
                move.l      d0,a1
                move.w      2(a0),28-34(a1)                             ; new vector address in SMC1
                move.w      -2(a0),28(a1)                               ; new vector address in SMC2
                ENDC

                movem.l     (sp)+,d0-d1/a0-a3
                rts
                ENDC

                IFNE    INCLUDE_020
get_cpu_type:   movem.l     d1/a0,-(sp)
                move.l      #'_CPU',d1
                bsr.s       find_cookie
                tst.l       d0
                bmi.s       .mc68000
                move.l      4(a0),d0
                bra.s       .exit
.mc68000:       moveq       #0,d0
.exit:          movem.l     (sp)+,d1/a0
                rts
                ENDC

                IFEQ    TIMER_A+TIMER_B+TIMER_D
cpu_dep_reloc:  rts
                ENDC

;..........................................................................................
find_cookie:    move.l      $5a0.w,d0                           ;Get address of cookie jar in D0
                beq.s       .nojar                              ;If zero, there's no jar.
                move.l      d0,a0                               ;Move the address of the jar to A0
.search:        tst.l       (a0)                                ;Is this jar entry the last one ?
                beq.s       .nofind                             ;Yes, the cookie was not found
                cmp.l       (a0),d1                             ;Does this cookie match what we're looking for?
                beq.s       .foundit                            ;Yes, it does.
                addq.l      #8,a0                               ;Advance to the next jar entry
                bra.s       .search                             ;And start over
.nofind:        moveq       #-1,d0                              ;A negative (-1) means cookie not found
                rts
.nojar:         moveq       #-2,d0                              ;A negative (-2) means there's no jar
                rts
.foundit:       moveq       #0,d0                               ;A null in D0 means the cookie was found.
.exit:          rts


                IFNE    INCLUDE_020
flush_cache:    move.l      d0,-(sp)
                bsr.s       get_cpu_type
                cmpi.w      #10,d0
                bls.s       .end
                cmpi.w      #30,d0
                bls.s       .ohthreeoh

.ohfouroh       cpusha      DC                                  ;040,060 perform cached writes
                cinva       IC                                  ;040,060 invalidate instruction cache
                bra.s       .end

.ohthreeoh      movec       cacr,d0                             ;020,030 flush instruction cache
                bset        #3,d0
                movec       d0,cacr

.end            move.l      (sp)+,d0
                rts
                ENDC
                IFEQ    INCLUDE_020
flush_cache:    rts
                ENDC

;..........................................................................................
setup_dma:      movem.l     d0-d7/a1-a6,-(sp)

                lea         dmasound_setup(pc),a1
                tst.w       (a1)
                bne         .end                                ; dma sound already setup

;.....
.st             lea         .st(pc),a3                          ; voicedatapoint(pc),a3
                add.l       #voicedatapoint-.st,a3
                add.l       (a3),a3                             ; a3 is digi0point
                move.l      a3,a2
                IFEQ        BUILD_BIN
                add.l       #digiparams-digi0point,a2           ; a2 points to digiparams
                ELSE
                lea         10*4+64*$20(a2),a2                  ; a2 points to digiparams
                ENDC
                moveq       #0,d1
                moveq       #0,d2
                move.w      (a2),d1                             ;digi0
                move.w      4(a2),d2
                add.l       d2,d1                               ;digi1
                move.w      8(a2),d2
                add.l       d2,d1                               ;digi2
                move.w      12(a2),d2
                add.l       d2,d1                               ;digi3
                move.w      16(a2),d2
                add.l       d2,d1                               ;digi4
                move.w      20(a2),d2
                add.l       d2,d1                               ;digi5
                move.w      24(a2),d2
                add.l       d2,d1                               ;digi6
                move.w      28(a2),d2
                add.l       d2,d1                               ;digi7
                move.l      d1,d7                               ;keep digi length in d7

                IFNE    DIGIDRUMS_A+DIGIDRUMS_B+DIGIDRUMS_D
                beq.s       .testmachine                        ; if no digi samples dont do more (avoid system trap & keep emu players happy)
                addq.l      #8,d1                               ; total up digilengths including end markers

                movem.l     d1-d2/a0-a2,-(sp)
                move.l      d1,-(sp)                            ; malloc the buffer needed to backup the sample data
                move.w      #$48,-(sp)
                trap        #1
                addq.l      #6,sp
                movem.l     (sp)+,d1-d2/a0-a2

                move.l      d0,a6                               ; a6 points to destination
                tst.l       d0                                  ; Leonard reckons moving to an address register does not set the zero flag
                beq         .end                                ; if error

                move.w      #1,(a1)+                            ; setup ST digidrums

                lea         digivolumetab(pc),a4                ; a4 points to LUT
                moveq       #7,d0
                moveq       #0,d1
                moveq       #0,d2
.stloop         move.l      a6,(a1)+                            ; store pointer to converted drum
                move.l      a3,a5                               ; a3 digi?point
                add.l       (a5),a5                             ; a5 points to source
                move.w      (a2),d1                             ; d1 holds digilength
                subq.w      #1,d1
                bmi.s       .stcopyend
.stcopy         move.b      (a5)+,d2                            ; source
                move.b      (a4,d2.w),(a6)+                     ; 8->4bit
                dbra        d1,.stcopy
.stcopyend      st.b        (a6)+                               ; end marker
                addq.l      #4,a2
                addq.l      #4,a3
                dbra        d0,.stloop
                ENDC


;.....
.testmachine
                IFNE    STE_DMA
                tst.b       49(a0)                              ; test stedmasound
                beq         .end
                cmpi.b      #4,49(a0)                           ; test midi out on ste dma channels
                beq         .end
                bsr         get_snd_type                        ; d0.l holds sound system cookie
                btst        #2,d0                               ; bit4 = cross bar   bit3 = dsp   bit2 = 16bit dma   bit1 = 8bit dma   bit0 = ym
                bne         .falcon
                btst        #1,d0
                bne.s       .ste
                bra         .end

;.....
.ste            lea         dmasound_setup(pc),a1
                move.w      #2,(a1)                             ; setup ste dma sound

                lea         olddmastate(pc),a1
                move.w      $ffff8900.w,(a1)+                   ; interrupts/control
                move.w      $ffff8902.w,(a1)+                   ; start addr
                move.w      $ffff8904.w,(a1)+                   ; start addr
                move.w      $ffff8906.w,(a1)+                   ; start addr
                move.w      $ffff890e.w,(a1)+                   ; end addr
                move.w      $ffff8910.w,(a1)+                   ; end addr
                move.w      $ffff8912.w,(a1)+                   ; end addr
                move.w      $ffff8920.w,(a1)+                   ; track contol/mode

                bsr         init_LMC1992                        ; reset microwire

                cmpi.b      #3,49(a0)
                blo         .steresample
                ;bra        .steresample


.stenative      move.w      #%00000000,$ffff8900.w              ; looping off, no play
                move.w      #%10000010,$ffff8920.w              ; mono, 8 bit, 25033Hz

                ;tst.l      d7                                  ; remember d7 is digilength
                ;beq        .end

                IFNE    STE_DMA_VOLUME
                moveq       #0,d0
                moveq       #0,d1
                move.b      56(a0),d0
                beq.s       .stenativeshift
                cmpi.b      #1,d0
                beq.s       .stenative1
                cmpi.b      #3,d0
                beq.s       .stenative2

.stenative3     addq.w      #1,d1
.stenative2     addq.w      #1,d1
.stenative1     addq.w      #1,d1
.stenativeshift lsl.l       d1,d7                               ; find memory size for all allowed volume levels
                ENDC

                movem.l     d1-d2/a0-a2,-(sp)
                move.w      #$30,-(sp)
                trap        #1
                addq.l      #2,sp                               ;get gemdos version
                tst.b       d0                                  ;d0.b holds major gemdos version
                bne.s       .mxalloc
                lsr.w       #8,d0                               ;d0.b holds minor gemdos version
                cmpi.b      #$19,d0
                bhs.s       .mxalloc
.malloc         move.l      d7,-(sp)                            ;size
                move.w      #$48,-(sp)                          ;Malloc()
                trap        #1
                addq.l      #6,sp
                bra.s       .mallocend
.mxalloc        clr.w       -(sp)                               ;ST RAM only (needed for DMA buffer in case of TT/Falcon with fast RAM)
                move.l      d7,-(sp)                            ;size
                move.w      #$44,-(sp)                          ;Mxalloc()
                trap        #1
                addq.l      #8,sp
.mallocend      movem.l     (sp)+,d1-d2/a0-a2                   ;assume malloc worked ;)
                move.l      d0,a4                               ;a4 points to destination

                lea         preshiftedsamp(pc),a1               ;a1 points to array of sample pointers
                lea         .mallocend(pc),a3                   ;voicedatapoint(pc),a3
                add.l       #voicedatapoint-.mallocend,a3
                add.l       (a3),a3                             ;a3 is digi0point
                move.l      a3,a2
                IFEQ        BUILD_BIN
                add.l       #digiparams-digi0point,a2           ; a2 points to digiparams
                ELSE
                lea         10*4+64*$20(a2),a2                  ; a2 points to digiparams
                ENDC

                moveq       #7,d0                               ;copy 8 samples to buffer - no volume adjust
.nextsample1    move.l      a4,(a1)+                            ;store pointer

                move.l      a3,a5
                add.l       (a5),a5                             ;a5 points to source

                move.w      (a2),d1                             ;digilength
                beq.s       .dontcopy1
                subq.w      #1,d1
.copy1          move.b      (a5)+,(a4)+                         ;copy
                dbra        d1,.copy1

.dontcopy1      addq.l      #4,a3
                addq.l      #4,a2
                dbra        d0,.nextsample1

                IFNE    STE_DMA_VOLUME
                moveq       #0,d2
                move.b      56(a0),d2
                subq.b      #1,d2
                bmi.s       .nativeendpoint                     ;remaining volume levels
.nextvolume     lea         .nextvolume(pc),a2                  ;voicedatapoint(pc),a2
                add.l       #voicedatapoint-.nextvolume,a2
                add.l       (a2),a2                             ;a2 is digi0point
                IFEQ        BUILD_BIN
                add.l       #digiparams-digi0point,a2           ; a2 points to digiparams
                ELSE
                lea         10*4+64*$20(a2),a2                  ; a2 points to digiparams
                ENDC

                moveq       #7,d0                               ;do for the 8 samples
.nextsample2    move.l      a4,(a1)+                            ;store pointer
                lea.l       -9*4(a1),a5
                move.l      (a5),a5                             ;a5 points to source (previously volume adjusted sample)

                move.w      (a2),d1                             ;digilength
                beq.s       .dontcopy2
                subq.w      #1,d1
.copy2          move.b      (a5)+,d3
                asr.b       #1,d3
                move.b      d3,(a4)+                            ;copy
                dbra        d1,.copy2

.dontcopy2      addq.l      #4,a2
                dbra        d0,.nextsample2
                dbra        d2,.nextvolume
                ENDC

.nativeendpoint move.l      a4,(a1)+                            ;store end pointer
                bra         .end


.steresample    move.w      #%00000010,$ffff8900.w                  ; looping on, no play
                move.w      #%10000010,$ffff8920.w                  ; mono, 8 bit, 25033Hz

                lea         dmamixrate(pc),a1
                move.w      #25033,(a1)                             ; store dma mix frequency

                lea         dmabufferpointa(pc),a1
                bsr         mallocdmabuffer     ; a1=points to pointers for dma buffers and remix routines
                                                ; returns d1=buffersize

                move.l      #80500,d0           ; malloc space for generated routines (space needed measured in debugger = 80400)
                ;move.l     #124000,d0          ; malloc space for generated routines (space needed measured in debugger = 123798) <- ste native
                cmpi.b      #2,49(a0)           ; test stedmasound
                bne.s       .mallocsgc          ; if one dma channel only, then malloc less space for generated mixing routines
                addi.l      #80500,d0

.mallocsgc      movem.l     d1-d2/a0-a2,-(sp)                       ; malloc space for resample and remix dma routines at once
                move.l      d0,-(sp)
                move.w      #$48,-(sp)
                trap        #1
                addq.l      #6,sp
                movem.l     (sp)+,d1-d2/a0-a2
                move.l      d0,a2                                   ; d0 = buffer start - assume malloc works :)
                addi.l      #80500,d0
                ;addi.l     #124000,d0
                move.l      d0,(resammixpoints-resamplepoints)(a1)  ; store pointer to remixing routine ready

                lea         dma_freq_tab(pc),a4
                cmpi.b      #3,49(a0)
                bne.s       .notstenative1
                lea         ste_freq_tab(pc),a4
.notstenative1  move.l      d1,d4                               ;d4.w = buffer size

                moveq       #0,d0                               ;d0 holds note number
.genresamste    move.l      a2,(a1)+                            ;store pointer to routine

                moveq       #0,d6
                move.w      (a4)+,d5                            ;d5.w wanted replay rate
                moveq       #0,d6
                move.w      dmamixrate(pc),d6
                divu        d5,d6                               ;d6.w gives whole part
                move.l      d6,d7
                clr.w       d7
                divu        d5,d7                               ;d7.w gives fractional part

                bsr         ste_resamgen
                addq.b      #1,d0
                cmpi.w      #68-24,d0
                blo.s       .genresamste
                bsr         flush_cache                         ;fix for possible TT bug

                cmpi.b      #2,49(a0)                           ; test stedmasound
                bne         .end                                ; if one dma channel only, then dont need to generate mixing routines

                move.l      (a1),a2                             ; (a1) = buffer start, pointer previously stored ready

                lea         dma_freq_tab(pc),a4

                moveq       #0,d0                               ;d0 holds note number
.gensammixste   move.l      a2,(a1)+                            ;store pointer to routine

                moveq       #0,d6
                move.w      (a4)+,d5                            ;d5.w wanted replay rate
                moveq       #0,d6
                move.w      dmamixrate(pc),d6
                divu        d5,d6                               ;d6.w gives whole part
                move.l      d6,d7
                clr.w       d7
                divu        d5,d7                               ;d7.w gives fractional part

                bsr         ste_resammixgen
                addq.b      #1,d0
                cmpi.w      #68-24,d0
                blo.s       .gensammixste
                bsr         flush_cache                         ;fix for possible TT bug

                bra         .end

;.....
.falcon
                IFEQ    BUILD_BIN
                move.l      #"EDIT",d0                          ;if falcon in editor
                cmp.l       editortag(pc),d0
                bne         .falc_noteditor
                ;bra        .falc_noteditor

.falc_editor    lea         crossbarset(pc),a1
                tst.b       (a1)                                ; is sound system setup?
                beq         .end

                lea         dmasound_setup(pc),a1
                move.w      #3,(a1)                             ; setup falcon dma sound

                lea         olddmastate(pc),a1
                move.w      $ffff8900.w,(a1)+                   ; interrupts/control
                move.w      #%00000010,$ffff8900.w              ;   looping on, no play
                move.w      $ffff8902.w,(a1)+                   ; start addr
                move.w      $ffff8904.w,(a1)+                   ; start addr
                move.w      $ffff8906.w,(a1)+                   ; start addr
                move.w      $ffff890e.w,(a1)+                   ; end addr
                move.w      $ffff8910.w,(a1)+                   ; end addr
                move.w      $ffff8912.w,(a1)+                   ; end addr
                move.w      $ffff8920.w,(a1)+                   ; track contol/mode
                move.w      #%01000011,$ffff8920.w              ;   stereo, 16 bit, 50066Hz(not used)

                move.w      $ffff8930.w,(a1)+                   ; crossbar source
                move.w      $ffff8932.w,(a1)+                   ; crossbar destination

                move.l      $ffff8934.w,(a1)+                   ; ste/internal clock, clock freq div, record tracks, DAC input
                move.l      #$01010003,$ffff8934.w              ;   49170Hz, mix both A/D and matrix
                move.l      $ffff8938.w,(a1)+                   ; ADC input, input amplification, output attenuation
                move.l      #$03CC0000,$ffff8938.w              ;   YM, +18db, -0db

                lea         f030_Uwire_emu(pc),a2
                clr.w       (a2)

                lea         dmamixrate(pc),a1
                move.b      crossbarfreq(pc),d0
                andi.b      #%11,d0
                cmpi.b      #1,d0
                beq.s       .clkcd
                bhi.s       .clkdat
.clkint         move.w      #49170,(a1)                         ; store dma mix frequency
                bra         .falc_common
.clkcd          move.w      #44100,(a1)
                bra         .falc_common
.clkdat         move.w      #48000,(a1)
                bra         .falc_common
                ENDC

.falc_noteditor movem.l     d1-d2/a0-a2,-(sp)                   ;attempt to lock sound system
                move.w      #$80,-(sp)
                trap        #14
                addq.l      #2,sp
                movem.l     (sp)+,d1-d2/a0-a2
                tst.l       d0
                bmi         .end

                lea         dmasound_setup(pc),a1
                move.w      #3,(a1)                             ; setup falcon dma sound

                lea         olddmastate(pc),a1
                move.w      $ffff8900.w,(a1)+                   ; interrupts/control
                move.w      #%00000010,$ffff8900.w              ;   looping on, no play
                move.w      $ffff8902.w,(a1)+                   ; start addr
                move.w      $ffff8904.w,(a1)+                   ; start addr
                move.w      $ffff8906.w,(a1)+                   ; start addr
                move.w      $ffff890e.w,(a1)+                   ; end addr
                move.w      $ffff8910.w,(a1)+                   ; end addr
                move.w      $ffff8912.w,(a1)+                   ; end addr
                move.w      $ffff8920.w,(a1)+                   ; track contol/mode
                move.w      #%01000011,$ffff8920.w              ;   stereo, 16 bit, 50066Hz(not used)

                move.w      $ffff8930.w,(a1)+                   ; crossbar source
                move.w      $ffff8932.w,(a1)+                   ; crossbar destination

                andi.w      #$FFF0,$ffff8930.w                  ;   src DMA playback = internal clock, handshake off
                ori.w       #$0001,$ffff8930.w                  ;
                andi.w      #$0FFF,$ffff8932.w                  ;   dest D/A = DMA playback, handshake off
                ori.w       #$1000,$ffff8932.w                  ;
                move.l      $ffff8934.w,(a1)+                   ; ste/internal clock, clock freq div, record tracks, DAC input
                move.l      #$01010003,$ffff8934.w              ;   49170Hz, mix both A/D and matrix
                move.l      $ffff8938.w,(a1)+                   ; ADC input, input amplification, output attenuation
                move.l      #$03CC0000,$ffff8938.w              ;   YM, +18db, -0db

                lea         f030_Uwire_emu(pc),a2
                clr.w       (a2)

                lea         dmamixrate(pc),a1
                move.w      #49170,(a1)                         ; store dma mix frequency

.falc_common    lea         dmabufferpointa(pc),a1
                bsr         mallocdmabuffer                     ; a1=points to pointers for dma buffers and remix routines
                                                                ; returns d1=buffersize

                move.l      #249000,d0
                cmpi.b      #2,49(a0)
                bne.s       .mallocsgcfalc
                addi.l      #161500,d0

.mallocsgcfalc  movem.l     d1-d2/a0-a2,-(sp)
                move.l      d0,-(sp)            ; malloc space for generated routines (space needed measured in debugger = 248502) <- ste native
                move.w      #$48,-(sp)          ;                                     (50066Hz replay rate)
                trap        #1
                addq.l      #6,sp
                movem.l     (sp)+,d1-d2/a0-a2
                move.l      d0,a2                                   ; d0 = buffer start - assume malloc works :)
                addi.l      #161500,d0
                move.l      d0,(resammixpoints-resamplepoints)(a1)  ; store pointer to remix routines

                lea         dma_freq_tab(pc),a4
                cmpi.b      #3,49(a0)
                bne.s       .fnotstenative1
                lea         ste_freq_tab(pc),a4
.fnotstenative1 move.l      d1,d4                               ;d4.w = buffer size

                moveq       #0,d0                               ;d0 holds note number
.genresamfalc   move.l      a2,(a1)+                            ;store pointer to routine

                moveq       #0,d6
                move.w      (a4)+,d5                            ;d5.w wanted replay rate
                moveq       #0,d6
                move.w      dmamixrate(pc),d6
                divu        d5,d6                               ;d6.w gives whole part
                move.l      d6,d7
                clr.w       d7
                divu        d5,d7                               ;d7.w gives fractional part

                bsr         falc_resamgen
                addq.b      #1,d0
                cmpi.w      #68-24,d0
                blo.s       .genresamfalc
                bsr         flush_cache

                cmpi.b      #2,49(a0)                           ; test stedmasound
                bne.s       .end                                ; if one dma channel only, then dont need to generate mixing routines

                move.l      (a1),a2

                lea         dma_freq_tab(pc),a4

                moveq       #0,d0                               ;d0 holds note number
.gensammixfalc  move.l      a2,(a1)+                            ;store pointer to routine

                moveq       #0,d6
                move.w      (a4)+,d5                            ;d5.w wanted replay rate
                moveq       #0,d6
                move.w      dmamixrate(pc),d6
                divu        d5,d6                               ;d6.w gives whole part
                move.l      d6,d7
                clr.w       d7
                divu        d5,d7                               ;d7.w gives fractional part

                bsr         falc_resammixgen
                addq.b      #1,d0
                cmpi.w      #68-24,d0
                blo.s       .gensammixfalc
                bsr         flush_cache
                ENDC


.end            movem.l     (sp)+,d0-d7/a1-a6
                rts


restore_dma:    movem.l     d0/a1-a2,-(sp)

                lea         dmasound_setup(pc),a1
                move.w      (a1),d0
                beq         .end                                ; dma sound not setup anyway

                IFNE    DIGIDRUMS_A+DIGIDRUMS_B+DIGIDRUMS_D
.st             lea         stdigipointers(pc),a1
                movem.l     d0-d2/a0-a2,-(sp)
                move.l      (a1),-(sp)                          ; free the 4bit digi drums
                move.w      #$49,-(sp)
                trap        #1
                addq.l      #6,sp
                movem.l     (sp)+,d0-d2/a0-a2
                REPT        8
                clr.l       (a1)+                               ; clear sample pointers
                ENDR
                ENDC

                IFNE    STE_DMA
                cmpi.w      #3,d0
                beq.s       .falcon
                cmpi.w      #2,d0
                beq.s       .ste
                bra         .end

.ste            lea         olddmastate(pc),a1
                move.w      (a1)+,$ffff8900.w                   ; interupts/control
                bclr        #0,$ffff8901.w                      ;  stop dma
                move.w      (a1)+,$ffff8902.w                   ; start addr
                move.w      (a1)+,$ffff8904.w                   ; start addr
                move.w      (a1)+,$ffff8906.w                   ; start addr
                move.w      (a1)+,$ffff890e.w                   ; end addr
                move.w      (a1)+,$ffff8910.w                   ; end addr
                move.w      (a1)+,$ffff8912.w                   ; end addr
                move.w      (a1)+,$ffff8920.w                   ; track contol/mode
                bsr         init_LMC1992                        ; reset microwire
                bra.s       .falconandste

.falcon         lea         olddmastate(pc),a1
                move.w      (a1)+,$ffff8900.w                   ; interupts/control
                bclr        #0,$ffff8901.w                      ;  stop dma
                move.w      (a1)+,$ffff8902.w                   ; start addr
                move.w      (a1)+,$ffff8904.w                   ; start addr
                move.w      (a1)+,$ffff8906.w                   ; start addr
                move.w      (a1)+,$ffff890e.w                   ; end addr
                move.w      (a1)+,$ffff8910.w                   ; end addr
                move.w      (a1)+,$ffff8912.w                   ; end addr
                move.w      (a1)+,$ffff8920.w                   ; track contol/mode

                move.w      (a1)+,$ffff8930.w                   ; crossbar source
                IFEQ    BUILD_BIN
                move.l      #"EDIT",d0                          ; dont restore crossbar destination if in editor
                cmp.l       editortag(pc),d0
                bne.s       .falcnoeditor
.falceditor     addq.l      #2,a1
                move.l      (a1)+,$ffff8934.w                   ; ste/internal clock, clock freq div, record tracks, DAC input
                move.l      (a1)+,$ffff8938.w                   ; ADC input, input amplification, output attenuation
                bra.s       .falconandste
                ENDC

.falcnoeditor   move.w      (a1)+,$ffff8932.w                   ; crossbar destination
                move.l      (a1)+,$ffff8934.w                   ; ste/internal clock, clock freq div, record tracks, DAC input
                move.l      (a1)+,$ffff8938.w                   ; ADC input, input amplification, output attenuation

                movem.l     d0-d2/a0-a2,-(sp)                   ; unlock sound system
                move.w      #$81,-(sp)
                trap        #14
                addq.l      #2,sp
                movem.l     (sp)+,d0-d2/a0-a2

.falconandste   movem.l     d0-d2/a0-a2,-(sp)
                move.l      dmabufferpointa(pc),-(sp)           ; free the dma buffer
                move.w      #$49,-(sp)
                trap        #1
                addq.l      #6,sp
                movem.l     (sp)+,d0-d2/a0-a2

                movem.l     d0-d2/a0-a2,-(sp)
                move.l      resamplepoints(pc),-(sp)            ; free generated routines
                move.w      #$49,-(sp)
                trap        #1
                addq.l      #6,sp
                movem.l     (sp)+,d0-d2/a0-a2

                ;movem.l    d0-d2/a0-a2,-(sp)
                ;move.l     resammixpoints(pc),-(sp)            ; free generated routines
                ;move.w     #$49,-(sp)
                ;trap       #1
                ;addq.l     #6,sp
                ;movem.l    (sp)+,d0-d2/a0-a2

                movem.l     d0-d2/a0-a2,-(sp)
                move.l      preshiftedsamp(pc),-(sp)            ; free preshifted samples
                move.w      #$49,-(sp)
                trap        #1
                addq.l      #6,sp
                movem.l     (sp)+,d0-d2/a0-a2

                ;clear dma instrument    instrumentD(pc),a2    instrumentend(pc),a1
                lea         .dmainstclear(pc),a1
                lea         .dmainstclear(pc),a2
                add.l       #instrumentend-.dmainstclear,a1
                add.l       #instrumentD-.dmainstclear,a2
.dmainstclear   clr.w       (a2)+
                cmpa.l      a2,a1
                bne.s       .dmainstclear
                ENDC


.end            lea         dmasound_setup(pc),a1
                clr.w       (a1)                                ; clear dma sound setup flag
                movem.l     (sp)+,d0/a1-a2
                rts


dmasound_setup: ds.w        1                                   ; 0=not set, 1=st, 2=ste/tt, 3=falcon
                IFNE    DIGIDRUMS_A+DIGIDRUMS_B+DIGIDRUMS_D
stdigipointers: ds.l        8                                   ; pointers to 4bit digi drums
                ENDC
                IFNE    STE_DMA
olddmastate:    ds.w        8
oldxbarstate:   ds.w        6
dmamixrate:     ds.w        1
dmabufferpointa ds.l        1                                   ; start
dmabufferpointb ds.l        1                                   ; middle
resamplepoints: ds.l        68-24                               ; pointers to generated resampling routines
resammixpoints: ds.l        68-24                               ; pointers to generated resampling and remixing routines
preshiftedsamp: ds.l        8+1
                IFNE    STE_DMA_VOLUME
                ds.l        7*8
                ENDC
                ENDC


                IFNE    DIGIDRUMS_A+DIGIDRUMS_B+DIGIDRUMS_D
digivolumetab:  include     replay.s/digi_vol.s                          ;8->4 bit LUT for coverting dma samples into YM digidrums
                ENDC


                ; bit4 = cross bar   bit3 = dsp   bit2 = 16bit dma   bit1 = 8bit dma   bit0 = ym
get_snd_type:
                movem.l     d1/a0,-(sp)
                move.l      #'_MCH',d1
                bsr         find_cookie
                tst.l       d0
                bmi.s       .st
                move.l      4(a0),d0
                bra.s       .got
.st             moveq       #0,d0
.got            swap        d0                                  ;0=ST, 1=STe, 2=TT, 3=Falcon
                tst.w       d0
                beq.s       .ym
                cmpi.w      #2,d0
                ble.s       .8bit
                cmpi.w      #3,d0
                beq.s       .16bit

.ym:            moveq       #%00001,d0
                movem.l     (sp)+,d1/a0
                rts

.8bit:          moveq       #%00011,d0
                movem.l     (sp)+,d1/a0
                rts

.16bit:         moveq       #%11111,d0
                movem.l     (sp)+,d1/a0
                rts


;get_snd_type:
;               movem.l     d1/a0,-(sp)
;               move.l      #'_SND',d1
;               bsr         find_cookie
;               tst.l       d0
;               bmi.s       .stsnd
;               move.l      4(a0),d0
;               bra.s       .exit
;.stsnd:        moveq       #1,d0                       ;YM2149 sound only
;.exit:         movem.l     (sp)+,d1/a0
;               rts



                IFNE    STE_DMA
mallocdmabuffer:move.b      14(a0),d0                   ; d0 holds driver replay freq
                cmpi.b      #66,d0
                bhi.s       .over66
                moveq       #4,d1                       ; d1 holds maximum number of 200Hz intervals between calls
                bra.s       .found200hzcall
.over66         cmpi.b      #99,d0
                bhi.s       .over99
                moveq       #3,d1                       ; d1 holds maximum number of 200Hz intervals between calls
                bra.s       .found200hzcall
.over99         cmpi.b      #199,d0
                bhi.s       .over199
                moveq       #2,d1                       ; d1 holds maximum number of 200Hz intervals between calls
                bra.s       .found200hzcall
.over199        moveq       #1,d1

.found200hzcall moveq       #0,d3
                move.w      dmamixrate(pc),d3
                move.l      d3,d4
                lsr.w       #1,d3                       ;scale to aviod overflow at higher frequencies
                divu        #190/2,d3
                divu        #180/2,d4


                btst        #6,$ffff8921.w              ;is sound system 16bit?
                beq.s       .no16adjust
.do16adjust     add.w       d3,d3
                add.w       d3,d3
                add.w       d4,d4
                add.w       d4,d4

.no16adjust     move.l      d1,d5                       ; d1+d5 holds maximum number of 200Hz intervals between calls
                mulu        d3,d1                       ; d1 holds size of single dma buffer
                mulu        d4,d5                       ; d5 holds malloc amount for 2 dma buffers (malloc a little more than needed)

                movem.l     d1-d2/a0-a2,-(sp)           ; d1=size of single dma buffer, d2=malloc amount
                move.w      #$30,-(sp)
                trap        #1
                addq.l      #2,sp                       ;get gemdos version
                tst.b       d0                          ;d0.b holds major gemdos version
                bne.s       .mxalloc
                lsr.w       #8,d0                       ;d0.b holds minor gemdos version
                cmpi.b      #$19,d0
                bhs.s       .mxalloc
.malloc:        move.l      d5,-(sp)                    ;size = guard+physical+logical
                move.w      #$48,-(sp)                  ;Malloc()
                trap        #1
                addq.l      #6,sp
                bra.s       .mallocend
.mxalloc:       clr.w       -(sp)                       ;ST RAM only (needed for DMA buffer in case of TT/Falcon with fast RAM)
                move.l      d5,-(sp)                    ;size = guard+physical+logical
                move.w      #$44,-(sp)                  ;Mxalloc()
                trap        #1
                addq.l      #8,sp
.mallocend      movem.l     (sp)+,d1-d2/a0-a2

                addq.l      #1,d0
                bclr        #0,d0                       ; ensure buffer starts on word boundary

                move.l      d0,(a1)+                    ; d0 = buffer start - assume malloc works :)
                move.l      d0,d2                       ; preserve start address in d2

                swap        d0
                move.b      d0,$ffff8903.w              ; upper byte first
                swap        d0
                move.b      d0,$ffff8907.w
                lsr.w       #8,d0
                move.b      d0,$ffff8905.w              ; buffer start

                add.l       d1,d2                       ; d2 = buffer middle
                move.l      d2,(a1)+
                add.l       d1,d2                       ; d2 = buffer end

                swap        d2
                move.b      d2,$ffff890f.w              ; upper byte first
                swap        d2
                move.b      d2,$ffff8913.w
                lsr.w       #8,d2
                move.b      d2,$ffff8911.w              ; buffer end
                rts                                     ; return d1=buffersize


ste_resamgen:   movem.l     d0-d7,-(sp)                 ;generate resampling code from address a2
                                                        ;d7.w=fraction  d6.w=whole  d5.w=samcount
                                                        ;d4.w=number of destination bytes to write

                IFNE    STE_DMA_VOLUME
                move.l      .copy_from(pc),d0

                tst.b       56(a0)                      ;determine if asr operation is really needed
                bne.s       .d0initdone
                cmpi.b      #1,49(a0)
                bne.s       .d0initdone

                swap        d0
                ENDC

                IFEQ    STE_DMA_VOLUME
                move.w      .copy_from(pc),d0
                ENDC


.d0initdone     move.w      .copy_to(pc),d1
                moveq       #0,d3                       ;fraction counter

                lea         .endwhole(pc),a3
                muls        #-6,d6
                lea         (a3,d6.w),a3

.top
                IFNE    STE_DMA_VOLUME
                tst.b       56(a0)
                bne.s       .yesvol
                cmpi.b      #1,49(a0)
                bne.s       .yesvol
.novol          move.w      d0,(a2)+
                bra.s       .endsource
.yesvol         move.l      d0,(a2)+                    ;get source data
                ENDC

                IFEQ    STE_DMA_VOLUME
                move.w      d0,(a2)+                    ;get source data
                ENDC

.endsource      jmp         (a3)                        ;whole part
                REPT        12
                move.w      d1,(a2)+                    ;write source data
                subq.w      #1,d4
                ble.s       .writerts
                ENDR
.endwhole
                add.w       d7,d3
                bcc.s       .top
                move.w      d1,(a2)+                    ;write source data
                subq.w      #1,d4
                ble.s       .writerts
                bra.s       .top

.writerts       move.w      #$4E75,(a2)+
                movem.l     (sp)+,d0-d7
                rts

.copy_from      move.b      (a6)+,d1                ;long
                asr.b       d2,d1
.copy_to        move.b      d1,(a5)+                ;word
.copy_end



ste_resammixgen movem.l     d0-d7,-(sp)             ;generate resampling code from address a2
                                                    ;d7.w=fraction  d6.w=whole  d5.w=mixcount
                                                    ;d4.w=number of destination bytes to write
                IFNE    STE_DMA_VOLUME
                move.l      .mix_from(pc),d0
                ENDC
                IFEQ    STE_DMA_VOLUME
                move.w      .mix_from(pc),d0
                ENDC

                move.w      .mix_to(pc),d1
                moveq       #0,d3                   ;fraction counter

                lea         .endwhole(pc),a3
                muls        #-6,d6
                lea         (a3,d6.w),a3

.top
                IFNE    STE_DMA_VOLUME
                move.l      d0,(a2)+                ;get source data
                ENDC
                IFEQ    STE_DMA_VOLUME
                move.w      d0,(a2)+                ;get source data
                ENDC

                jmp         (a3)                    ;whole part
                REPT        12
                move.w      d1,(a2)+                ;write source data
                subq.w      #1,d4
                ble.s       .writerts
                ENDR
.endwhole
                add.w       d7,d3
                bcc.s       .top
                move.w      d1,(a2)+                ;write source data
                subq.w      #1,d4
                ble.s       .writerts
                bra.s       .top

.writerts       move.w      #$4E75,(a2)+
                movem.l     (sp)+,d0-d7
                rts

.mix_from       move.b      (a6)+,d1                ;word
                asr.b       d2,d1                   ;word
.mix_to         add.b       d1,(a5)+                ;word
.mix_end



falc_resamgen:  movem.l     d0-d7,-(sp)             ;generate resampling code from address a2
                                                    ;d7.w=fraction  d6.w=whole  d5.w=samcount
                                                    ;d4.w=number of destination bytes to write

                IFNE    STE_DMA_VOLUME
                move.l      .copy_from1(pc),d0

                tst.b       56(a0)                  ;determine if asr operation is really needed
                bne.s       .d0initdone
                cmpi.b      #1,49(a0)
                bne.s       .d0initdone

                swap        d0
                ENDC

                IFEQ    STE_DMA_VOLUME
                move.w      .copy_from1(pc),d0
                ENDC

.d0initdone     move.l      .copy_from2(pc),d2

                move.w      .copy_to(pc),d1
                moveq       #0,d3                   ;fraction counter
                move.l      d6,a3                   ;a3 stores whole part

.top
                IFNE    STE_DMA_VOLUME
                tst.b       56(a0)
                bne.s       .yesvol
                cmpi.b      #1,49(a0)
                bne.s       .yesvol
.novol          move.w      d0,(a2)+
                bra.s       .endsource
.yesvol         move.l      d0,(a2)+                ;get source data
                ENDC
                IFEQ    STE_DMA_VOLUME
                move.w      d0,(a2)+                ;get source data
                ENDC

.endsource      move.l      d2,(a2)+                ;convert 8->16bit

                move.w      a3,d6
                beq.s       .endwhole
                subq.w      #1,d6
.wholetop       move.w      d1,(a2)+                ;write source data
                subq.w      #4,d4
                ble.s       .writerts
                dbra        d6,.wholetop
.endwhole
                add.w        d7,d3
                bcc.s        .top
                move.w       d1,(a2)+               ;write source data
                subq.w       #4,d4
                ble.s       .writerts
                bra.s       .top

.writerts       move.w      #$4E75,(a2)+            ;write rts
                movem.l     (sp)+,d0-d7
                rts

.copy_from1     move.b      (a6)+,d1                ;word
                asr.b       d2,d1                   ;word
.copy_from2     move.l      (a3,d1.w*4),d0          ;long
.copy_to        move.l      d0,(a5)+                ;word
.copy_end


falc_resammixgen
                movem.l     d0-d7,-(sp)             ;generate resampling code from address a2
                                                    ;d7.w=fraction  d6.w=whole  d5.w=mixcount
                                                    ;d4.w=number of destination bytes to write
                IFNE    STE_DMA_VOLUME
                move.l      .mix_from1(pc),d0
                ENDC
                IFEQ    STE_DMA_VOLUME
                move.w      .mix_from1(pc),d0
                ENDC
                move.l      .mix_from2(pc),d2

                move.w      .mix_to(pc),d1
                moveq       #0,d3                   ;fraction counter
                move.l      d6,a3                   ;a3 stores whole part

.top
                IFNE    STE_DMA_VOLUME
                move.l      d0,(a2)+                ;get source data
                ENDC
                IFEQ    STE_DMA_VOLUME
                move.w      d0,(a2)+                ;get source data
                ENDC
                move.l      d2,(a2)+                ;convert 8->16bit

                move.w      a3,d6
                beq.s       .endwhole
                subq.w      #1,d6
.topwhole       move.w      d1,(a2)+                ;write source data
                subq.w      #4,d4
                ble.s       .writerts
                dbra        d6,.topwhole
.endwhole
                add.w       d7,d3
                bcc.s       .top
                move.w      d1,(a2)+                ;write source data
                subq.w      #4,d4
                ble.s       .writerts
                bra.s       .top

.writerts       move.w      #$4E75,(a2)+            ;write rts=$4E75 illegal=$4afc
                movem.l     (sp)+,d0-d7
                rts

.mix_from1      move.b      (a6)+,d1                ;word
                asr.b       d2,d1                   ;word
.mix_from2      move.l      (a3,d1.w*4),d0          ;long
.mix_to         add.l       d0,(a5)+                ;word
.mix_end



init_LMC1992:   move.l      d0,-(sp)
                move.w      #%10000000001,d0        ;mix DMA and YM equally
                bsr.s       set_LMC1992
                move.w      #%10001000110,d0        ;+0db bass
                bsr.s       set_LMC1992
                move.w      #%10010000110,d0        ;+0db treble
                bsr.s       set_LMC1992
                move.w      #%10011101000,d0        ;-0db master volume
                bsr.s       set_LMC1992
                move.w      #%10100010100,d0        ;-0db right
                bsr.s       set_LMC1992
                move.w      #%10101010100,d0        ;-0db left
                bsr.s       set_LMC1992
                move.l      (sp)+,d0
                rts


set_LMC1992:    move.w      sr,-(sp)
                move.w      #$2700,sr                       ;interrupts off during start of operation
                move.w      #%11111111111,$ffff8924.w       ;set microwire mask
                move.w      d0,$ffff8922.w

.waitstart      cmpi.w      #%11111111111,$ffff8924.w       ;wait for microwire write to start
                beq.s       .waitstart
                move.w      (sp)+,sr                        ;now microwire write started, we can safely re-enable interrupts

.waitend        cmpi.w      #%11111111111,$ffff8924.w       ;wait for microwire write to finish
                bne.s       .waitend
                rts


fake_LMC1992:   movem.l     d0-d1,-(sp)
                moveq       #0,d0                           ;d0=left
                move.b      f030_Uwire_emu(pc),d0
                move.l      d0,d1                           ;d1=right
                add.b       f030_Uwire_emu+1(pc),d0
                add.b       f030_Uwire_emu+2(pc),d1
                cmpi.b      #$F,d0
                bls.s       .noclipL
                moveq       #$F,d0
.noclipL        cmpi.b      #$F,d1
                bls.s       .noclipR
                moveq       #$F,d1
.noclipR        lsl.w       #4,d0
                add.b       d1,d0
                lsl.w       #4,d0
                move.w      d0,$ffff893a.w                  ;set falcon attenuation register
                movem.l     (sp)+,d0-d1
                rts
f030_Uwire_emu: ds.b        1                               ;stores master volume level 0->F
                ds.b        1                               ;stores left volume level   0->F
                ds.b        1                               ;stores right volume level  0->F
                ds.b        1                               ;not used
                even

dma_freq_tab:   include     replay.s/dma_freq.s
ste_freq_tab:   include     replay.s/ste_freq.s
eight_2_sixteen include     replay.s/8_to_16.s
                ENDC

                IFEQ    BUILD_BIN
                IFEQ    STE_DMA
set_LMC1992:
fake_LMC1992:   rts
f030_Uwire_emu: ds.b        4
                ENDC
                ENDC

;..........................................................................................
                IFNE    TIMER_A+TIMER_B+TIMER_D
setup_vecr_spi: move.l      a1,-(sp)                        ;assume a0 points to tracker data
                move.w      sr,-(sp)
                move.w      #$2700,sr

                tst.b       36(a0)                          ;if no timers used
                beq.s       .end

                lea         oldvectorreg(pc),a1             ;stolen vector register and spurious interrupt already?
                tst.b       1(a1)
                bne.s       .end

                st.b        1(a1)                           ;set stolen

                btst        #3,$FFFFFA17.w                  ;store
                sne.b       (a1)

                bclr        #3,$FFFFFA17.w                  ;automatic end of interrupt - vector register

                move.l      $60.w,2(a1)                     ;store old spurious interrupt vector

                lea         dummyinterrupt(pc),a1           ;set spurious interrupt vector to dummy routine
                move.l      a1,$60.w

.end            move.w      (sp)+,sr
                move.l      (sp)+,a1
                rts


rest_vecr_spi:  move.l      a1,-(sp)
                move.w      sr,-(sp)
                move.w      #$2700,sr

                lea         oldvectorreg(pc),a1
                tst.b       1(a1)                           ;test if stolen
                beq.s       .end                            ;if not stolen, dont do anything

                clr.b       1(a1)                           ;set not stolen

                tst.b       (a1)                            ;restore vector register
                beq.s       .clear
.set            bset        #3,$FFFFFA17.w
                bra.s       .restore_spi
.clear          bclr        #3,$FFFFFA17.w

.restore_spi    move.l      2(a1),$60.w                     ;restore spurious interrupt

.end            move.w      (sp)+,sr
                move.l      (sp)+,a1
                rts

oldvectorreg:   ds.b        1        ;+0
vecr_spi_stolen ds.b        1        ;+1
oldspuriousint: ds.l        1        ;+2
                ENDC

                IFEQ    TIMER_A+TIMER_B+TIMER_D
setup_vecr_spi: rts
rest_vecr_spi:  rts
                ENDC

;..........................................................................................
                IFNE    TIMER_A
setuptimerA:    move.l      a1,-(sp)                        ;assume a0 points to tracker data
                move.w      sr,-(sp)
                move.w      #$2700,sr

                btst        #2,36(a0)                       ;test if timer enabled
                beq.s       .end

                lea         oldAvector(pc),a1               ;store old timer A vector

                tst.b       8(a1)
                bne.s       .end                            ;timer already enabled

                move.l      $134.w,(a1)+
                move.b      $FFFFFA19.w,(a1)+               ;store old timer A control
                move.b      $FFFFFA1F.w,(a1)+               ;store old timer A data
                btst        #5,$FFFFFA07.w
                sne.b       (a1)+                           ;enable
                btst        #5,$FFFFFA13.w
                sne.b       (a1)+                           ;mask

                st.b        (a1)+                           ;set enabled flag
                clr.b       (a1)+                           ;timer usage off

                lea         dummyinterrupt(pc),a1           ;setup dummy interrupt vector...
                move.l      a1,$134.w                       ;...on timer A
                clr.b       $FFFFFA19.w                     ;divider B
                clr.b       $FFFFFA1F.w                     ;max counter A
                bset        #5,$FFFFFA07.w                  ;enable timer A
                bset        #5,$FFFFFA13.w                  ;unmask timer A


.end            move.w      (sp)+,sr
                move.l      (sp)+,a1
                rts


restoretimerA:  move.l      a1,-(sp)
                move.w      sr,-(sp)
                move.w      #$2700,sr

                lea         oldAvector(pc),a1               ;restore old timer A values

                tst.b       8(a1)
                beq.s       .end                            ;timer not enabled anyway

                move.l      (a1)+,$134.w
                move.b      (a1)+,$FFFFFA19.w
                move.b      (a1)+,$FFFFFA1F.w

                tst.b       (a1)+
                beq.s       .enaAoff
.enaAon         bset.b      #5,$FFFFFA07.w
                bra.s       .tstmask
.enaAoff        bclr.b      #5,$FFFFFA07.w

.tstmask        tst.b       (a1)+
                beq.s       .mskAoff
.mskAon         bset.b      #5,$FFFFFA13.w
                bra.s       .endrestoreA
.mskAoff        bclr.b      #5,$FFFFFA13.w
.endrestoreA

                clr.b       (a1)+                           ;timer not enabled
                clr.b       (a1)+                           ;timer usage off

.end            move.w      (sp)+,sr
                move.l      (sp)+,a1
                rts


;timer restore data
oldAvector:     ds.l        1       ;+0
oldAcontrol:    ds.b        1       ;+4
oldAdata:       ds.b        1       ;+5
oldintenA:      ds.b        1       ;+6
oldintmaA:      ds.b        1       ;+7
timerAenabled:  ds.b        1       ;+8
timerusageA:    ds.b        1       ;+9
                even
                ENDC

                IFEQ    TIMER_A
setuptimerA:    rts
restoretimerA:  rts
                ENDC
;..........................................................................................
                IFNE    TIMER_B
setuptimerB:    move.l      a1,-(sp)                        ;assume a0 points to tracker data
                move.w      sr,-(sp)
                move.w      #$2700,sr

                btst        #1,36(a0)                       ;test if timer enabled
                beq.s       .end

                lea         oldBvector(pc),a1               ;store old timer B vector

                tst.b       8(a1)
                bne.s       .end                            ;timer already enabled

                move.l      $120.w,(a1)+
                move.b      $FFFFFA1B.w,(a1)+               ;store old timer B control
                move.b      $FFFFFA21.w,(a1)+               ;store old timer B data
                btst        #0,$FFFFFA07.w
                sne.b       (a1)+                           ;enable
                btst        #0,$FFFFFA13.w
                sne.b       (a1)+                           ;mask

                st.b        (a1)+                           ;set enabled flag
                clr.b       (a1)+                           ;timer not used

                lea         dummyinterrupt(pc),a1           ;setup dummy interrupt vector...
                move.l      a1,$120.w                       ;...on timer B
                clr.b       $FFFFFA1B.w                     ;divider B
                clr.b       $FFFFFA21.w                     ;max counter B
                bset        #0,$FFFFFA07.w                  ;enable timer B
                bset        #0,$FFFFFA13.w                  ;unmask timer B


.end            move.w      (sp)+,sr
                move.l      (sp)+,a1
                rts


restoretimerB:  move.l      a1,-(sp)
                move.w      sr,-(sp)
                move.w      #$2700,sr

                lea         oldBvector(pc),a1               ;restore old timer A values

                tst.b       8(a1)
                beq.s       .end                            ;timer not enabled anyway

                move.l      (a1)+,$120.w
                move.b      (a1)+,$FFFFFA1B.w
                move.b      (a1)+,$FFFFFA21.w

                tst.b       (a1)+
                beq.s       .enaBoff
.enaBon         bset.b      #0,$FFFFFA07.w
                bra.s       .tstmask
.enaBoff        bclr.b      #0,$FFFFFA07.w

.tstmask        tst.b       (a1)+
                beq.s       .mskBoff
.mskBon         bset.b      #0,$FFFFFA13.w
                bra.s       .endrestoreB
.mskBoff        bclr.b      #0,$FFFFFA13.w
.endrestoreB

                clr.b       (a1)+                           ;timer not enabled
                clr.b       (a1)+                           ;timer not used

.end            move.w      (sp)+,sr
                move.l      (sp)+,a1
                rts


;timer restore data
oldBvector:     ds.l        1       ;+0
oldBcontrol:    ds.b        1       ;+4
oldBdata:       ds.b        1       ;+5
oldintenB:      ds.b        1       ;+6
oldintmaB:      ds.b        1       ;+7
timerBenabled:  ds.b        1       ;+8
timerusageB:    ds.b        1       ;+9
                even
                ENDC

                IFEQ    TIMER_B
setuptimerB:    rts
restoretimerB:  rts
                ENDC
;..........................................................................................
                IFNE    TIMER_D
setuptimerD:    move.l      a1,-(sp)                        ;assume a0 points to tracker data
                move.w      sr,-(sp)
                move.w      #$2700,sr

                btst        #0,36(a0)                       ;test if timer enabled
                beq.s       .end

                lea         oldDvector(pc),a1               ;store old timer D vector

                tst.b       8(a1)
                bne.s       .end                            ;timer already enabled

                move.l      $110.w,(a1)+
                move.b      $FFFFFA1D.w,(a1)                ;store old timer D control
                andi.b      #%00001111,(a1)+
                move.b      $FFFFFA25.w,(a1)+               ;store old timer D data
                btst        #4,$FFFFFA09.w
                sne.b       (a1)+                           ;enable
                btst        #4,$FFFFFA15.w
                sne.b       (a1)+                           ;mask

                st.b        (a1)+                           ;set enabled flag
                clr.b       (a1)+                           ;timer usage off

                lea         dummyinterrupt(pc),a1           ;setup dummy interrupt vector...
                move.l      a1,$110.w                       ;...on timer D
                andi.b      #%11110000,$FFFFFA1D.w          ;divider D
                clr.b       $FFFFFA25.w                     ;max counter D
                bset        #4,$FFFFFA09.w                  ;enable timer D
                bset        #4,$FFFFFA15.w                  ;unmask timer D

.end            move.w      (sp)+,sr
                move.l      (sp)+,a1
                rts


restoretimerD:  movem.l     a1/d0,-(sp)
                move.w      sr,-(sp)
                move.w      #$2700,sr

                lea         oldDvector(pc),a1               ;restore old timer D values

                tst.b       8(a1)
                beq.s       .end                            ;timer not enabled anyway

                move.l      (a1)+,$110.w
                move.b      $FFFFFA1D.w,d0
                andi.b      #%11110000,d0
                or.b        (a1)+,d0
                move.b      d0,$FFFFFA1D.w                  ;control
                move.b      (a1)+,$FFFFFA25.w               ;data

                tst.b       (a1)+
                beq.s       .enaDoff
.enaDon         bset.b      #4,$FFFFFA09.w
                bra.s       .tstmask
.enaDoff        bclr.b      #4,$FFFFFA09.w

.tstmask        tst.b       (a1)+
                beq.s       .mskDoff
.mskDon         bset.b      #4,$FFFFFA15.w
                bra.s       .endrestoreD
.mskDoff        bclr.b      #4,$FFFFFA15.w
.endrestoreD

                clr.b       (a1)+                           ;timer not enabled
                clr.b       (a1)+                           ;timer usage off

.end            move.w      (sp)+,sr
                movem.l     (sp)+,a1/d0
                rts


;timer restore data
oldDvector:     ds.l        1       ;+0
oldDcontrol:    ds.b        1       ;+4
oldDdata:       ds.b        1       ;+5
oldintenD:      ds.b        1       ;+6
oldintmaD:      ds.b        1       ;+7
timerDenabled:  ds.b        1       ;+8
timerusageD:    ds.b        1       ;+9
                even
                ENDC

                IFEQ    TIMER_D
setuptimerD:    rts
restoretimerD:  rts
                ENDC
;..........................................................................................

replayrout:     movem.l     d0-a6,-(sp)

                lea         replayrout(pc),a0               ; lea trakerdatapoint(pc),a0
                add.l       #trakerdatapoint-replayrout,a0
                add.l       (a0),a0                         ; a0 points to tracker data
                addq.l      #8,a0
                tst.b       25(a0)                          ; test playing byte
                bne.s       .playing

                IFEQ    BUILD_BIN
                lea         editortag(pc),a1                ;if song play, we change to the current song position
                cmpi.l      #"EDIT",(a1)
                beq         .setupym
                ENDC

                movem.l     (sp)+,d0-a6
                rts

;..........................................
; we're playing - need to parse line?
.playing        tst.b       48(a0)                          ;test song speed counter
                bne         .setupym

;..........................................
; counter = 0, so parse tracker line
.testparse1     lea         .testparse1(pc),a4              ; lea instrumentA(pc),a4
                add.l       #instrumentA-.testparse1,a4
                tst.b       61(a4)                          ; test rle counter
                bne.s       .skipparse1
                moveq       #0,d0
                moveq       #0,d1
                move.b      4(a0),d0                        ; pattern number channel 1
                move.b      44(a0),d1                       ; track pattern position
                btst        #0,37(a0)                       ; channel muted?
                sne         d2
                bsr         parsetracker                    ; parse Ch A tracker codes
                addq.b      #1,44(a0)                       ; increase position for next time
                bra.s       .testparse2
.skipparse1     subq.b      #1,61(a4)


.testparse2     lea         .testparse2(pc),a4              ; lea instrumentB(pc),a4
                add.l       #instrumentB-.testparse2,a4
                tst.b       61(a4)                          ; test rle counter
                bne.s       .skipparse2
                moveq       #0,d0
                moveq       #0,d1
                move.b      5(a0),d0                        ; pattern number channel 2
                move.b      45(a0),d1
                btst        #1,37(a0)                       ; channel muted?
                sne         d2
                bsr         parsetracker                    ; parse Ch B tracker codes
                addq.b      #1,45(a0)                       ; increase position for next time
                bra.s       .testparse3
.skipparse2     subq.b      #1,61(a4)


.testparse3     lea         .testparse3(pc),a4              ; lea instrumentC(pc),a4
                add.l       #instrumentC-.testparse3,a4
                tst.b       61(a4)                          ; test rle counter
                bne.s       .skipparse3
                moveq       #0,d0
                moveq       #0,d1
                move.b      6(a0),d0                        ; pattern number channel 3
                move.b      46(a0),d1
                btst        #2,37(a0)                       ; channel muted?
                sne         d2
                bsr         parsetracker                    ; parse Ch C tracker codes
.mute3          addq.b      #1,46(a0)                       ; increase position for next time
                bra.s       .testparse4
.skipparse3     subq.b      #1,61(a4)


.testparse4
                IFEQ    BUILD_BIN
                cmpi.b      #4,49(a0)                       ;test if midi on ste dma channels
                beq.s       .parsemididma
                ENDC
                move.w      dmasound_setup(pc),d0           ;if STFM or DMA not setup
                cmpi.w      #1,d0
                bls.s       .parseend
.parsemididma   lea         .parsemididma(pc),a4            ; lea instrumentD(pc),a4
                add.l       #instrumentD-.parsemididma,a4
                tst.b       7(a4)                           ; test rle counter
                bne.s       .skipparse4
                moveq       #0,d0
                moveq       #0,d1
                move.b      7(a0),d0                        ; pattern number channel 4
                move.b      47(a0),d1                       ; track position channel 4
                bsr         parsetrackste                   ; parse Ch D tracker codes
.mute4          addq.b      #1,47(a0)                       ; increase position for next time
                bra.s       .parseend
.skipparse4     subq.b      #1,7(a4)

.parseend

;..........................................
; we're playing - do per replay rout update
; setup YM registers
.setupym        lea         .setupym(pc),a1                 ; lea YMtemp(pc),a1
                add.l       #YMtemp-.setupym,a1
                ;       a2 is temp instrument pointer
                lea         .setupym(pc),a3                 ; lea voicedatapoint(pc),a3
                add.l       #voicedatapoint-.setupym,a3
                add.l       (a3),a3
                IFEQ    BUILD_BIN
                lea         sequencedata-digi0point(a3),a3
                ENDC
                IFNE    BUILD_BIN
                lea         2120(a3),a3                     ; may need changing with modified data structures
                ENDC
                ; a3 points to sequence data
                lea         .setupym(pc),a4                 ; lea instrumentstart(pc),a4
                add.l       #instrumentstart-.setupym,a4
                ;       a5 temp
                ;       a6 temp
                ;       a7 is stack pointer

; ...................
; caclulate sequences
;                lea         INSTAOFFSET(a4),a2
                lea         (a4),a2                         ; ggn
                bsr         dosequences
                lea         INSTBOFFSET(a4),a2
                bsr         dosequences
                lea         INSTCOFFSET(a4),a2
                bsr         dosequences

; .................
; setup YM volume A
                ;a2 points to instrument
                ;d0 holds return value
                moveq       #0,d0
;                lea.l       INSTAOFFSET(a4),a2
                lea.l       (a4),a2                         ; ggn
                bsr         getchvol
.setvolA        move.b      d0,32+2(a1)                     ;(8*4)+2

; .................
; setup YM volume B
                ;a2 points to instrument
                ;d0 holds return value
                moveq       #0,d0
                lea.l       INSTBOFFSET(a4),a2
                bsr         getchvol
.setvolB        move.b      d0,36+2(a1)                     ;(8*4)+2

; .................
; setup YM volume C
                ;a2 points to instrument
                ;d0 holds return value
                moveq       #0,d0
                lea.l       INSTCOFFSET(a4),a2
                bsr         getchvol
.setvolC        move.b      d0,40+2(a1)                     ;(8*4)+2


; .............................
; setup YM registers - YM mixer
                moveq       #-1,d3                          ;d3 = new mixer        b5=Cnoise b4=Bnoise b3=Anoise b2=Ctone b1=Btone b0=Atone
                                                            ;IO ports set to output

; A......
                tst.b       32+2(a1)
                beq.s       .nosquareA
                move.b      INSTAOFFSET+78(a4),d0           ;noise and square in bottom 2 nibbles of d0
                btst        #4,d0                           ;d1 gives noise
                beq.s       .nonoiseA
                bclr        #3,d3
.nonoiseA       btst        #0,d0                           ;d0 gives square
                beq.s       .nosquareA
                bclr        #0,d3
.nosquareA


; B......
                tst.b       36+2(a1)
                beq.s       .nosquareB
                move.b      INSTBOFFSET+78(a4),d0           ;noise and square in bottom 2 nibbles of d0
                btst        #4,d0                           ;d1 gives noise
                beq.s       .nonoiseB
                bclr        #4,d3
.nonoiseB       btst        #0,d0                           ;d0 gives square
                beq.s       .nosquareB
                bclr        #1,d3
.nosquareB

; C......
                tst.b       40+2(a1)
                beq.s       .nosquareC
                move.b      INSTCOFFSET+78(a4),d0           ;noise and square in bottom 2 nibbles of d0
                btst        #4,d0                           ;d1 gives noise
                beq.s       .nonoiseC
                bclr        #5,d3
.nonoiseC:      btst        #0,d0                           ;d0 gives square
                beq.s       .nosquareC
                bclr        #2,d3
.nosquareC

; set....
                move.b      d3,28+2(a1)                     ;move YM mixer to YMTemp

; ...............................
; setup YM registers - noise freq
                btst        #5,d3                           ;mixer still setup on d3
                beq.s       .noisefC                        ;higher channels get priority
                btst        #4,d3
                beq.s       .noisefB
                btst        #3,d3
                beq.s       .noisefA
                bra.s       .endnoisefreq                   ;if noise not used, dont bother setting it

.noisefC        lea         INSTCOFFSET(a4),a2
                bra.s       .setnoisefreq
.noisefB        lea         INSTBOFFSET(a4),a2
                bra.s       .setnoisefreq
;.noisefA        lea         INSTAOFFSET(a4),a2
.noisefA        lea         (a4),a2                         ; ggn

.setnoisefreq   moveq       #0,d0
                move.w      80(a2),d0                       ;precalculated noise freq

.skiptonoitrans add.b       48(a2),d0                       ;include effect noise transpose
                IFEQ    BUILD_BIN
                add.b       92(a2),d0                       ;include track noise transpose
                ENDC
                bge.s       .noiseabovezero
.noisebelowzero moveq       #0,d0
                bra.s       .writenoise
.noiseabovezero cmpi.b      #$20,d0
                blt.s       .writenoise
                moveq       #$1F,d0

.writenoise     move.b      d0,24+2(a1)
.endnoisefreq


; .............................................
; find buzzer channel
                move.w      INSTCOFFSET+78(a4),d0
                andi.w      #$00F0,d0
                bne.s       .foundbuzzerC
                move.w      INSTBOFFSET+78(a4),d0
                andi.w      #$00F0,d0
                bne.s       .foundbuzzerB
                move.w      INSTAOFFSET+78(a4),d0
                andi.w      #$00F0,d0
                bne.s       .foundbuzzerA

.notfoundbuzzer move.w      #0,52(a1)                       ;set buzzer waveform channel
                bra.s       .nosetupbuzz

.foundbuzzerC   move.w      #3,52(a1)
                lea.l       INSTCOFFSET(a4),a2
                bra.s       .foundbuzzercom

.foundbuzzerB   move.w      #2,52(a1)
                lea.l       INSTBOFFSET(a4),a2
                bra.s       .foundbuzzercom

.foundbuzzerA   move.w      #1,52(a1)
;                lea         INSTAOFFSET(a4),a2             ; ggn
                lea         (a4),a2
                ;bra.s      .foundbuzzercom

.foundbuzzercom bsr         getbuzzfreq                     ; set buzzer frequency
.nosetupbuzz


; ......................
; setup YM channel MACRO    \1     \2      \3      \4        \5      \6   \7
setupYMchan_MAC macro       ymchan,maskbit,maskreg,intvector,divider,data,timerletter

; .............
; setup YM freq
                tst.b       32+2+\1*4(a1)                   ;test YM volume
                beq.s       .endchfreq

                btst        #\1,28+2(a1)                    ;square wave used?
                bne.s       .endchfreq

                ifeq \1                                     ; ggn
                lea.l       (a4),a2                         ; ggn
                else                                        ; ggn
                lea.l       \1*128(a4),a2
                endc                                        ; ggn
                bsr         getsqufreq
                move.b      d3,0+2+\1*8(a1)                 ; LSB
                move.w      d3,4+2+\1*8(a1)                 ; MSB (store full 12 bits in YM buffer to make it easier for FM routine to access it)
                bne.s       .endchfreq
                bset        #\1,28+2(a1)                    ; if period is 0, mute square to avoid high pitched whine on emulators
.endchfreq

; .............
; setup timers
                IFNE    TIMER_\7

                btst        #2-\1,36(a0)                    ;test if timer enabled
                bne.s       .timerenabled

.timerdisabled  lea         \1*128+78(a4),a5
                andi.w      #$FFF0,(a5)                     ;remove timer entry in mixer
                bra         .endsetuptimer

.timerenabled   tst.b       32+2+4*\1(a1)                   ;if volume is zero
                beq.s       .masktimer

                move.w      \1*128+78(a4),d0                ;d0 holds mixer
                andi.w      #$000F,d0                       ;d0 holds timer mixer
                beq.s       .masktimer                      ;probably its zero ;)
                add.w       d0,d0
                move.w      .timerjumptab(pc,d0.w),d0
                jmp         .timerjumptab(pc,d0.w)

.timerjumptab:  dc.w        .masktimer-.timerjumptab        ;0

                IFNE    SYNCSQUARE_\7
                dc.w        .yessyncsqu-.timerjumptab       ;1
                ENDC
                IFEQ    SYNCSQUARE_\7
                dc.w        .endsetuptimer-.timerjumptab    ;1
                ENDC

                dc.w        .endsetuptimer-.timerjumptab    ;2
                dc.w        .endsetuptimer-.timerjumptab    ;3
                dc.w        .endsetuptimer-.timerjumptab    ;4

                IFNE    SID_\7
                dc.w        .yessid-.timerjumptab           ;5
                ENDC
                IFEQ    SID_\7
                dc.w        .endsetuptimer-.timerjumptab    ;5
                ENDC

                dc.w        .endsetuptimer-.timerjumptab    ;6
                dc.w        .endsetuptimer-.timerjumptab    ;7
                dc.w        .endsetuptimer-.timerjumptab    ;8

                IFNE    PWM_\7
                dc.w        .yespwm-.timerjumptab           ;9
                ENDC
                IFEQ    PWM_\7
                dc.w        .endsetuptimer-.timerjumptab    ;9
                ENDC

                dc.w        .endsetuptimer-.timerjumptab    ;A

                IFNE    SYNCBUZZER_\7
                dc.w        .yessyncbuzz-.timerjumptab      ;B
                ENDC
                IFEQ    SYNCBUZZER_\7
                dc.w        .endsetuptimer-.timerjumptab    ;B
                ENDC

                IFNE    FMSB_\7
                dc.w        .yesfmsb-.timerjumptab          ;C
                ENDC
                IFEQ    FMSB_\7
                dc.w        .endsetuptimer-.timerjumptab    ;C
                ENDC

                IFNE    DIGIDRUMS_\7
                dc.w        .yesdigi-.timerjumptab          ;D
                ENDC
                IFEQ    DIGIDRUMS_\7
                dc.w        .endsetuptimer-.timerjumptab    ;D
                ENDC

                IFNE    FMB_\7
                dc.w        .yesfmb-.timerjumptab           ;E
                ENDC
                IFEQ    FMB_\7
                dc.w        .endsetuptimer-.timerjumptab    ;E
                ENDC

                IFNE    FM_\7
                dc.w        .yesfm-.timerjumptab            ;F
                ENDC
                IFEQ    FM_\7
                dc.w        .endsetuptimer-.timerjumptab    ;F
                ENDC

;................

.vol0masktimer  move.b      #0,32+2+4*\1(a1)                ;clear volume on channel
.masktimer      bclr        #\2,\3                          ;mask timer
                bra         .endsetuptimer

;................
                IFNE    DIGIDRUMS_\7
.yesdigi        tst.b       \1*128+59(a4)                   ;digi already started?
                bne         .endsetuptimer

                lea         dmasound_setup(pc),a5           ;test if st digidrums are setup
                tst.w       (a5)+                           ;a5 now points to pointers to 4bit digidrums
                beq         .endsetuptimer                  ;if YM digidrums not setup
                tst.l       (a5)
                beq         .endsetuptimer                  ;if no YM digidrums malloc'd

                moveq       #0,d0
                move.b      \1*128+20(a4),d0                ;digi sample
                beq         .endsetuptimer                  ;if digi is 0
                subq.b      #1,d0
                add.w       d0,d0
                add.w       d0,d0
                move.l      (a5,d0.w),a5                    ;a5 points to 4bit digidrum sample

                move.b      2+32+\1*4(a1),d1                ;get YM volume

                move.w      sr,d0
                move.w      #$2700,sr                       ;kill interrupts

                cmpi.b      #$f,d1
                bne.s       .yesdigivol

                move.l      digiinterrupt\7(pc),\4          ;setup digi interrupt
                lea         digiinter\7_SMC+8(pc),a2
                move.l      a5,(a2)                         ;setup sample start address
                bra.s       .settimerusage

.yesdigivol     move.l      digiinterrvol\7(pc),\4          ;setup digi interrupt
                lea         digiinvol\7_SMC+12(pc),a2
                move.l      a5,(a2)                         ;setup sample start address
                move.b      d1,21-12(a2)                    ;setup sample on  volume
                subq.w      #2,d1
                bge.s       .setoffvol
                moveq       #0,d1
.setoffvol      move.b      d1,41-12(a2)                    ;setup sample off volume

.settimerusage  lea         timerusage\7(pc),a2
                move.b      #$D,(a2)

                IFNC    '\7','D'
                move.b      #1,\5                           ;divide by 4
                ENDC
                IFC     '\7','D'
                andi.b      #$F0,\5
                ori.b       #$01,\5
                ENDC

                move.b      \1*128+21(a4),\6                ;setup digirate
                st.b        \1*128+59(a4)                   ;digi started
                bset        #\2,\3                          ;unmask timer

                move.w      d0,sr

                bra         .endsetuptimer
                ENDC

;................
                IFNE    SID_\7
.yessid         ;get YM frequency resister value for timer
                ;use actual YM value for now

                ifeq \1                                     ; ggn
                lea.l       (a4),a2                         ; ggn
                else                                        ; ggn
                lea.l       \1*128(a4),a2
                endc                                        ; ggn
                bsr         gettimfreq                      ;d3 holds timer frequency

                ;lookup MFP divider
                moveq       #0,d0
                moveq       #0,d1
                move.b      \1*128+38(a4),d0                ;timer sequence
                move.b      d0,d6
                lsl.w       #SEQ_LEN_LOG2+1,d0              ;sequence length in words
                lea.l       (a3,d0),a5                      ;a5 points to start of timer seq
                move.b      (SEQ_LENGTH*2)-2(a5),d1         ;d1 timer sequence length
                cmpi.b      #SID_STEPS,d1
                bls.s       .nottrimsid
                moveq       #SID_STEPS,d1
.nottrimsid     move.l      d1,d5                           ;d5 timer sequence length

                lea         timer_div_tab(pc),a2
                move.l      d1,d2
                lsl.w       #5,d2
                add.l       d2,a2                           ;a2 points to correct table row for sequence length

                moveq       #0,d0
.findSIDdiv     cmp.w       (a2)+,d3
                bls.s       .foundSIDdiv
                addq.l      #2,a2
                addq.b      #1,d0
                cmpi.b      #8,d0
                beq         .vol0masktimer                  ;if reached > 7 stop timer interrupt
                bra.s       .findSIDdiv

.foundSIDdiv    ;calculate MFP data register                d0=timer divider
                mulu        #393,d3
                mulu        (a2),d1
                divu        d1,d3
                addq.w      #1,d3
                lsr.w       #1,d3                           ;d3=data

                cmpi.b      #1,d0                           ;do safety check on timer values
                bne.s       .safeSID
                cmpi.b      #24,d3
                blo         .vol0masktimer                  ;timer freq > 25600Hz so unsafe
.safeSID
                ;
                ;setup MFP
                ;
                move.w      sr,d2
                move.w      #$2700,sr                       ;kill interrupts


                move.b      2+32+\1*4(a1),d1
                cmp.b       \1*128+123(a4),d1               ;compare current against old ym vols
                bne.s       .yesSIDmodvol
                cmp.b       \1*128+124(a4),d6               ;compare against old sid timer seq
                beq.s       .noSIDmodvol

.yesSIDmodvol   move.b      d1,\1*128+123(a4)               ;old YM vol
                move.b      d6,\1*128+124(a4)               ;old sid timer seq

                IFNE    INCLUDE_020
                lea         timlength\7_020(pc),a2
                move.b      d5,(a2)
                ENDC

                lea         sidinters\7_SMC(pc),a2
                move.l      a2,d4
                andi.w      #%1111111000000000,d4
                move.l      d4,a2
                lea         sidinterrupts\7+4+2(pc),a6
                subq.b      #1,d5
.setSIDrout     moveq       #0,d4
                move.b      d1,d4                           ;YM channel volume
                sub.w       (a5)+,d4
                bge.s       .nozeroSID
                moveq       #0,d4
.nozeroSID      move.b      d4,4(a2)                        ;setup volume #1
                move.w      (a6),10(a2)                     ;setup next sid #1
                addq.l      #4,a6
                lea.l       16(a2),a2                       ;length of sid
                dbra        d5,.setSIDrout
                move.w      sidinterrupts\7+2(pc),10-16(a2) ;setup next sid #last - restart
.noSIDmodvol

                IFC     '\7','D'
                move.b      \5,d1
                andi.b      #$F0,d1
                or.b        d1,d0                           ; apply timer C divider to d0
                ENDC

                lea         timerusage\7(pc),a2
                tst.b       \1*128+19(a4)                   ; do we need to do sync?
                beq.s       .noSIDsync
                tst.b       \1*128+50(a4)                   ; have we synced?
                beq.s       .forcesyncSID

.noSIDsync      cmpi.b      #5,(a2)
                beq.s       .skipsetvec
                move.b      #5,(a2)
                move.l      sidinterrupts\7(pc),\4          ;setup sid vector
.skipsetvec     move.b      d3,\6                           ;data register
                move.b      d0,\5                           ;divider
                bra.s       .endSID

.forcesyncSID   move.b      #5,(a2)
                move.l      sidinterrupts\7(pc),\4          ;setup sid
                clr.b       \5                              ;stop timer
                move.b      d3,\6                           ;data register
                move.b      d0,\5                           ;divider

.endSID         bset        #\2,\3                          ;unmask timer
                move.w      d2,sr

                bra         .endsetuptimer
                ENDC

;................
                IFNE    SYNCSQUARE_\7
.yessyncsqu     ;get YM frequency register value for timer
                ifeq \1                                     ; ggn
                lea.l       (a4),a2                         ; ggn
                else                                        ; ggn
                lea.l       \1*128(a4),a2
                endc                                        ; ggn
                bsr         gettimfreq                      ;d3 timer frequency

                moveq       #0,d6
                move.w      4+2+\1*8(a1),d6                 ;d6 YM square frequency
                beq         .vol0masktimer

                move.w      d3,d7
                divu        d6,d7                           ; d7/d6 -> d7
                swap        d7
                cmpi.w      #$E,d7
                bge.s       .notreset0
.reset0         moveq       #0,d7
                bra.s       .foundreset
.notreset0      sub.w       d6,d7
                cmpi.w      #-$E,d7
                bge.s       .reset0
                moveq       #YM_FREQ_RESET,d7
.foundreset                                                 ; d7 holds YM frequency reset value

                lea         timer_div_tab(pc),a2
                moveq       #0,d1
.findSYSQdiv    cmp.w       (a2)+,d3
                bls.s       .foundSYSQdiv
                addq.l      #2,a2
                addq.b      #1,d1
                cmpi.b      #8,d1
                beq         .vol0masktimer                  ;if reached > 7 stop timer interrupt
                bra.s       .findSYSQdiv

.foundSYSQdiv   ;calculate MFP data register                d1=timer divider
                moveq       #0,d0
                move.w      (a2),d0
                mulu        #393,d3
                divu        d0,d3
                addq.w      #1,d3
                lsr.w       #1,d3                           ;d3=data

                cmpi.b      #1,d1                           ;do safety check on timer values
                bne.s       .safeSYSQ
                cmpi.b      #24,d3
                blo         .vol0masktimer                  ;timer freq > 25600Hz so unsafe
.safeSYSQ
                moveq       #0,d0
                moveq       #0,d5
                move.b      \1*128+38(a4),d0                ;timer sequence
                lsl.w       #SEQ_LEN_LOG2+1,d0              ;sequence length in words
                lea.l       (a3,d0),a5                      ;a5 points to start of timer seq
                move.b      (SEQ_LENGTH*2)-2(a5),d5         ;d5 timer sequence length
                cmpi.b      #SYNSQU_STEPS,d5
                bls.s       .nottrimSYSQ
                moveq       #SYNSQU_STEPS,d5
.nottrimSYSQ    addq.l      #1,a5

                ;
                ;setup MFP
                move.w      sr,d2
                move.w      #$2700,sr                       ; kill interrupts

                IFNE    INCLUDE_020
                lea         timlength\7_020(pc),a2          ; number of steps in timer waveform
                move.b      d5,(a2)
                ENDC

                lea         synsqint\7_SMC(pc),a2
                move.l      a2,d4
                clr.b       d4
                move.l      d4,a2                           ; a2 points to relocated sync square                SMC routine
                lea         synsqinterupt\7+4+2(pc),a6      ; a6 points to the least significant word of the next sync square routine

                move.l      a0,-(sp)
                lea         fm_trans_tab(pc),a0

                subq.b      #1,d5
.setSYSQrout    move.l      d6,d4                           ; d6 is YM square frequency
                move.b      (a5),d0                         ; sequence stores transpose amount
                andi.l      #%01111111,d0                   ; 0->127
                beq.s       .noSYSQtrans
                add.w       d0,d0
                mulu        (a0,d0.w),d4
                swap        d4
                ;lsr.w       d0,d4
.noSYSQtrans    move.b      d7,14(a2)                       ; squarewave reset value
                move.b      d4,24(a2)                       ; setup frequency LSB
                move.w      d4,32(a2)                       ; setup frequency MSB
                move.w      (a6),38(a2)                     ; setup next syncsquare routine
                addq.l      #4,a6
                lea.l       44(a2),a2                       ; length of syncsquare routine
                addq.l      #2,a5
                dbra        d5,.setSYSQrout
                move.w      synsqinterupt\7+2(pc),38-44(a2) ; setup next syncsquare #last - restart
                move.l      (sp)+,a0

                IFC     '\7','D'
                move.b      \5,d0
                andi.b      #$F0,d0
                or.b        d0,d1                           ; apply timer C divider to d1
                ENDC

                lea         timerusage\7(pc),a2
                tst.b       \1*128+19(a4)                   ; do we need to do sync?
                beq.s       .noSYSQsync
                tst.b       \1*128+50(a4)                   ; have we synced?
                beq.s       .forcesycSYSQ

.noSYSQsync     cmpi.b      #$F,(a2)
                beq.s       .endSYSQ
                move.b      #$F,(a2)
                move.l      synsqinterupt\7(pc),\4          ;setup sync square

                lea         synsqint\7_SMC(pc),a5
                move.l      a5,d4
                clr.b       d4
                move.l      d4,a5                           ; a5 points to relocated syncsquare SMC routine
                move.l      22(a5),$ffff8800.w              ; we need to set the YM frequency up avoid the start of the sound having the wrong freq
                move.l      30(a5),$ffff8800.w
                bra.s       .endSYSQ

.forcesycSYSQ   move.b      #$F,(a2)
                move.l      synsqinterupt\7(pc),\4          ;setup interrupt
                lea         synsqint\7_SMC(pc),a5
                move.l      a5,d4
                clr.b       d4
                move.l      d4,a5                           ; a5 points to relocated syncsquare SMC routine
                move.w      4(a5),0+2+\1*8(a1)              ; update YM buffer with first syncsquare freq, later when start sync is done, this will be used
                move.w      12(a5),4+2+\1*8(a1)
                clr.b       \5                              ;stop timer

.endSYSQ        move.b      d3,\6                           ;data register
                move.b      d1,\5                           ;divider

                bset        #\2,\3                          ;unmask timer
                move.w      d2,sr

                bra         .endsetuptimer
                ENDC

;................
                IFNE    SYNCBUZZER_\7
.yessyncbuzz    move.w      52(a1),d0                       ;sync buzzer on this channel?
                cmpi.w      #1+\1,d0
                bne         .endsetuptimer

                ;get YM frequency register value for timer
                ifeq \1                                     ; ggn
                lea.l       (a4),a2                         ; ggn
                else                                        ; ggn
                lea.l        \1*128(a4),a2
                endc                                        ; ggn
                bsr         gettimfreq                      ;d3 holds timer frequency

                lea         timer_div_tab(pc),a2

                moveq       #0,d1
.findSYBUZdiv   cmp.w       (a2)+,d3
                bls.s       .foundSYBUZdiv
                addq.l      #2,a2
                addq.b      #1,d1
                cmpi.b      #8,d1
                beq         .vol0masktimer                  ;if reached > 7 stop timer interrupt
                bra.s       .findSYBUZdiv

.foundSYBUZdiv  ;calculate MFP data register                d1=timer divider
                moveq       #0,d0
                move.w      (a2),d0
                mulu        #393,d3
                divu        d0,d3
                addq.w      #1,d3
                lsr.w       #1,d3                           ;d3=data

                cmpi.b      #1,d1                           ;do safety check on timer values
                bne.s       .safeSYBUZ
                cmpi.b      #24,d3
                blo         .vol0masktimer                  ;timer freq > 25600Hz so unsafe
.safeSYBUZ
                moveq       #0,d0
                moveq       #0,d5
                move.b      \1*128+38(a4),d0                ;timer sequence
                move.b      d0,d6
                lsl.w       #SEQ_LEN_LOG2+1,d0              ;sequence length=win words
                lea.l       (a3,d0),a5                      ;a5 points to start of timer seq
                move.b      (SEQ_LENGTH*2)-2(a5),d5         ;d5 timer sequence length
                cmpi.b      #SYNCBUZZ_STEPS,d5
                bls.s       .nottrimSYBUZ
                moveq       #SYNCBUZZ_STEPS,d5
.nottrimSYBUZ   addq.l      #1,a5

                ;
                ;setup MFP
                move.w      sr,d2
                move.w      #$2700,sr                       ;kill interrupts

                IFNE    INCLUDE_020
                lea         timlength\7_020(pc),a2          ;number of steps in timer waveform
                move.b      d5,(a2)
                ENDC

                lea         synbuzint\7_SMC(pc),a2
                move.l      a2,d4
                clr.b       d4
                move.l      d4,a2
                lea         synbuzinterupt\7+4+2(pc),a6


                cmp.b       \1*128+125(a4),d6
                beq.s       .nosetSYBUZrout
                move.b      d6,\1*128+125(a4)
                subq.b      #1,d5
.setSYBUZrout   move.b      (a5),4(a2)                      ;setup buzzwave
                move.w      (a6),10(a2)                     ;setup next sid
                addq.l      #4,a6
                lea.l       16(a2),a2
                addq.l      #2,a5
                dbra        d5,.setSYBUZrout
                move.w      synbuzinterupt\7+2(pc),10-16(a2);setup next syncbuzz #last - restart
.nosetSYBUZrout

                IFC     '\7','D'
                move.b      \5,d0
                andi.b      #$F0,d0
                or.b        d0,d1                           ; apply timer C divider to d1
                ENDC

                lea         timerusage\7(pc),a2
                tst.b       \1*128+19(a4)                   ; do we need to do sync?
                beq.s       .noSYBUZsync
                tst.b       \1*128+50(a4)                   ; have we synced?
                beq.s       .forcesycSYBUZ

.noSYBUZsync    cmpi.b      #$B,(a2)
                beq.s       .skipsetvSYBUZ
                move.b      #$B,(a2)
                move.l      synbuzinterupt\7(pc),\4         ;setup sync buzzer
.skipsetvSYBUZ  move.b      d3,\6                           ;data register
                move.b      d1,\5                           ;divider
                bra.s       .endSYBUZ

.forcesycSYBUZ  move.b      #$B,(a2)
                move.l      synbuzinterupt\7(pc),\4         ;setup sync buzzer
                clr.b       \5                              ;stop timer
                move.b      d3,\6                           ;data register
                move.b      d1,\5                           ;divider

.endSYBUZ       bset        #\2,\3                          ;unmask timer
                move.w      d2,sr

                bra         .endsetuptimer
                ENDC

;................
                IFNE    FM_\7
.yesfm          ;get YM frequency register value for timer
                ifeq \1                                     ; ggn
                lea.l       (a4),a2                         ; ggn
                else                                        ; ggn
                lea.l        \1*128(a4),a2
                endc                                        ; ggn
                bsr         gettimfreq                      ;d3 holds timer frequency

                lea         timer_div_tab(pc),a2

                moveq       #0,d1
.findFMdiv      cmp.w       (a2)+,d3
                bls.s       .foundFMdiv
                addq.l      #2,a2
                addq.b      #1,d1
                cmpi.b      #8,d1
                beq         .vol0masktimer                  ;if reached > 7 stop timer interrupt
                bra.s       .findFMdiv

.foundFMdiv     ;calculate MFP data register                d1=timer divider
                moveq       #0,d0
                move.w      (a2),d0
                mulu        #393,d3
                divu        d0,d3
                addq.w      #1,d3
                lsr.w       #1,d3                           ;d3=data

                cmpi.b      #1,d1                           ;do safety check on timer values
                bne.s       .safeFM
                cmpi.b      #24,d3
                blo         .vol0masktimer                  ;timer freq > 25600Hz so unsafe
.safeFM
                moveq       #0,d0
                moveq       #0,d5
                move.b      \1*128+38(a4),d0                ;timer sequence
                move.b      d0,d6
                lsl.w       #SEQ_LEN_LOG2+1,d0              ;sequence length in words
                lea.l       (a3,d0),a5                      ;a5 points to start of timer seq
                move.b      (SEQ_LENGTH*2)-2(a5),d5         ;d5 timer sequence length
                cmpi.b      #FM_STEPS,d5
                bls.s       .nottrimFM
                moveq       #FM_STEPS,d5
.nottrimFM      addq.l      #1,a5

                ;
                ;setup MFP
                moveq       #0,d6
                move.w      4+2+\1*8(a1),d6                 ; get YM square frequency in d6
                beq         .vol0masktimer

                move.w      sr,d2
                move.w      #$2700,sr                       ; kill interrupts

                IFNE    INCLUDE_020
                lea         timlength\7_020(pc),a2          ; number of steps in timer waveform
                move.b      d5,(a2)
                ENDC

                lea         fmint\7_SMC(pc),a2
                move.l      a2,d4
                clr.b       d4
                move.l      d4,a2                           ; a2 points to relocated FM SMC routine
                lea         fminterupt\7+4+2(pc),a6         ; a6 points to the least significant word of the next FM routine

                move.l      a0,-(sp)
                lea         fm_trans_tab(pc),a0

                subq.b      #1,d5
.setFMrout      move.l      d6,d4
                move.b      (a5),d0                         ; sequence stores transpose amount
                andi.l      #%01111111,d0                   ; 0->127
                beq.s       .noFMtrans
                add.w       d0,d0
                mulu        (a0,d0.w),d4
                swap        d4
                ;lsr.w       d0,d4
.noFMtrans      move.b      d4,4(a2)                        ; setup frequency LSB
                move.w      d4,12(a2)                       ; setup frequency MSB
                move.w      (a6),18(a2)                     ; setup next FM routine
                addq.l      #4,a6
                lea.l       24(a2),a2                       ; length of FM routine
                addq.l      #2,a5
                dbra        d5,.setFMrout
                move.w      fminterupt\7+2(pc),18-24(a2)    ; setup next FM #last - restart
                move.l      (sp)+,a0

                IFC     '\7','D'
                move.b      \5,d0
                andi.b      #$F0,d0
                or.b        d0,d1                           ; apply timer C divider to d1
                ENDC

                lea         timerusage\7(pc),a2
                tst.b       \1*128+19(a4)                   ; do we need to do sync?
                beq.s       .noFMsync
                tst.b       \1*128+50(a4)                   ; have we synced?
                beq.s       .forcesycFM

.noFMsync       cmpi.b      #$F,(a2)
                beq.s       .endFM
                move.b      #$F,(a2)
                move.l      fminterupt\7(pc),\4             ;setup FM

                lea         fmint\7_SMC(pc),a5
                move.l      a5,d4
                clr.b       d4
                move.l      d4,a5                           ; a5 points to relocated FM SMC routine
                move.l      2(a5),$ffff8800.w               ; we need to set the YM frequency up avoid the start of the sound having the wrong freq
                move.l      10(a5),$ffff8800.w
                bra.s       .endFM

.forcesycFM     move.b      #$F,(a2)
                move.l      fminterupt\7(pc),\4             ;setup interrupt
                lea         fmint\7_SMC(pc),a5
                move.l      a5,d4
                clr.b       d4
                move.l      d4,a5                           ; a5 points to relocated FM SMC routine
                move.w      4(a5),0+2+\1*8(a1)              ; update YM buffer with first FM freq, later when start sync is done, this will be used
                move.w      12(a5),4+2+\1*8(a1)
                clr.b       \5                              ;stop timer

.endFM          move.b      d3,\6                           ;data register
                move.b      d1,\5                           ;divider

                bset        #\2,\3                          ;unmask timer
                move.w      d2,sr

                bra         .endsetuptimer
                ENDC


;................
                IFNE    FMB_\7
.yesfmb         move.w      52(a1),d0                       ;buzzer FM on this channel?
                cmpi.w      #1+\1,d0
                bne         .endsetuptimer

                ;get YM frequency register value for timer
                ifeq \1                                     ; ggn
                lea.l       (a4),a2                         ; ggn
                else                                        ; ggn
                lea.l       \1*128(a4),a2
                endc                                        ; ggn
                bsr         gettimfreq                      ;d3 holds timer frequency
                addq.w      #8,d3                           ;quantise timer frequency to buzzer resolution, including rounding
                andi.w      #%111111111110000,d3            ;this improves the audio quality of the effect

                lea         timer_div_tab(pc),a2

                moveq       #0,d1
.findFMBdiv     cmp.w       (a2)+,d3
                bls.s       .foundFMBdiv
                addq.l      #2,a2
                addq.b      #1,d1
                cmpi.b      #8,d1
                beq         .vol0masktimer                  ;if reached > 7 stop timer interrupt
                bra.s       .findFMBdiv

.foundFMBdiv    ;calculate MFP data register                d1=timer divider
                moveq       #0,d0
                move.w      (a2),d0
                mulu        #393,d3
                divu        d0,d3
                addq.w      #1,d3
                lsr.w       #1,d3                           ;d3=data

                cmpi.b      #1,d1                           ;do safety check on timer values
                bne.s       .safeFMB
                cmpi.b      #24,d3
                blo         .vol0masktimer                  ;timer freq > 25600Hz so unsafe
.safeFMB
                moveq       #0,d0
                moveq       #0,d5
                move.b      \1*128+38(a4),d0                ;timer sequence
                move.b      d0,d6
                lsl.w       #SEQ_LEN_LOG2+1,d0              ;sequence length in words
                lea.l       (a3,d0),a5                      ;a5 points to start of timer seq
                move.b      (SEQ_LENGTH*2)-2(a5),d5         ;d5 timer sequence length
                cmpi.b      #FMB_STEPS,d5
                bls.s       .nottrimFMB
                moveq       #FMB_STEPS,d5
.nottrimFMB     addq.l      #1,a5

                ;
                ;setup MFP
                moveq       #0,d6
                move.w      48+2(a1),d6                     ; get YM buzzer frequency in d6
                beq         .vol0masktimer

                move.w      sr,d2
                move.w      #$2700,sr                       ; kill interrupts

                IFNE    INCLUDE_020
                lea         timlength\7_020(pc),a2          ; number of steps in timer waveform
                move.b      d5,(a2)
                ENDC

                lea         fmbint\7_SMC(pc),a2
                move.l      a2,d4
                clr.b       d4
                move.l      d4,a2                           ; a2 points to relocated FMB SMC routine
                lea         fmbinterupt\7+4+2(pc),a6        ; a6 points to the least significant word of the next FMB routine

                move.l      a0,-(sp)
                lea         fm_trans_tab(pc),a0

                subq.b      #1,d5
.setFMBrout     move.l      d6,d4
                move.b      (a5),d0                         ; sequence stores transpose amount
                andi.l      #%01111111,d0                   ; 0->127
                beq.s       .noFMBtrans
                add.w       d0,d0
                mulu        (a0,d0.w),d4
                swap        d4
.noFMBtrans     move.b      d4,4(a2)                        ; setup frequency LSB
                move.w      d4,12(a2)                       ; setup frequency MSB
                move.w      (a6),18(a2)                     ; setup next FMB routine
                addq.l      #4,a6
                lea.l       24(a2),a2                       ; length of FMB routine
                addq.l      #2,a5
                dbra        d5,.setFMBrout
                move.w      fmbinterupt\7+2(pc),18-24(a2)   ; setup next FMB #last - restart
                move.l      (sp)+,a0

                IFC     '\7','D'
                move.b      \5,d0
                andi.b      #$F0,d0
                or.b        d0,d1                           ; apply timer C divider to d1
                ENDC

                lea         timerusage\7(pc),a2
                tst.b       \1*128+19(a4)                   ; do we need to do sync?
                beq.s       .noFMBsync
                tst.b       \1*128+50(a4)                   ; have we synced?
                beq.s       .forcesycFMB

.noFMBsync      cmpi.b      #$E,(a2)
                beq.s       .endFMB
                bra.s       .setFMBintv

.forcesycFMB    clr.b       \5                              ;stop timer

.setFMBintv     move.b      #$E,(a2)
                move.l      fmbinterupt\7(pc),\4            ;setup FMB
                lea         fmbint\7_SMC(pc),a5
                move.l      a5,d4
                clr.b       d4
                move.l      d4,a5                           ; a5 points to relocated FMB SMC routine
                move.l      2(a5),$ffff8800.w               ; we need to set the YM frequency up avoid the start of the sound having the wrong freq
                move.l      10(a5),$ffff8800.w

.endFMB         move.b      d3,\6                           ;data register
                move.b      d1,\5                           ;divider

                bset        #\2,\3                          ;unmask timer
                move.w      d2,sr

                bra         .endsetuptimer
                ENDC


;................
                IFNE    FMSB_\7
.yesfmsb        move.w      52(a1),d0                       ;syncbuzzer FM on this channel?
                cmpi.w      #1+\1,d0
                bne         .endsetuptimer

                ;get YM frequency register value for timer
                ifeq \1                                     ; ggn
                lea.l       (a4),a2                         ; ggn
                else                                        ; ggn
                lea.l       \1*128(a4),a2
                endc                                        ; ggn
                bsr         gettimfreq                      ;d3 holds timer frequency
                lea         timer_div_tab(pc),a2

                moveq       #0,d1
.findFMSBdiv    cmp.w       (a2)+,d3
                bls.s       .foundFMSBdiv
                addq.l      #2,a2
                addq.b      #1,d1
                cmpi.b      #8,d1
                beq         .vol0masktimer                  ;if reached > 7 stop timer interrupt
                bra.s       .findFMSBdiv

.foundFMSBdiv   ;calculate MFP data register                d1=timer divider
                moveq       #0,d0
                move.w      (a2),d0
                mulu        #393,d3
                divu        d0,d3
                addq.w      #1,d3
                lsr.w       #1,d3                           ;d3=data

                cmpi.b      #1,d1                           ;do safety check on timer values
                bne.s       .safeFMSB
                cmpi.b      #24,d3
                blo         .vol0masktimer                  ;timer freq > 25600Hz so unsafe
.safeFMSB
                moveq       #0,d0
                moveq       #0,d5
                move.b      \1*128+38(a4),d0                ;timer sequence
                move.b      d0,d6
                lsl.w       #SEQ_LEN_LOG2+1,d0              ;sequence length in words
                lea.l       (a3,d0),a5                      ;a5 points to start of timer seq
                move.b      (SEQ_LENGTH*2)-2(a5),d5         ;d5 timer sequence length
                cmpi.b      #FMSB_STEPS,d5
                bls.s       .nottrimFMSB
                moveq       #FMSB_STEPS,d5
.nottrimFMSB

                ;
                ;setup MFP
                moveq       #0,d6
                move.w      48+2(a1),d6                     ; get YM buzzer frequency in d6
                beq         .vol0masktimer

                move.w      sr,d2
                move.w      #$2700,sr                       ; kill interrupts

                IFNE    INCLUDE_020
                lea         timlength\7_020(pc),a2          ; number of steps in timer waveform
                move.b      d5,(a2)
                ENDC

                lea         fmsbint\7_SMC(pc),a2
                move.l      a2,d4
                clr.b       d4
                move.l      d4,a2                           ; a2 points to relocated FMSB SMC routine
                lea         fmsbinterupt\7+4+2(pc),a6       ; a6 points to the least significant word of the next FMSB routine

                move.l      a0,-(sp)
                lea         fm_trans_tab(pc),a0

                subq.b      #1,d5
.setFMSBrout    move.l      d6,d4
                move.b      (a5),d0                         ; sequence stores transpose amount
                andi.l      #%01111111,d0                   ; 0->127
                beq.s       .noFMSBtrans
                add.w       d0,d0
                mulu        (a0,d0.w),d4
                swap        d4
.noFMSBtrans    move.b      1(a5),4(a2)                     ; FMSB waveform
                move.b      d4,12(a2)                       ; setup frequency LSB
                move.w      d4,20(a2)                       ; setup frequency MSB
                move.w      (a6),26(a2)                     ; setup next FMSB routine
                addq.l      #4,a6
                lea.l       32(a2),a2                       ; length of FMSB routine
                addq.l      #2,a5
                dbra        d5,.setFMSBrout
                move.w      fmsbinterupt\7+2(pc),26-32(a2)  ; setup next FMSB #last - restart
                move.l      (sp)+,a0

                IFC     '\7','D'
                move.b      \5,d0
                andi.b      #$F0,d0
                or.b        d0,d1                           ; apply timer C divider to d1
                ENDC

                lea         timerusage\7(pc),a5
                tst.b       \1*128+19(a4)                   ; do we need to do sync?
                beq.s       .noFMSBsync
                tst.b       \1*128+50(a4)                   ; have we synced?
                beq.s       .forcesycFMSB

.noFMSBsync     cmpi.b      #$C,(a5)
                beq.s       .endFMSB
                ;bra.s       .setFMSBintv                   ; always sync the start of syncbuzzer FM, we need to ensure waveform and frequency are correct or the start of the sound is wrong

.forcesycFMSB   clr.b       \5                              ;stop timer

.setFMSBintv    move.b      #$C,(a5)
                move.l      fmsbinterupt\7(pc),\4           ;setup FMSB
                move.l      2-32(a2),$ffff8800.w            ; setup waveform
                move.l      10-32(a2),$ffff8800.w           ; we need to set the YM frequency up avoid the start of the sound having the wrong freq
                move.l      18-32(a2),$ffff8800.w           ; the last stage is used here, as the interrupt will load the first stage, so we don't want that twice

.endFMSB        move.b      d3,\6                           ;data register
                move.b      d1,\5                           ;divider

                bset        #\2,\3                          ;unmask timer
                move.w      d2,sr

                bra         .endsetuptimer
                ENDC


;................
                IFNE    PWM_\7
.yespwm         ;get YM frequency resister value for timer
                ;use actual YM value for now
                ifeq \1                                     ; ggn
                lea.l       (a4),a2                         ; ggn
                else                                        ; ggn
                lea.l       \1*128(a4),a2
                endc                                        ; ggn
                bsr         gettimfreq                      ;d3 holds timer frequency
                sub.w       pulsewidth_tune(pc),d3          ;$5943 = subq.w #4,d3     $5543 = subq.w #2,d3
                move.w      d3,d0                           ;d0 holds timer frequency
                move.w      d3,d7                           ;d7 holds timer frequency

                moveq       #0,d2
                move.b      84(a2),d2                       ; pwm sequence precalc
                moveq       #0,d1
                move.b      17(a2),d1                       ; instrument pulse width
                tst.b       69(a2)
                bne.s       .9ulsecommand
                add.w       96(a2),d1                       ; pwm slide accumulator
.9ulsecommand   add.w       d2,d1
                ble.s       .zeropwm
.positivepwm    cmpi.w      #$FF,d1
                ble.s       .mulpwm
                move.w      #$FF,d1
.mulpwm         mulu.w      d1,d0
                lsr.l       #8,d0

                add.w       d0,d3                           ; include fixed PWM
                sub.w       d0,d7                           ; include fixed PWM
.zeropwm

                move.w      #5102,d0                        ; check timer low frequency limit
                sub.w       d3,d0
                bge.s       .donelowclippwm
                move.w      #5102,d3
                sub.w       d0,d7

.donelowclippwm moveq       #-10,d0                         ; check timer high frequency limit
                add.w       d7,d0
                bge.s       .donehiclippwm
                moveq       #10,d7
                add.w       d0,d3

.donehiclippwm  ;get timer sequence (used for volumes)
                moveq       #0,d0
                move.b      \1*128+38(a4),d0                ;timer sequence
                lsl.w       #SEQ_LEN_LOG2+1,d0              ;sequence length in words
                lea.l       (a3,d0),a5                      ;a5 points to start of timer seq

                ;.........................

                ;lookup MFP divider 1
                lea         timer_div_tab+2*32(pc),a2       ;a2 points to correct table row for sequence length of 2
                moveq       #0,d0
.findPWMdiv1    cmp.w       (a2)+,d3
                bls.s       .foundPWMdiv1
                addq.l      #2,a2
                addq.b      #1,d0
                cmpi.b      #8,d0
                beq         .vol0masktimer                  ;if reached > 7 stop timer interrupt
                bra.s       .findPWMdiv1
.foundPWMdiv1                                               ;d0=timer divider
                ;calculate MFP data register 1
                mulu        #393,d3
                move.w      (a2),d1
                add.w       d1,d1
                divu        d1,d3
                addq.w      #1,d3
                lsr.w       #1,d3                           ;d3=data

                ;do safety check on timer values
                cmpi.b      #1,d0
                bne.s       .safePWM1
                cmpi.b      #24,d3
                blo         .vol0masktimer                  ;timer freq > 25600Hz so unsafe
.safePWM1
                ;.........................

                ;lookup MFP divider 2
                lea         timer_div_tab+2*32(pc),a2       ;a2 points to correct table row for sequence length of 2
                moveq       #0,d5
.findPWMdiv2    cmp.w       (a2)+,d7
                bls.s       .foundPWMdiv2
                addq.l      #2,a2
                addq.b      #1,d5
                cmpi.b      #8,d5
                beq         .vol0masktimer                  ;if reached > 7 stop timer interrupt
                bra.s       .findPWMdiv2
.foundPWMdiv2                                               ;d5=timer divider
                ;calculate MFP data register 2
                mulu        #393,d7
                move.w      (a2),d1
                add.w       d1,d1
                divu        d1,d7
                addq.w      #1,d7
                lsr.w       #1,d7                           ;d7=data

                ;do safety check on timer values
                cmpi.b      #1,d5
                bne.s       .safePWM2
                cmpi.b      #24,d7
                blo         .vol0masktimer                  ;timer freq > 25600Hz so unsafe
.safePWM2
                ;.........................

                ;setup MFP
                ;
                move.w      sr,d2
                move.w      #$2700,sr                       ;kill interrupts

                moveq       #0,d1
                move.b      2+32+\1*4(a1),d1                ;YM vol

                IFNE    INCLUDE_020
                lea         timlength\7_020(pc),a2
                move.b      #2,(a2)
                ENDC

                lea         pwminter\7_SMC1(pc),a2
                move.l      a2,d4
                clr.b       d4
                move.l      d4,a2
                move.b      d7,9(a2)                        ;timer data
                move.b      d5,15(a2)                       ;timer divider
                move.w      d1,d4                           ;YM channel volume
                sub.w       (a5),d4
                bge.s       .nozeroPWM1
                moveq       #0,d4
.nozeroPWM1     move.b      d4,22(a2)                       ;setup volume

                move.b      d3,9+34(a2)                     ;timer data
                move.b      d0,15+34(a2)                    ;timer divider
                move.w      d1,d4                           ;YM channel volume
                sub.w       2(a5),d4
                bge.s       .nozeroPWM2
                moveq       #0,d4
.nozeroPWM2     move.b      d4,22+34(a2)                    ;setup volume

                IFC     '\7','D'
                move.b      \5,d1
                andi.b      #$F0,d1
                or.b        d1,d0                           ; apply timer C divider to d0
                ENDC

                lea         timerusage\7(pc),a2
                tst.b       \1*128+19(a4)                   ; do we need to do sync?
                beq.s       .noPWMsync
                tst.b       \1*128+50(a4)                   ; have we synced?
                beq.s       .forcesyncPWM

.noPWMsync      cmpi.b      #9,(a2)
                beq.s       .endPWM
                move.b      #9,(a2)
                move.l      pwminterrupts\7(pc),\4          ;setup pwm vector
                move.b      d3,\6                           ;data register
                move.b      d0,\5                           ;divider
                bra.s       .endPWM

.forcesyncPWM   move.b      #9,(a2)
                move.l      pwminterrupts\7(pc),\4          ;setup pwm vector
                clr.b       \5                              ;stop timer
                move.b      d3,\6                           ;data register
                move.b      d0,\5                           ;divider

.endPWM         bset        #\2,\3                          ;unmask timer
                move.w      d2,sr

                bra.s       .endsetuptimer
                ENDC

;................
.yesothereffect nop
;................
.endsetuptimer
                ENDC
                endm


                                  ; /1/2/3          /4     /5          /6          /7
set_chan_A:     setupYMchan_MAC     0,5,$FFFFFA13.w,$134.w,$FFFFFA19.w,$FFFFFA1F.w,A
set_chan_B:     setupYMchan_MAC     1,0,$FFFFFA13.w,$120.w,$FFFFFA1B.w,$FFFFFA21.w,B
set_chan_D:     setupYMchan_MAC     2,4,$FFFFFA15.w,$110.w,$FFFFFA1D.w,$FFFFFA25.w,D


; ................
; Do YM write
                move.w      #$ffff8800,a5                   ;small optimisation
                move.w      sr,d1
                move.w      #$2700,sr                       ;interrupts off

                tst.b       INSTAOFFSET+19(a4)              ; do we need to do sync A ?
                beq.s       .nochAsync
                tst.b       INSTAOFFSET+50(a4)              ; have we synced A?
                bne.s       .nochAsync
                move.l      #$01000000,(a5)                 ; sync A
                nop
                move.l      #$00000000+(256*YM_FREQ_RESET),(a5)
                nop
                move.l      (a1)+,(a5)                      ;fA
                move.l      (a1)+,(a5)                      ;fA
                bra.s       .nofmA
.nochAsync      move.w      INSTAOFFSET+78(a4),d0
                andi.w      #$000F,d0
                cmpi.b      #$F,d0                          ;test FM
                beq.s       .fmA
                cmpi.b      #$1,d0                          ;test syncsquare FM
                beq.s       .fmA
                move.l      (a1)+,(a5)                      ;fA
                move.l      (a1)+,(a5)                      ;fA
                bra.s       .nofmA
.fmA            addq.l      #8,a1
.nofmA

                tst.b       INSTBOFFSET+19(a4)              ;do we need to do sync B ?
                beq.s       .nochBsync
                tst.b       INSTBOFFSET+50(a4)              ; have we synced B?
                bne.s       .nochBsync
                move.l      #$03000000,(a5)                 ; sync B
                nop
                move.l      #$02000000+(256*YM_FREQ_RESET),(a5)
                nop
                move.l      (a1)+,(a5)                      ;fB
                move.l      (a1)+,(a5)                      ;fB
                bra.s       .nofmB
.nochBsync      move.w      INSTBOFFSET+78(a4),d0
                andi.w      #$000F,d0
                cmpi.b      #$F,d0                          ;test FM
                beq.s       .fmB
                cmpi.b      #$1,d0                          ;test FM
                beq.s       .fmB
                move.l      (a1)+,(a5)                      ;fB
                move.l      (a1)+,(a5)                      ;fB
                bra.s       .nofmB
.fmB            addq.l      #8,a1
.nofmB

                tst.b       INSTCOFFSET+19(a4)              ; do we need to do sync C ?
                beq.s       .nochCsync
                tst.b       INSTCOFFSET+50(a4)              ; have we synced C?
                bne.s       .nochCsync
                move.l      #$05000000,(a5)                 ; sync C
                nop
                move.l      #$04000000+(256*YM_FREQ_RESET),(a5)
                nop
                move.l      (a1)+,(a5)                      ;fC
                move.l      (a1)+,(a5)                      ;fC
                bra.s       .nofmC
.nochCsync      move.w      INSTCOFFSET+78(a4),d0
                andi.w      #$000F,d0
                cmpi.b      #$F,d0                          ;test FM
                beq.s       .fmC
                cmpi.b      #$1,d0                          ;test FM
                beq.s       .fmC
                move.l      (a1)+,(a5)                      ;fC
                move.l      (a1)+,(a5)                      ;fC
                bra.s       .nofmC
.fmC            addq.l      #8,a1
.nofmC

                move.l      (a1)+,(a5)                      ;fNoise


                move.l      (a1)+,(a5)                      ;Mixer


                tst.b       2(a1)
                beq.s       .skiptovolA
                move.w      INSTAOFFSET+78(a4),d0
                andi.w      #$000F,d0
                cmpi.b      #$D,d0                                  ;test digi
                beq.s       .timerfxA
                cmpi.b      #$5,d0                                  ;test SID
                beq.s       .timerfxA
                cmpi.b      #$9,d0                                  ;test PWM
                beq.s       .timerfxA
.skiptovolA     move.l      (a1)+,(a5)                              ;vA
                bra.s       .endA
.timerfxA       addq.l      #4,a1
.endA

                tst.b       2(a1)
                beq.s       .skiptovolB
                move.w      INSTBOFFSET+78(a4),d0
                andi.w      #$000F,d0
                cmpi.b      #$D,d0                                  ;test digi
                beq.s       .timerfxB
                cmpi.b      #$5,d0                                  ;test SID
                beq.s       .timerfxB
                cmpi.b      #$9,d0                                  ;test PWM
                beq.s       .timerfxB
.skiptovolB     move.l      (a1)+,(a5)                              ;vB
                bra.s       .endB
.timerfxB       addq.l      #4,a1
.endB

                tst.b       2(a1)
                beq.s       .skiptovolC
                move.w      INSTCOFFSET+78(a4),d0
                andi.w      #$000F,d0
                cmpi.b      #$D,d0                                  ;test digi
                beq.s       .timerfxD
                cmpi.b      #$5,d0                                  ;test SID
                beq.s       .timerfxD
                cmpi.b      #$9,d0                                  ;test PWM
                beq.s       .timerfxD
.skiptovolC     move.l      (a1)+,(a5)                              ;vC
                bra.s       .endC
.timerfxD       addq.l      #4,a1
.endC

                move.w      8(a1),d2
                beq.s       .endsetupbuzz                           ;test if buzzer used

                subq.w      #1,d2
                lsl.w       #7,d2                                   ; *128 bytes per instrument buffer
                move.w      78(a4,d2.w),d0
                move.w      d0,d3
                andi.w      #$000F,d0                               ;d0 holds timer mixer nibble

                cmpi.b      #$E,d0                                  ;d0 holds timer mixer nibble
                beq.s       .buzzerFm
                cmpi.b      #$C,d0                                  ;d0 holds timer mixer nibble
                beq.s       .buzzerFm
                move.l      (a1)+,(a5)                              ;fBuzz
                move.l      (a1)+,(a5)                              ;fBuzz
                bra.s       .testSyncBuzz
.buzzerFm       addq.l      #8,a1


.testSyncBuzz   cmpi.b      #$B,d0                                  ;d0 holds timer mixer nibble
                beq.s       .endsetupbuzz                           ;if syncbuzz dont touch the buzzer wave
                cmpi.b      #$C,d0                                  ;d0 holds timer mixer nibble
                beq.s       .endsetupbuzz                           ;if syncbuzz FM dont touch the buzzer wave

.noTestSyncBuzz move.b      #$D,(a5)                                ; select buzzer waveform on YM
                move.b      18(a4,d2.w),d0                          ; new buzzer wave in d0
                lsr.b       #4,d3                                   ; d3 holds buzzer from timer sequence
                cmpi.b      #1,d3                                   ; if 1, use instrument panel buzzer waveform
                beq.s       .comparewave
                move.b      d3,d0                                   ; otherwise we take the wave from the mixer sequence
.comparewave    cmp.b       (a5),d0                                 ; compare buzzer current
                bne.s       .setupwave                              ; if different setup wave
                tst.b       50(a4,d2.w)                             ; if not different then
                bne.s       .endsetupbuzz                           ; is buzzer waveform setup?
                tst.b       19(a4,d2.w)                             ; do we need buzzer start sync?
                beq.s       .endsetupbuzz                           ; if yes, go ahead and set up waveform
.setupwave      move.b      d0,2(a5)                                ;
.endsetupbuzz

                move.w      d1,sr                                   ;restore interrupts


; ................
; set synced flags
                st.b        INSTAOFFSET+50(a4)
                st.b        INSTBOFFSET+50(a4)
                st.b        INSTCOFFSET+50(a4)


; ..............................
; update portamento, PWM and slide accumulators
;                lea         INSTAOFFSET(a4),a2                      ;A
                lea         (a4),a2                                 ;A (ggn)
                bsr         adjportslidacc

                lea.l       INSTBOFFSET(a4),a2                      ;B
                bsr         adjportslidacc

                lea         INSTCOFFSET(a4),a2                      ;C
                bsr         adjportslidacc


; .............................................
; setup STe DMA
                IFNE    STE_DMA
                move.w      dmasound_setup(pc),d0
                cmpi.w      #1,d0
                bls         stedma_end                              ;no dma sound initialised (dma off or plain ST)
                cmpi.b      #3,d0
                beq         stedma_1                                ;if falcon, dont do STe native rate stuff
                cmpi.b      #3,49(a0)                               ;test if ste native rate mode
                blo         stedma_1

stedma_native:  tst.b       8+INSTDOFFSET(a4)                       ;do we think sound is playing?
                bne         stedma_end                              ;yes - dont do anything

                bclr        #0,$ffff8901.w                          ;stop dma
                move.b      1+INSTDOFFSET(a4),d0                    ;d0 holds sample number
                beq         stedma_end                              ;if sample zero, play nothing
                subq.b      #1,d0

                andi.b      #%11111100,$ffff8921.w                  ;reset freq to 6khz
                move.b      0+INSTDOFFSET(a4),d2                    ;d2 holds note
                cmpi.b      #24,d2
                blo         stedma_end                              ;if note below range
                IFEQ    BUILD_BIN
                add.b       22+INSTDOFFSET(a4),d2                   ;dma track transpose
                ENDC
                cmpi.b      #24+12,d2                               ;octave 1=6khz 2=12khz 3=25khz 4=50khz
                blo.s       .freqset
                cmpi.b      #24+12+12,d2
                blo.s       .twelve
                cmpi.b      #24+12+12+12,d2
                blo.s       .twentyfive

.fifty          ori.b       #%11,$ffff8921.w                        ;50066Hz
                bra.s       .freqset
.twentyfive     ori.b       #%10,$ffff8921.w                        ;25033Hz
                bra.s       .freqset
.twelve         ori.b       #%01,$ffff8921.w                        ;12517Hz
                                                                    ; 6258Hz
.freqset
                IFNE    STE_DMA_VOLUME
                moveq       #0,d1
                move.b      2+INSTDOFFSET(a4),d1                    ;d1 volume
                add.b       24+INSTDOFFSET(a4),d1                   ;track volume
                cmp.b       56(a0),d1                               ;check against ste dma volume
                bgt         stedma_end
                lsl.w       #5,d1                                   ;8 long sample pointers
                ENDC

                lea         preshiftedsamp(pc),a6
                add.w       d0,d0
                add.w       d0,d0
                add.l       d0,a6
                IFNE    STE_DMA_VOLUME
                add.l       d1,a6
                ENDC
                move.l      (a6),d0                                 ;d0 start
                move.l      4(a6),d1                                ;d1 end
                cmp.l       d0,d1
                beq         stedma_end                              ;if sample length 0, play nothing

                addq.l      #1,d0                                   ;ensure start address is rounded up
                swap        d0
                move.b      d0,$ffff8903.w                          ;upper byte first
                swap        d0
                move.b      d0,$ffff8907.w
                lsr.w       #8,d0
                move.b      d0,$ffff8905.w                          ;buffer start

                subq.l      #1,d1                                   ;ensure start address is rounded down
                swap        d1
                move.b      d1,$ffff890F.w                          ;upper byte first
                swap        d1
                move.b      d1,$ffff8913.w
                lsr.w       #8,d1
                move.b      d1,$ffff8911.w                          ;buffer end

                bset        #0,$ffff8901.w                          ;start dma
                st.b        8+INSTDOFFSET(a4)                       ;playing sound
                bra         stedma_end


stedma_1:       move.b      8+INSTDOFFSET(a4),d1                    ;playing byte, 0=not playing, 1=playing lower, 2=playing higher
                beq.s       .dmastart


.dmaupdate      move.l      10+INSTDOFFSET(a4),a6                   ;a6 holds sample pointer

                moveq       #0,d0
                move.b      $ffff8909.w,d0
                swap        d0
                move.b      $ffff890b.w,d0
                lsl.w       #8,d0
                move.b      $ffff890d.w,d0                          ;d0 points to current playing buffer position

                cmp.l       dmabufferpointb(pc),d0                  ;compare to middle position
                blt.s       .dmawrite_hi

.dmawrite_lo    cmpi.b      #1,d1                                   ;did we already write lower buffer? dont do it again
                beq         stedma_2
                move.l      dmabufferpointa(pc),a5                  ;a5 write start
                move.b      #1,8+INSTDOFFSET(a4)                    ;just written lower
                lea         resamplepoints(pc),a2                   ;pointer to resampling routines
                bra         .dmadoresample

.dmawrite_hi    cmpi.b      #2,d1                                   ;did we already write upper buffer? dont do it again
                beq         stedma_2
                move.l      dmabufferpointb(pc),a5                  ;a5 write start
                move.b      #2,8+INSTDOFFSET(a4)                    ;just written upper
                lea         resamplepoints(pc),a2                   ;pointer to resampling routines
                bra.s       .dmadoresample


.dmastart       moveq       #0,d0
                move.b      1+INSTDOFFSET(a4),d0                    ;d0 holds sample number
                beq         stedma_2                                ;if sample zero, play nothing
                subq.b      #1,d0

                lea         .dmastart(pc),a6
                add.l       #voicedatapoint-.dmastart,a6
                add.l       (a6),a6
                move.l      a6,a2
                lsl.w       #2,d0
                add.l       d0,a6
                add.l       (a6),a6                                 ;a6 holds sample pointer (source start)

                IFEQ        BUILD_BIN
                add.l       #digiparams-digi0point,a2               ; a2 points to digiparams
                ELSE
                lea         10*4+64*$20(a2),a2                      ; a2 points to digiparams
                ENDC
                move.w      (a2,d0.w),14+INSTDOFFSET(a4)            ;sample length to play
                beq         .yessampleend                           ;if zero, dont play sample

                cmpi.b      #1,9+INSTDOFFSET(a4)                    ;see what dma channel 2 is doing
                bhs.s       .dma2playing
.dma2stopped    bclr        #0,$ffff8901.w                          ;restart dma
                move.l      dmabufferpointa(pc),a5                  ;a5 points to buffer start  (destination start point)
                move.b      #1,8+INSTDOFFSET(a4)                    ;set channel 1 status as just written to lower
                lea         resamplepoints(pc),a2                   ;pointer to resampling routines
                bra.s       .dmadoresample

.dma2playing    bne.s       .dma2playinghi
.dma2playinglo  move.l      dmabufferpointb(pc),a5                  ;a5 points to buffer start  (destination start point)
                move.b      #2,8+INSTDOFFSET(a4)                    ;set channel 1 status as just written to upper
                lea         resamplepoints(pc),a2                   ;pointer to resampling routines
                bra.s       .dmadoresample
.dma2playinghi  move.l      dmabufferpointa(pc),a5                  ;a5 points to buffer start  (destination start point)
                move.b      #1,8+INSTDOFFSET(a4)                    ;set channel 1 status as just written to lower
                lea         resamplepoints(pc),a2                   ;pointer to resampling routines


.dmadoresample  moveq       #0,d2
                move.b      2+INSTDOFFSET(a4),d2                    ;d2 volume
                add.b       24+INSTDOFFSET(a4),d2                   ;track volume
                cmp.b       56(a0),d2                               ;check ste dma volume
                bhi.s       .yessampleend

                cmpi.b      #2,49(a0)
                bne.s       .singledmaonly
                addq.b      #1,d2                                   ;prevent overflow when mixing
.singledmaonly  cmpi.b      #8,d2                                   ;for safety and saving cpu
                bhs.s       .yessampleend

                moveq       #0,d6
                move.b      0+INSTDOFFSET(a4),d6                    ;d6 holds note
                IFEQ    BUILD_BIN
                add.b       22+INSTDOFFSET(a4),d6                   ;dma track transpose
                cmpi.b      #68,d6
                blo.s       .note1notclip
                move.b      #67,d6
                ENDC
.note1notclip   subi.w      #24,d6
                blt.s       .yessampleend                           ;if note below range

                add.w       d6,d6

                lea         dma_freq_tab(pc),a1
                cmpi.b      #3,49(a0)
                bne.s       .notnative
                lea         ste_freq_tab(pc),a1
.notnative      moveq       #0,d0
                move.w      (a1,d6.w),d0                            ;d0.l holds note frequency
                move.l      dmabufferpointb(pc),d1
                sub.l       dmabufferpointa(pc),d1                  ;d1.l holds length of dma buffer
                btst        #6,$ffff8921.w                          ;16 bit sound system?
                beq.s       .not16_1
                lsr.l       #2,d1
.not16_1        mulu        d1,d0
                divu        dmamixrate(pc),d0                       ;d0.w holds number of bytes from sample to be played in the next buffer
                sub.w       d0,14+INSTDOFFSET(a4)                   ;update sample length to play
                bmi.s       .yessampleend                           ;if it would go past the end of the sample - stop sound

                add.w       d6,d6
                move.l      (a2,d6.w),a2                            ;a2 points to correct resample/mix routine pointer

                moveq       #0,d1                                   ;clear data
                lea         eight_2_sixteen(pc),a3
                jsr         (a2)                                    ;a6=source, a5=destination, a3=8 to 16bit lut , d2=volume

                move.l      a6,10+INSTDOFFSET(a4)                   ;store current sample pointer
                bra.s       stedma_2


.yessampleend   clr.b       1+INSTDOFFSET(a4)                       ;clear instrument number
                clr.b       8+INSTDOFFSET(a4)                       ;clear playing byte



stedma_2:       cmpi.b      #2,49(a0)                               ; test stedmasound for 2 dma channels
                bne         stedma_teststop

                move.b      8+INSTDOFFSET(a4),d2                    ;other dma channel
                move.b      9+INSTDOFFSET(a4),d1                    ;playing byte, 0=not playing, 1=playing lower, 2=playing higher
                beq.s       .dmastart


.dmaupdate      move.l      16+INSTDOFFSET(a4),a6                   ;a6 holds sample pointer

                lea         resammixpoints(pc),a2                   ;pointer to resampling routines

                cmpi.b      #1,d2
                beq.s       .dmawrite_lo
                bhi.s       .dmawrite_hi
                                                                    ;here we know dma1 is not playing
                lea         resamplepoints(pc),a2                   ;pointer to resampling routines

                moveq       #0,d0
                move.b      $ffff8909.w,d0
                swap        d0
                move.b      $ffff890b.w,d0
                lsl.w       #8,d0
                move.b      $ffff890d.w,d0                          ;d0 points to current playing buffer position

                cmp.l       dmabufferpointb(pc),d0                  ;compare to middle position
                blt.s       .dmawrite_hi

.dmawrite_lo    cmpi.b      #1,d1                                   ;did we already write lower buffer? dont do it again
                beq         stedma_teststop
                move.l      dmabufferpointa(pc),a5                  ;a5 write start
                move.b      #1,9+INSTDOFFSET(a4)                    ;just written lower
                bra         .dmadoresample

.dmawrite_hi    cmpi.b      #2,d1                                   ;did we already write upper buffer? dont do it again
                beq         stedma_teststop
                move.l      dmabufferpointb(pc),a5                  ;a5 write start
                move.b      #2,9+INSTDOFFSET(a4)                    ;just written upper
                bra.s       .dmadoresample


.dmastart       moveq       #0,d0
                move.b      4+INSTDOFFSET(a4),d0                    ;d0 holds sample number
                beq         stedma_teststop                         ;if sample zero, play nothing
                subq.b      #1,d0

                lea         .dmastart(pc),a6
                add.l       #voicedatapoint-.dmastart,a6
                add.l       (a6),a6
                move.l      a6,a2
                lsl.w       #2,d0
                add.l       d0,a6
                add.l       (a6),a6                                 ;a6 holds sample pointer (source start)

                IFEQ        BUILD_BIN
                add.l       #digiparams-digi0point,a2               ; a2 points to digiparams
                ELSE
                lea         10*4+64*$20(a2),a2                      ; a2 points to digiparams
                ENDC
                move.w      (a2,d0.w),20+INSTDOFFSET(a4)            ;sample length to play
                beq         .yessampleend                           ;if zero, dont play sample

                cmpi.b      #1,d2                                   ;see what dma channel 1 is doing
                bhs.s       .dma1playing
.dma1stopped    bclr        #0,$ffff8901.w                          ;restart dma
                move.l      dmabufferpointa(pc),a5                  ;a5 points to buffer start  (destination start point)
                move.b      #1,9+INSTDOFFSET(a4)                    ;set channel 2 status as just written to lower
                lea         resamplepoints(pc),a2                   ;pointer to resampling routines
                bra.s       .dmadoresample

.dma1playing    bne.s       .dma1playinghi
.dma1playinglo  move.l      dmabufferpointa(pc),a5                  ;a5 points to buffer start  (destination start point)
                move.b      #1,9+INSTDOFFSET(a4)                    ;set channel 2 status as just written to lower
                lea         resammixpoints(pc),a2                   ;pointer to resampling routines
                bra.s       .dmadoresample
.dma1playinghi  move.l      dmabufferpointb(pc),a5                  ;a5 points to buffer start  (destination start point)
                move.b      #2,9+INSTDOFFSET(a4)                    ;set channel 2 status as just written to upper
                lea         resammixpoints(pc),a2                   ;pointer to resampling routines


.dmadoresample  moveq       #0,d2
                move.b      5+INSTDOFFSET(a4),d2                    ;d2 volume
                add.b       25+INSTDOFFSET(a4),d2                   ;track volume
                cmp.b       56(a0),d2                               ;check ste volume
                bhi.s       .yessampleend
                addq.b      #1,d2                                   ;prevent overflow
                cmpi.b      #8,d2                                   ;check ste volume
                bhs.s       .yessampleend

                moveq       #0,d6
                move.b      3+INSTDOFFSET(a4),d6                    ;d6 holds note
                IFEQ    BUILD_BIN
                add.b       23+INSTDOFFSET(a4),d6                   ;dma track transpose
                cmpi.b      #68,d6
                blo.s       .note2notclip
                move.b      #67,d6
                ENDC
.note2notclip   subi.w      #24,d6
                blt.s       .yessampleend                           ;if note below range

                add.w       d6,d6
                lea         dma_freq_tab(pc),a1
                moveq       #0,d0
                move.w      (a1,d6.w),d0                            ;d0.l holds note frequency
                move.l      dmabufferpointb(pc),d1
                sub.l       dmabufferpointa(pc),d1                  ;d1.l holds length of dma buffer
                btst        #6,$ffff8921.w                          ;16bit sound system?
                beq.s       .not16_2
                lsr.l       #2,d1
.not16_2        mulu        d1,d0
                divu        dmamixrate(pc),d0                       ;d0.w holds number of bytes from sample to be played in the next buffer
                sub.w       d0,20+INSTDOFFSET(a4)                   ;update sample length to play
                bmi.s       .yessampleend                           ;if it would go past the end of the sample - stop sound

                add.w       d6,d6
                move.l      (a2,d6.w),a2                            ;a2 points to correct resample/mix routine pointer

                moveq       #0,d1                                   ;clear data
                lea         eight_2_sixteen(pc),a3
                jsr         (a2)                                    ;a6=source, a5=destination, a3=8 to 16bit lut , d2=volume

                move.l      a6,16+INSTDOFFSET(a4)                   ;store current sample pointer
                bra.s       stedma_teststop


.yessampleend   clr.b       4+INSTDOFFSET(a4)                       ;clear instrument number
                clr.b       9+INSTDOFFSET(a4)                       ;clear playing byte



stedma_teststop tst.w       8+INSTDOFFSET(a4)                       ;test if either dma channel is active
                bne.s       .nostopdma
.yesstopdma     bclr        #0,$ffff8901.w                          ;stop dma
                bra.s       stedma_end
.nostopdma      bset        #0,$ffff8901.w                          ;start dma


stedma_end:
                ENDC


                IFEQ    BUILD_BIN
                IFEQ    HALF_MEG
                cmpi.b  #4,49(a0)                                   ;test if midi on ste dma channels
                bne.s   dmamidiout_end
                move.l  #"EDIT",d0                                  ;if in editor and midi output in dma channels
                cmp.l   editortag(pc),d0
                bne.s   dmamidiout_end
                bsr     midinoteout
dmamidiout_end:
                ENDC
                ENDC

;...........................................
; find out if we need to go to the next line
                tst.b       25(a0)                                  ; test playing byte
                beq         .end
                moveq       #0,d0
                addq.b      #1,48(a0)                               ;song speed count
                move.b      48(a0),d0
                cmp.b       13(a0),d0                               ; 13(a0)=song speed
                blo         .end                                    ; we dont need the next line

;......................
; getting the next line
.nextline       clr.b       48(a0)                                  ;clear song speed counter
                addq.b      #1,1(a0)                                ; increase current pattern position
                cmpi.b      #$40,1(a0)                              ; test if end of pattern
                bne         .testscroll
                clr.b       1(a0)                                   ; if the end then reset pattern position
                clr.l       44(a0)                                  ; reset track pattern position offsets

                tst.b       27(a0)                                  ; if in pattern play mode, skip loading new song position
                bne         .nextpatts

;.......................................................
; reached the end of the pattern - get the next song pos
.nextsongpos    moveq       #0,d2
                move.b      38(a0),d2                                ; d2 = next song position

                cmp.b       64+1024(a0),d2                           ; did we reach the end?  64+1024 = song length
                blo.s       .norepeat

.repeat         move.b      64+1025(a0),d2                           ; load repeat position into d2

.norepeat       move.b      d2,(a0)                                  ; update current song position with d2
                move.b      d2,d1
                addq.b      #1,d1
                move.b      d1,38(a0)

                add.w       d2,d2                                    ; 4 bytes per entry
                add.w       d2,d2
                move.l      64(a0,d2.w),d0

                ;tst.b      39(a0)
                ;bne.s      .live
                tst.b       d2                                       ;ignore loop pattern on first song step
                beq.s       .firstlastsongp
                cmp.b       64+1024(a0),d1                           ;ignore loop pattern on last song step
                beq.s       .firstlastsongp

.live           move.l      d0,d1                                    ; check for special loop pattern
                cmpi.b      #$00FD,d1
                beq.s       .foundloop
                andi.w      #$FF00,d1
                cmpi.w      #$FD00,d1
                beq.s       .foundloop
                swap        d1
                cmpi.b      #$00FD,d1
                beq.s       .foundloop
                andi.w      #$FF00,d1
                cmpi.w      #$FD00,d1
                beq.s       .foundloop

.firstlastsongp move.l      d0,4(a0)                                ; update current patterns
                move.l      d0,52(a0)                               ; update next patterns (in case switching to pattern play mode)

                                                                    ; 64(a0)=song data 4(a0)=current patterns
                clr.b       INSTAOFFSET+61(a4)                      ; clear rle counters in case of pattern break
                clr.b       INSTBOFFSET+61(a4)
                clr.b       INSTCOFFSET+61(a4)
                clr.b       INSTDOFFSET+7(a4)
                bra.s       .testscroll

;.................
; handle special loop pattern
.foundloop      tst.b       39(a0)                                  ;test if live mode
                beq.s       .nextsongpos                            ;if not live mode skip to next song step
                lea         64(a0,d2.w),a1                          ;location in song data where we are
                lea         64(a0),a2                               ;start of song data
                moveq       #-3,d1                                  ;$FD.b

.searchforloop  cmp.b       -(a1),d1
                beq.s       .foundnextloop
                cmpa.l      a1,a2
                beq.s       .foundstart
                bra.s       .searchforloop

.foundnextloop  addq.l      #4,a1
.foundstart     sub.l       a2,a1
                move.l      a1,d2
                lsr.w       #2,d2
                cmp.b       (a0),d2                                 ;handle jumping into double repeat case
                bne.s       .norepeat
                bra         .nextsongpos

;.......................................................
; pattern play mode, get next patterns
.nextpatts      tst.b       39(a0)                                  ; test if live mode
                beq.s       .testscroll                             ; if not live mode, dont get new patterns
                move.l      52(a0),4(a0)                            ; update current patterns with next patterns

;.................
;scroll the screen
.testscroll     tst.b       11(a0)                                  ; test if necessary to scroll the pattern
                beq.s       .end
                move.b      1(a0),2(a0)                             ; 1(a0) = current pattern position  2(a0) = current pattern position editor

.end            movem.l     (sp)+,d0-a6
                rts


;..................................................................................
gen_patterntab: movem.l     d0/a0-a2,-(sp)                          ; a0 already points to tracker data

                IFNE    BUILD_BIN
                move.l      a0,a2
                subq.l      #8,a2
                lea         gen_patterntab(pc),a1
                add.l       #trakerdatalen-gen_patterntab,a1
                add.l       (a1),a2                                 ;a2 points to pattern data end
                ENDC

                lea         1090(a0),a0                             ; a0 points to pattern data
                lea         gen_patterntab(pc),a1
                add.l       #pattern_table-gen_patterntab,a1        ; a1 points to pattern table

                IFEQ    BUILD_BIN
                lea         gen_patterntab(pc),a2
                add.l       #voicedatapoint-gen_patterntab,a2       ; a2 points to voicedata
                add.l       (a2),a2                                 ; a2 points to digi0 pointer
                move.l      #"EDIT",d0                              ; if the editor is running, there might be an offset to the start of digi0
                cmp.l       editortag(pc),d0
                beq.s       .editor
.noeditor       add.l       (a2),a2                                 ; a2 points to digi0 start
                subq.l      #8,a2                                   ; a2 points to pattern data end (for variable length pattern data)
                bra.s       .top
.editor         add.l       #trackerdataend-digi0point,a2           ; a2 points to pattern data end (safety in editor)
                ENDC


.top            move.l      a0,(a1)+                                ; store pattern pointer
                moveq       #0,d0                                   ; d0 holds current unpacked length
.scanpattern    addq.b      #1,d0                                   ; add 1 to represent this line
                add.b       7(a0),d0                                ; add rle byte to represent empty lines
                addq.l      #8,a0                                   ; next pattern line
                cmpi.b      #64,d0
                blo.s       .scanpattern

.endofpattern   cmpa.l      a2,a0                                   ; end test
                blo.s       .top

                movem.l     (sp)+,d0/a0-a2
                rts


;..................................................................................
parsetracker:   lea         parsetracker(pc),a2             ; a2 points to pattern table
                add.l       #pattern_table-parsetracker,a2  ; a4 points to instrument buffer
                                                            ; d0 holds pattern number
                                                            ; d1 holds track pattern position
                                                            ; d2 holds channel mute state

                                                            ; length of pattern = 512 bytes
                                                            ; +0 note number              ;+2 volume        ;+4 effect 1 number ;+6 effect 2 number
                                                            ; +1 instrument number        ;+3 effect 1 type ;+5 effect 2 type   ;+7 rle byte
                cmpi.b      #$F0,d0
                blo.s       .notspecial

.special        tst.b       d2                          ;test mute
                bne.s       .emptypatt
                cmpi.b      #$FF,d0
                beq.s       .emptypatt
                cmpi.b      #$FE,d0
                beq.s       .noteoffpatt
.undefined
.emptypatt      move.b      #0,61(a4)                   ;RLE
                rts

.noteoffpatt    tst.b       d1                          ;test if first step in pattern
                bne.s       .emptypatt

                lea         92(a4),a4
                moveq       #0,d0
                moveq       #0,d1
                moveq       #0,d2
                moveq       #0,d3
                moveq       #0,d4
                moveq       #0,d5
                moveq       #0,d6
                moveq       #0,d7
                movea.l     d1,a1
                movea.l     d1,a3
                movea.l     d1,a5
                movea.l     d1,a6
                movem.l     a1/a3/a5/a6/d0-d7,-(a4)     ;clear instrument buffer        48 bytes
                movem.l     a1/a3/a5/a6/d0-d6,-(a4)     ;clear instrument buffer        92 bytes
                move.b      #0,122(a4)                  ;instrument not set

                move.b      #0,61(a4)                   ;RLE
                rts


.notspecial     add.w       d0,d0                       ; length of each pattern table entry = 4 bytes
                add.w       d0,d0
                move.l      (a2,d0.w),a2                ; a2 points to pattern

                lsl.w       #3,d1                       ; each tracker row = 8 bytes
                add.l       d1,a2                       ; a2 points to tracker line

                tst.b       d2                          ; channel mute active?
                bne         .volnotset                  ; only parse tracker codes then

                moveq       #0,d1
                move.b      1(a2),d1                    ; is instrument set?
                beq.s       .instnotset
.instyesset     cmp.b       122(a4),d1
                beq.s       .skipinstset
                move.b      d1,122(a4)

                subq.w      #1,d1
                lsl.w       #6,d1                       ;64 bytes per inst
                lea         .instyesset(pc),a3
                add.l       #voicedatapoint-.instyesset,a3
                add.l       (a3),a3
                lea         56(a3),a3                   ;a3 points to instrument data, skipping names
                add.l       d1,a3

                movem.l     (a3),a1/a3/a5/a6/d0-d7      ;12 registers = 48 bytes
                movem.l     a1/a3/a5/a6/d0-d7,(a4)      ;copy instrument to buffer

.skipinstset    move.w      54(a4),d0                   ;d0 holds portamento accumulators
                swap        d0
                move.w      88(a4),d0                   ;d0 also holds slide accumulators
                lea         92(a4),a4

                moveq       #0,d1
                moveq       #0,d2
                moveq       #0,d3
                moveq       #0,d4
                moveq       #0,d5
                moveq       #0,d6
                moveq       #0,d7
                movea.l     d1,a1
                movea.l     d1,a3
                movea.l     d1,a5
                movea.l     d1,a6
                movem.l     a1/a3/a5/a6/d1-d7,-(a4)     ;clear replay rout specific instrument data 11 reg = 44 bytes 48-92 cleared
                lea         -48(a4),a4

                cmpi.b      #'P',3(a2)
                beq.s       .instyesporta
                cmpi.b      #'P',5(a2)
                beq.s       .instyesporta
                bra.s       .instnotset
.instyesporta   move.w      d0,88(a4)                   ;restore slide and portamento accumulators if portamento
                swap        d0
                move.w      d0,54(a4)


.instnotset     moveq       #0,d1
                move.b      (a2),d1                     ;get note number in d1
                beq.s       .notenotset
                cmpi.b      #1,d1
                beq.s       .noteoffnote
.noteyesset     tst.b       127(a4)                     ;if no current note
                beq.s       .notenotporta               ;dont do portamento
                cmpi.b      #'P',3(a2)
                beq.s       .noteyesporta
                cmpi.b      #'P',5(a2)
                beq.s       .noteyesporta
.notenotporta   move.b      d1,127(a4)                  ;set current note
                bra.s       .notenotset
.noteyesporta   lea         squa_freq_tab(pc),a5
                moveq       #0,d0
                move.b      127(a4),d0                  ;get existing current note
                move.b      d1,127(a4)                  ;set new current note
                add.w       d0,d0
                add.w       d1,d1
                move.w      (a5,d0.w),d0
                sub.w       (a5,d1.w),d0
                add.w       88(a4),d0                   ;include slide accumulator
                add.w       d0,54(a4)                   ;set portamento accumulator
                move.w      #0,88(a4)                   ;clear slide accumulator
                bra.s       .notenotset
.noteoffnote    lea         92(a4),a4

                moveq       #0,d0
                moveq       #0,d1
                moveq       #0,d2
                moveq       #0,d3
                moveq       #0,d4
                moveq       #0,d5
                moveq       #0,d6
                moveq       #0,d7
                movea.l     d1,a1
                movea.l     d1,a3
                movea.l     d1,a5
                movea.l     d1,a6
                movem.l     a1/a3/a5/a6/d0-d7,-(a4)     ;clear instrument buffer        48 bytes
                movem.l     a1/a3/a5/a6/d0-d6,-(a4)     ;clear instrument buffer        92 bytes
                clr.b       122(a4)                     ;instrument not set
                bra.s       .volnotset                  ;test effect codes
.notenotset

                move.b      2(a2),d0
                beq.s       .volnotset
                andi.b      #$F,d0                      ;truncate $10 volume value (shown as '0' on display)
                move.b      d0,51(a4)                   ;volume column
.volnotset

                IFNE    EFFECTCODES
                tst.b       3(a2)                       ;fx 1
                beq.s       .fx1notset
                lea         3(a2),a5
                bsr.s       parsefxcolumn
.fx1notset

                tst.b       5(a2)                       ;fx 2
                beq.s       .fx2notset
                lea         5(a2),a5
                bsr.s       parsefxcolumn
.fx2notset
                ENDC

.end            move.b      7(a2),61(a4)                ;set counter for RLE
                rts

;..................................................................................
                IFNE    EFFECTCODES
parsefxcolumn:  moveq       #0,d0                       ;a5         points to fx code
                move.b      (a5),d0                     ;a4        points to instrument
                cmpi.b      #'Z',d0                     ;test maximum limit
                bhi         .noFX
                tst.b       d2
                beq.s       .dojumptable                ;always do fx if mute off
                                                        ;just do certain fx if mute on
                cmpi.b      #'B',d0                     ; pattern break
                beq.s       .dojumptable
                cmpi.b      #'S',d0                     ; speed
                beq.s       .dojumptable
                cmpi.b      #'U',d0                     ; set microwire
                beq.s       .dojumptable
                cmpi.b      #'Y',d0                     ; dynamic timer
                beq.s       .dojumptable
                cmpi.b      #'Z',d0                     ; demo Zync
                beq.s       .dojumptable
                cmpi.b      #'7',d0                     ; PWM s7ide
                beq.s       .dojumptable
                bra         .noFX
.dojumptable    add.w       d0,d0                           ;*2 (words)
                move.w      .fxjumptable(pc,d0.w),d0
                jmp         parsefxcolumn(pc,d0.w)


.fxjumptable:
                rept        $30
                dc.w        .noFX-parsefxcolumn             ;$00-$2F
                endr
                dc.w        .noFX-parsefxcolumn             ;$30 0

                dc.w        .portaflags-parsefxcolumn       ;$31 1 - portamento flags
                dc.w        .arpeggioflags-parsefxcolumn    ;$32 2 - arpeggio flags
                dc.w        .vibratoflags-parsefxcolumn     ;$33 3 - vibrato flags
                dc.w        .transposeflags-parsefxcolumn   ;$34 4 - transpose flags
                dc.w        .fixfreqflags-parsefxcolumn     ;$35 5 - fix frequency flags
                dc.w        .fixdetuneflags-parsefxcolumn   ;$36 6 - fix detune flags
                dc.w        .pwms7ide-parsefxcolumn         ;$37 7 - pwm slide
                dc.w        .8wmseq-parsefxcolumn           ;$38 8 - pwm seq
                dc.w        .9ulsewidth-parsefxcolumn       ;$39 9 - pulse width

                rept        $7
                dc.w        .noFX-parsefxcolumn             ;$3A-$40
                endr

                dc.w        .Arpeggioseq-parsefxcolumn      ;$41 A - Arpeggio sequence
                dc.w        .patternBreak-parsefxcolumn     ;$42 B - pattern Break
                dc.w        .startsynC-parsefxcolumn        ;$43 C - start synC
                dc.w        .Detunecoarse-parsefxcolumn     ;$44 D - Detune coarse
                dc.w        .dEtunefine-parsefxcolumn       ;$45 E - dEtune fine
                dc.w        .Fixfreqseq-parsefxcolumn       ;$46 F - Fix frequency sequence
                dc.w        .diGisample-parsefxcolumn       ;$47 G - diGi sample
                dc.w        .pitcHslide-parsefxcolumn       ;$48 H - pitcH slide
                dc.w        .tImerseq-parsefxcolumn         ;$49 I - tImer sequence
                dc.w        .noFX-parsefxcolumn             ;$4A J
                dc.w        .noFX-parsefxcolumn             ;$4B K
                dc.w        .voLumeseq-parsefxcolumn        ;$4C L - voLume sequence
                dc.w        .Mixerseq-parsefxcolumn         ;$4D M - Mixer sequence
                dc.w        .Noiseseq-parsefxcolumn         ;$4E N - Noise sequence
                dc.w        .nOisetransp-parsefxcolumn      ;$4F O - nOise transpose
                dc.w        .Portamento-parsefxcolumn       ;$50 P - Portamento
                dc.w        .seQuencespeed-parsefxcolumn    ;$51 Q - seQuence speed
                dc.w        .digiRate-parsefxcolumn         ;$52 R - digi Rate
                dc.w        .songSpeed-parsefxcolumn        ;$53 S - song Speed
                dc.w        .Transpose-parsefxcolumn        ;$54 T - Transpose
                dc.w        .Uwire-parsefxcolumn            ;$55 U - set microwire
                dc.w        .Vibratoseq-parsefxcolumn       ;$56 V - Vibrato sequence
                dc.w        .buzzWave-parsefxcolumn         ;$57 W - buzzer Wave
                dc.w        .eXtraarpeggio-parsefxcolumn    ;$58 X - eXtra pattern arpeggio
                dc.w        .dYnamictimer-parsefxcolumn     ;$59 Y - dYnamic timer allocation
                dc.w        .demoZync-parsefxcolumn         ;$5A Z - demo Zync


.noFX           rts

;................
;other fx

.Transpose      move.b      1(a5),60(a4)                             ;set transpose
                rts

.nOisetransp    move.b      1(a5),48(a4)                             ;set noise transpose
                rts

.Portamento     moveq       #0,d0
                move.b      1(a5),d0
                tst.w       54(a4)                                   ;negative target?
                bpl.s       .posPortamento
                neg.w       d0
.posPortamento  move.w      d0,52(a4)                                ;set portamento rate
                move.w      #0,86(a4)                                ;stop any slide
                rts

.pitcHslide     move.b      1(a5),d0
                ext.w       d0
                neg.w       d0
                move.w      d0,86(a4)                                ;set slide rate
                move.w      #0,52(a4)                                ;stop any portamento
                rts

.pwms7ide       clr.b       69(a4)                                   ;stop ignoring PWM slides
                move.b      1(a5),d0
                cmpi.b      #$80,d0                                  ;780 resets the PWM slide
                bne.s       .notresets7ide
                clr.b       95(a4)                                   ;reset PWM slide rate
                clr.w       96(a4)                                   ;reset PWM slide accumulator
                rts
.notresets7ide  move.b      1(a5),95(a4)
                rts

.songSpeed      btst        #0,31(a0)                                ;midi clock enable
                bne.s       .songSpeedend
                move.b      1(a5),d0
                cmpi.b      #2,d0
                blo.s       .songSpeedend
                move.b      d0,13(a0)
.songSpeedend   rts

.demoZync       lea         demozyncroniser(pc),a6
                move.b      1(a5),(a6)
                rts

.patternBreak   move.b      #$3F,1(a0)                                ;move direct to end of pattern
                rts

.eXtraarpeggio  move.b      1(a5),d0
                move.b      d0,d1
                andi.b      #$F,d0
                lsr.b       #4,d1
                move.b      d1,56(a4)                                ;step 1
                move.b      d0,57(a4)                                ;step 2
                clr.b       58(a4)                                        ;extra arp counter
                bclr        #1,85(a4)                                ;arpeggio sequence no longer over
                rts

.dYnamictimer   move.b      1(a5),d1                                ;d1-new timer mask
                andi.b      #7,d1
                move.b      36(a0),d0                                ;d0-old timer mask
                move.b      d1,36(a0)                                ;update byte

                move.b      d0,d2
                eor.b       d1,d2                                        ;d2 holds changed timers

                IFNE    TIMER_A
                btst        #2,d2
                beq.s       .nochangetimerA                                ;if timer A changed
                btst        #2,d1
                beq.s       .timerArestore
.timerAset      bsr         setuptimerA
                bra.s       .nochangetimerA
.timerArestore  bsr         restoretimerA
.nochangetimerA
                ENDC


                IFNE    TIMER_B
                btst        #1,d2
                beq.s       .nochangetimerB                                ;if timer B changed
                btst        #1,d1
                beq.s       .timerBrestore
.timerBset      bsr         setuptimerB
                bra.s       .nochangetimerB
.timerBrestore  bsr         restoretimerB
.nochangetimerB
                ENDC


                IFNE    TIMER_D
                btst        #0,d2
                beq.s       .nochangetimerD                                ;if timer D changed
                btst        #0,d1
                beq.s       .timerDrestore
.timerDset      bsr         setuptimerD
                bra.s       .nochangetimerD
.timerDrestore  bsr         restoretimerD
.nochangetimerD
                ENDC


                IFNE    TIMER_A+TIMER_B+TIMER_D
                tst.b       d1                                        ;test if necessary to restore timer vector reg and spurious interrupt:
                bne.s       .norestvectspi                                ;if zero now
                tst.b       d0
                beq.s       .norestvectspi                                ;and not zero before
                bsr         rest_vecr_spi
.norestvectspi

                tst.b       d1                                        ;test if necessary to setup timer vector reg and spurious interrupt:
                beq.s       .nosetupvectspi                                ;if non-zero now
                tst.b       d0
                bne.s       .nosetupvectspi                                ;and zero before
                bsr         setup_vecr_spi
.nosetupvectspi
                ENDC
                rts


.Uwire
                IFNE    STE_DMA
                move.w      dmasound_setup(pc),d0
                cmpi.w      #3,d0
                beq         .UwireFalc
                cmpi.w      #2,d0
                bne         .UwireEnd

.UwireSTe       moveq       #0,d0
                move.b      1(a5),d0
                cmpi.b      #$7F,d0
                bhi.s       .UwireVolSTe

.UwireEQ        cmpi.b      #$1F,d0                        ; test if bass or treble?
                bhi         .UwireEnd
                cmpi.b      #$0F,d0
                bhi.s       .UwireTreble
.UwireBass      cmpi.b      #$0C,d0                        ;bass $00 -> $0C
                bhi         .UwireEnd
                andi.w      #%00000001111,d0
                ori.w       #%10001000000,d0
                bsr         set_LMC1992
                bra         .UwireEnd
.UwireTreble    cmpi.b      #$1C,d0                        ;treble $10 -> $1C
                bhi         .UwireEnd
                andi.w      #%00000001111,d0
                ori.w       #%10010000000,d0
                bsr         set_LMC1992
                bra         .UwireEnd

.UwireVolSTe    cmpi.b      #$BF,d0                        ; test if pan or master volume
                bhi.s       .UwirePanSTe
.UwireMvolSTe   cmpi.b      #$A0,d0                        ; master volume $80 -> $A0
                bhi         .UwireEnd
                addq.w      #8,d0                        ; get to maximum possible volume
                andi.w      #%00000111111,d0
                ori.w       #%10011000000,d0
                bsr         set_LMC1992
                bra         .UwireEnd

.UwirePanSTe    cmpi.b      #$E0,d0                        ; pan position $C0 -> $D0 -> $E0
                bhi         .UwireEnd
                move.l      d0,d1
                cmpi.b      #$D0,d0
                bhs.s       .UwirePanRSTe
.UwirePanLSTe   move.w      #%10101010100,d0                ; left vol max
                bsr         set_LMC1992
                subi.w      #$D0,d1
                addi.w      #20,d1
                move.w      d1,d0
                andi.w      #%00000111111,d0
                ori.w       #%10100000000,d0
                bsr         set_LMC1992
                bra.s       .UwireEnd
.UwirePanRSTe   move.w      #%10100010100,d0                ; right vol max
                bsr         set_LMC1992
                subi.w      #$D0,d1
                neg.w       d1
                addi.w      #20,d1
                move.w      d1,d0
                andi.w      #%00000111111,d0
                ori.w       #%10101000000,d0
                bsr         set_LMC1992
                bra.s       .UwireEnd

.UwireFalc      moveq       #0,d0
                move.b      1(a5),d0
                lea         f030_Uwire_emu(pc),a5
                cmpi.b      #$80,d0
                blo.s       .UwireEnd
                cmpi.b      #$A0,d0
                bhi.s       .UwirePanFalc

.UwireMvolFalc  subi.w      #$a0,d0                        ; master vol $80 -> $A0
                neg.w       d0                        ; d0 has LUT offset
                move.b      d0,(a5)
                bsr         fake_LMC1992
                bra.s       .UwireEnd

.UwirePanFalc   cmpi.b      #$C0,d0
                blo.s       .UwireEnd
                cmpi.b      #$E0,d0
                bhi.s       .UwireEnd
                cmpi.b      #$D0,d0                        ; pan position $C0 -> $D0 -> $E0
                bhs.s       .UwirePanRFalc
.UwirePanLFalc  clr.b       1(a5)                        ; left vol max
                subi.w      #$D0,d0
                neg.w       d0
                move.b      d0,2(a5)                ; set right vol
                bsr         fake_LMC1992
                bra.s       .UwireEnd
.UwirePanRFalc  clr.b       2(a5)                        ; right vol max
                subi.w      #$D0,d0
                move.b      d0,1(a5)                ; set left vol
                bsr         fake_LMC1992
                ;bra.s      .UwireEnd

                ENDC
.UwireEnd       rts



;................
;instrument fx

.buzzWave       move.b      1(a5),18(a4)                                ;set buzzer waveform
                andi.b      #$F,18(a4)
                clr.b       50(a4)                                        ;need to setup again
                clr.b       122(a4)                                        ;instrument modified
                rts
.seQuencespeed  move.b      1(a5),16(a4)
                clr.b       122(a4)                                        ;instrument modified
                rts
.startsynC      move.b      1(a5),19(a4)
                clr.b       50(a4)
                clr.b       122(a4)                                        ;instrument modified
                rts
.diGisample     move.b      1(a5),d0
                cmpi.b      #8,d0
                bhi.s       .enddigisamp
                move.b      d0,20(a4)
                clr.b       122(a4)                                        ;instrument modified
.enddigisamp    rts
.digiRate       move.b      1(a5),d0
                cmpi.b      #$CD,d0
                bhi.s       .enddigirate
                cmpi.b      #$18,d0
                blo.s       .enddigirate
                move.b      d0,21(a4)
                clr.b       122(a4)                                        ;instrument modified
.enddigirate    rts
.Detunecoarse   move.b      1(a5),23(a4)
                clr.b       122(a4)                                        ;instrument modified
                rts
.dEtunefine     move.b      1(a5),24(a4)
                clr.b       122(a4)                                        ;instrument modified
                rts
.9ulsewidth     move.b      1(a5),17(a4)
                st.b        69(a4)                                         ;9ulse FX code is now set! ignore PWM slides
                ;clr.b       95(a4)                                         ;reset PWM slide rate
                ;clr.w       96(a4)                                         ;reset PWM slide accumulator
                clr.b       122(a4)                                        ;instrument modified
                rts

;................
;flag fx

.portaflags     move.b      1(a5),(a4)
                clr.b       122(a4)                                        ;instrument modified
                rts
.arpeggioflags  move.b      1(a5),1(a4)
                clr.b       122(a4)                                        ;instrument modified
                bclr        #1,85(a4)                                      ; do sequence
                rts
.vibratoflags   move.b      1(a5),2(a4)
                clr.b       122(a4)                                        ;instrument modified
                bclr        #2,85(a4)                                ; do sequence
                rts
.transposeflags move.b      1(a5),3(a4)
                clr.b       122(a4)                                        ;instrument modified
                rts
.fixfreqflags   move.b      1(a5),4(a4)
                clr.b       122(a4)                                        ;instrument modified
                bclr        #5,85(a4)                                ; do sequence
                rts
.fixdetuneflags move.b      1(a5),5(a4)
                clr.b       122(a4)                                        ;instrument modified
                rts

;................
;sequence fx

.voLumeseq      move.b      1(a5),32(a4)                ;set sequence
                clr.b       62(a4)                        ;clear counter
                bclr        #0,85(a4)                ;clear sequence completed
                clr.b       122(a4)                        ;instrument modified
                rts
.Arpeggioseq    move.b      1(a5),33(a4)
                clr.b       63(a4)
                bclr        #1,85(a4)
                clr.b       122(a4)
                rts
.Vibratoseq     move.b      1(a5),34(a4)
                clr.b       64(a4)
                bclr        #2,85(a4)
                clr.b       122(a4)
                rts
.Mixerseq       move.b      1(a5),35(a4)
                clr.b       65(a4)
                bclr        #3,85(a4)
                clr.b       122(a4)
                rts
.Noiseseq       move.b      1(a5),36(a4)
                clr.b       66(a4)
                bclr        #4,85(a4)
                clr.b       122(a4)
                rts
.Fixfreqseq     move.b      1(a5),37(a4)
                clr.b       67(a4)
                bclr        #5,85(a4)
                clr.b       122(a4)
                rts
.tImerseq       move.b      1(a5),38(a4)                ;no counter to clear or sequence to complete
                clr.b       122(a4)
                rts
.8wmseq         move.b      1(a5),39(a4)
                clr.b       68(a4)
                bclr        #6,85(a4)
                clr.b       122(a4)
                rts

                ENDC


;..................................................................................
                ;IFNE   STE_DMA
parsetrackste:  lea         parsetrackste(pc),a2            ; a2 points to pattern table
                add.l       #pattern_table-parsetrackste,a2 ; a4 points to instrument buffer
                                                            ; d0 holds pattern number
                                                            ; d1 holds track pattern position
                                                            ; d2 holds track mute enabled

                                                            ; length of pattern = 512 bytes
                                                            ; +0 note 1        ; +2 volume 1    ;+4                ;+6 reserved
                                                            ; +1 instr 1       ;                ;+5                ;+7 rle byte

                cmpi.b      #$F0,d0
                blo.s       .notspecial

.special        cmpi.b      #$FF,d0
                beq.s       .emptypatt
                cmpi.b      #$FE,d0
                beq.s       .noteoffpatt
.undefined
.emptypatt      move.b      #0,7(a4)        ;RLE
                rts

.noteoffpatt    tst.b       d1                ;test if first step in pattern
                bne.s       .emptypatt

                move.b      #1,(a4)                ; store note
                clr.b       1(a4)                ; no instrument ch1
                clr.b       8(a4)                ; not playing ch1
                move.b      #1,3(a4)        ; store note
                clr.b       4(a4)                ; no instrument ch2
                clr.b       9(a4)                ; not playing ch2

                move.b      #0,7(a4)        ;RLE
                rts


.notspecial     add.w       d0,d0                                ; length of each pattern table entry = 4 bytes
                add.w       d0,d0
                move.l      (a2,d0.w),a2                        ; a2 points to pattern

                lsl.w       #3,d1                                ; each tracker row = 8 bytes
                add.l       d1,a2                                ; a2 points to tracker line


                btst        #3,37(a0)                        ; 1st ste channel muted?
                bne.s       .ste1mute

                move.b      1(a2),d0                        ; is instrument set?
                beq.s       .inst1notset
.inst1yesset    cmpi.b      #8,d0                                ; limit maximum instrument number for safety
                bhi.s       .inst1notset
                move.b      d0,1(a4)
                ;clr.b      8(a4)                                ; instrument not playing
                clr.b       2(a4)                                ; reset volume
.inst1notset

                move.b      (a2),d0                                ; is note set?
                beq.s       .note1notset
.note1yesset    cmpi.b      #1,d0
                beq.s       .note1yesoff
                cmpi.b      #4,49(a0)                        ;test if midi on ste dma channels
                beq.s       .note1notclip                        ; don't clip on midi mode
.note1yesclip   cmpi.b      #68,d0
                blo.s       .note1notclip
                subi.b      #12,d0
                bra.s       .note1yesclip
.note1notclip   move.b      d0,(a4)                                ; store note
                clr.b       8(a4)                                ; not playing
                bra.s       .note1notset
.note1yesoff    move.b      d0,(a4)                                ; store note
                clr.b       1(a4)                                ; no instrument
                clr.b       8(a4)                                ; not playing
.note1notset

                move.b      2(a2),d0
                beq.s       .vol1notset
.vol1yesset     andi.b      #$F,d0                                ;truncate $10 volume value (shown as '0' on display)
                move.b      d0,2(a4)                        ;volume column
.vol1notset



.ste1mute       cmpi.b      #2,49(a0)                        ; test if 2nd dma channel enabled
                beq.s       .twoormidi
                cmpi.b      #4,49(a0)                        ; test if midi out on ste channels enabled
                bne.s       .end
.twoormidi      btst        #4,37(a0)                        ; 2nd ste channel muted?
                bne.s       .end

                move.b      4(a2),d0                        ; is instrument set?
                beq.s       .inst2notset
.inst2yesset    cmpi.b      #8,d0                                ; limit maximum instrument number for safety
                bhi.s       .inst2notset
                move.b      d0,4(a4)
                ;clr.b      9(a4)                                ; instrument not playing
                clr.b       5(a4)                                ; reset volume
.inst2notset

                move.b      3(a2),d0                        ; is note set?
                beq.s       .note2notset
.note2yesset    cmpi.b      #1,d0
                beq.s       .note2yesoff
                cmpi.b      #4,49(a0)                        ;test if midi on ste dma channels
                beq.s       .note2notclip                        ; don't clip on midi mode
.note2yesclip   cmpi.b      #68,d0
                blo.s       .note2notclip
                subi.b      #12,d0
                bra.s       .note2yesclip
.note2notclip   move.b      d0,3(a4)                        ; store note
                clr.b       9(a4)                                ; not playing
                bra.s       .note2notset
.note2yesoff    move.b      d0,3(a4)                        ; store note
                clr.b       4(a4)                                ; no instrument
                clr.b       9(a4)                                ; not playing
.note2notset

                move.b      5(a2),d0
                beq.s       .vol2notset
.vol2yesset     andi.b      #$F,d0                                ;truncate $10 volume value (shown as '0' on display)
                move.b      d0,5(a4)                        ;volume column
.vol2notset


.end            move.b      7(a2),7(a4)                        ; set counter for RLE
                rts
                ;ENDC


;..................................................................................
getchvol:       ;check buzzer mixer value and if instrument is set up
                moveq       #0,d0

                move.w      78(a2),d1                           ;d1 holds precalced mixer
                beq.s       .endzero                            ;if nothing enabled, zero volume

                move.b      51(a2),d2                           ;d2 holds volume column

                move.w      72(a2),d0                           ;sequence volume into d0
                beq.s       .endzero

                sub.b       d2,d0                               ;volume column
                sub.b       22(a2),d0                           ;instrument volume
                sub.b       94(a2),d0                           ;track volume
                bgt.s       .notzero                            ;did we go below zero?

.endzero        moveq       #0,d0
                and.w       #$FF0F,78(a2)                       ;clear inaudible buzzer from mixer
                rts

.notzero        andi.w      #$00F0,d1
                beq.s       .end
                moveq       #%00010000,d0                       ;enable buzzer
.end            rts



;..................................................................................
dosequences:    tst.b       127(a2)                                  ;if note not set, dont do sequences
                beq.s       .end

                move.b      49(a2),d0                                ;sequence speed count = 49(a2)
                beq.s       .doadjust


                addq.b      #1,d0
                cmp.b       16(a2),d0                                ;sequence speed = 16(a2)
                blo.s       .endnoadjust
                moveq       #0,d0
.endnoadjust    move.b      d0,49(a2)                                ;update sequence speed counter
.end            rts


.doadjust       addq.b      #1,d0
                cmp.b       16(a2),d0                                ;sequence speed = 16(a2)
                blo.s       .skipclear
                moveq       #0,d0
.skipclear      move.b      d0,49(a2)                                ;update sequence speed counter


;................
;vol...
                btst        #0,85(a2)                                ;volume sequence finished?
                bne.s       .endvol
                moveq       #0,d0
                move.b      32(a2),d0                                ;volume sequence
                beq.s       .nullvol

                lsl.w       #SEQ_LEN_LOG2+1,d0                       ;sequence length in words
                move.b      (2*SEQ_LENGTH)-2(a3,d0.w),d1             ;d1 holds seq length
                moveq       #0,d2
                move.b      62(a2),d2                                ;d2 holds sequence counter

                move.w      d0,d4                                    ;
                add.w       d2,d4                                    ;
                add.w       d2,d4                                    ;
                move.w      (a3,d4.w),72(a2)                         ;store current volume

                addq.b      #1,d2                                    ;increase counter
                cmp.b       d2,d1                                    ;do we need to repeat?
                bhi.s       .setvolcounter

                move.b      (2*SEQ_LENGTH)-1(a3,d0.w),d2             ;use repeat

                subq.b      #1,d1
                cmp.b       d2,d1                                    ;sequence end and repeat at same point?
                beq.s       .nullvol1                                ;mark sequence finished
                bra.s       .setvolcounter

.nullvol        clr.w       72(a2)                                   ;store current volume
.nullvol1       bset        #0,85(a2)

.setvolcounter  move.b      d2,62(a2)
.endvol

;................
;arp...
                btst        #1,85(a2)                                ;arpeggio sequence finished?
                bne         .endarp

                tst.b       1(a2)                                    ;test arpeggio flag
                beq.s       .nullarp

                moveq       #0,d0
                move.b      33(a2),d0                                ;d0 holds arp sequence
                beq.s       .nullarp

                lsl.w       #SEQ_LEN_LOG2+1,d0                       ;sequence length
                move.b      (2*SEQ_LENGTH)-2(a3,d0.w),d1             ;d1 holds seq length
                moveq       #0,d2
                move.b      63(a2),d2                                ;d2 holds seq counter

                move.w      d0,d4
                add.w       d2,d4
                add.w       d2,d4
                move.w      (a3,d4.w),d5                             ;store arpeggio

                addq.b      #1,d2                                    ;increase counter
                cmp.b       d2,d1                                    ;do we need to repeat?
                bhi.s       .setarpcounter

                move.b      (2*SEQ_LENGTH)-1(a3,d0.w),d2             ;d2 use repeat

                subq.b      #1,d1
                cmp.b       d2,d1                                    ;sequence end and repeat at same point?
                beq.s       .nullarp1
                bra.s       .setarpcounter

.nullarp        moveq       #0,d5
.nullarp1       bset        #1,85(a2)                                ;set arpeggio sequence finished

.setarpcounter  move.b      d2,63(a2)



.xtraarp        tst.w       56(a2)                                        ;any extra arpeggio?
                beq.s       .storearp

                bclr        #1,85(a2)                                ;with xtra arp running, sequence can never finish
                cmpi.b      #1,58(a2)                                ;test which step of xtra arpeggio
                beq.s       .step1                                        ;step 1
                bhi.s       .step2                                        ;step 2

.step0          move.b      #1,58(a2)                                ;set next step
                bra.s       .storearp

.step1          moveq       #0,d2
                move.b      56(a2),d2                                ;get step 1 shift
                cmpi.b      #$F,d2                                        ;test for 2 step arpeggio
                beq.s       .step2b
                move.b      #2,58(a2)                                ;set next step
                bra.s       .setxtraarp

.step2          moveq       #0,d2
.step2b         move.b      57(a2),d2                                ;get step2 shift
                clr.b       58(a2)                                        ;set next step

.setxtraarp     add.w       d2,d5                                    ;include caluculated xtra arp
.storearp       move.w      d5,74(a2)                                ;store arpeggio
.endarp

;................
;vib...
                btst        #2,85(a2)                                ;finished vibrato?
                bne.s       .endvib
                tst.b       2(a2)                                    ;test vibrato flags
                beq.s       .nullvib

                moveq       #0,d0
                move.b      34(a2),d0                                ;vib sequence
                beq.s       .nullvib

                lsl.w       #SEQ_LEN_LOG2+1,d0                       ;sequence length
                move.b      (2*SEQ_LENGTH)-2(a3,d0),d1               ;d1 holds seq length
                moveq       #0,d2
                move.b      64(a2),d2

                move.w      d0,d4
                add.w       d2,d4
                add.w       d2,d4
                move.w      (a3,d4.w),76(a2)                         ;store current vibrato

                addq.b      #1,d2                                    ;increase counter
                cmp.b       d2,d1                                    ;do we need to repeat?
                bhi.s       .setvibcounter

                move.b      (2*SEQ_LENGTH)-1(a3,d0),d2               ;d2 use repeat

                subq.b      #1,d1
                cmp.b       d2,d1                                    ;sequence end and repeat at same point?
                beq.s       .nullvib1
                bra.s       .setvibcounter

.nullvib        clr.w       76(a2)
.nullvib1       bset        #2,85(a2)                                ;use repeat

.setvibcounter  move.b      d2,64(a2)
.endvib

;................
;mix...
                btst        #3,85(a2)                                ;mixer finished?
                bne.s       .endmix

                moveq       #0,d0
                move.b      35(a2),d0                                ;mixer counter
                beq.s       .nullmix

                lsl.w       #SEQ_LEN_LOG2+1,d0                       ;sequence length
                move.b      (2*SEQ_LENGTH)-2(a3,d0.w),d1             ;d1 holds seq length
                moveq       #0,d2
                move.b      65(a2),d2

                move.w      d0,d4
                add.w       d2,d4
                add.w       d2,d4
                move.w      (a3,d4.w),78(a2)

                addq.b      #1,d2                                    ;increase counter
                cmp.b       d2,d1                                    ;do we need to repeat?
                bhi.s       .setmixcounter

                move.b      (2*SEQ_LENGTH)-1(a3,d0.w),d2             ;d2 use repeat

                subq.b      #1,d1
                cmp.b       d2,d1                                    ;sequence end and repeat at same point?
                beq.s       .nullmix1
                bra.s       .setmixcounter

.nullmix        clr.w       78(a2)
.nullmix1       bset        #3,85(a2)

.setmixcounter  move.b      d2,65(a2)
.endmix

;................
;noi...
                btst        #4,85(a2)                                ;noise sequence finished?
                bne.s        .endnoi

                moveq       #0,d0
                move.b      36(a2),d0                                ;noise sequence
                beq.s       .nullnoi

                lsl.w       #SEQ_LEN_LOG2+1,d0                       ;length of sequence data structure
                move.b      (2*SEQ_LENGTH)-2(a3,d0.w),d1             ;d1 holds seq length
                moveq       #0,d2
                move.b      66(a2),d2

                move.w      d0,d4
                add.w       d2,d4
                add.w       d2,d4
                move.w      (a3,d4.w),80(a2)

                addq.b      #1,d2                                    ;increase counter
                cmp.b       d2,d1                                    ;do we need to repeat?
                bhi.s       .setnoicounter

                move.b      (2*SEQ_LENGTH)-1(a3,d0),d2               ;d2 use repeat

                subq.b      #1,d1
                cmp.b       d2,d1                                    ;sequence end and repeat at same point?
                beq.s       .nullnoi1
                bra.s       .setnoicounter

.nullnoi        clr.w       80(a2)
.nullnoi1       bset        #4,85(a2)                                ;finished noise sequence

.setnoicounter  move.b      d2,66(a2)
.endnoi

;................
;fix...
                btst        #5,85(a2)                                ;fix sequence finished?
                bne.s       .endfix
                tst.b       4(a2)                                    ;test fixed freq flag
                beq.s       .nullfix

                moveq       #0,d0
                move.b      37(a2),d0                                ;increase fix freq counter
                beq.s       .nullfix

                lsl.w       #SEQ_LEN_LOG2+1,d0                       ;64 bytes per seq
                move.b      (2*SEQ_LENGTH)-2(a3,d0),d1               ;d1 holds seq length
                moveq       #0,d2
                move.b      67(a2),d2

                move.w      d0,d4
                add.w       d2,d4
                add.w       d2,d4
                move.w      (a3,d4.w),82(a2)

                addq.b      #1,d2                                    ;increase counter
                cmp.b       d2,d1                                    ;do we need to repeat?
                bhi.s       .setfixcounter

                move.b      (2*SEQ_LENGTH)-1(a3,d0),d2               ;d2 use repeat

                subq.b      #1,d1
                cmp.b       d2,d1                                    ;sequence end and repeat at same point?
                beq.s       .nullfix1
                bra.s       .setfixcounter

.nullfix        clr.w       82(a2)
.nullfix1       bset        #5,85(a2)

.setfixcounter  move.b      d2,67(a2)
.endfix

;................
;pwm...
                btst        #6,85(a2)                                ;pwm sequence finished?
                bne.s       .endpwm

                moveq       #0,d0
                move.b      39(a2),d0                                ;pwm sequence
                beq.s       .nullpwm

                lsl.w       #SEQ_LEN_LOG2+1,d0                       ;length of sequence data structure
                move.b      (2*SEQ_LENGTH)-2(a3,d0.w),d1             ;d1 holds seq length
                moveq       #0,d2
                move.b      68(a2),d2

                move.w      d0,d4
                add.w       d2,d4
                add.w       d2,d4
                move.b      1(a3,d4.w),84(a2)

                addq.b      #1,d2                                    ;increase counter
                cmp.b       d2,d1                                    ;do we need to repeat?
                bhi.s       .setpwmcounter

                move.b      (2*SEQ_LENGTH)-1(a3,d0),d2               ;d2 use repeat

                subq.b      #1,d1
                cmp.b       d2,d1                                    ;sequence end and repeat at same point?
                beq.s       .nullpwm1
                bra.s       .setpwmcounter

.nullpwm        clr.b       84(a2)
.nullpwm1       bset        #6,85(a2)                                ;finished pwm sequence

.setpwmcounter  move.b      d2,68(a2)
.endpwm

;................
                rts

;..................................................................................
; adjust portamento accumulators
adjportslidacc: move.b      95(a2),d0                       ; get PWM s7ide rate
                beq.s       .testporta                      ; slide not set
                ext.w       d0                              ;
                add.w       d0,96(a2)                       ;
                cmpi.w      #$FF,96(a2)
                ble.s       .slidebelowFF
                move.w      #$FF,96(a2)
                bra.s       .testporta
.slidebelowFF   cmpi.w      #-$FF,96(a2)
                bge.s       .testporta
                move.w      #-$FF,96(a2)

.testporta      tst.b       (a2)                            ;are portamentos enabled?
                beq.s       .end                            ;no - then don't calculate

                move.w      86(a2),d0                       ;pitcHslide
                add.w       d0,88(a2)                       ;

                move.w      52(a2),d0                       ;portamento rate
                beq.s       .end                            ;reached end of portamento
                bpl.s       .chportuptest

.chportdwtest   sub.w       d0,54(a2)                       ;update accumulator for next time
                blt.s       .end
                clr.l       52(a2)                          ;clear portamento rate - speed optimisation and indication porta is over
                rts                                         ;clear portamento accumulator

.chportuptest   sub.w       d0,54(a2)                       ;update accumulator for next time
                bgt.s       .end
                clr.l       52(a2)                          ;clear portamento rate - speed optimisation and indication porta is over
.end            rts                                         ;clear portamento accumulator

;..................................................................................
getsqufreq:     moveq       #0,d3

                tst.b       127(a2)                         ;note number
                beq         .end                            ;if 0 then dont set

                btst        #2,4(a2)                        ;fixed freq flag
                beq.s       .nochfix

                btst        #2,5(a2)                        ;test fix detune flag
                beq.s       .fixfreqseq

.fixfreqfix     tst.b       37(a2)                          ;make sure fixed sequence is zero
                bne.s       .fixfreqseq
                move.b      23(a2),d3
                lsl.w       #8,d3
                move.b      24(a2),d3
                bra         .chfixskip

.fixfreqseq     move.w      82(a2),d3                       ;d3 holds current fix freq adjustment
                cmpi.w      #$FFFF,d3                       ;if $FFFF then we ignore the fixed freq
                bne         .chfixskip                      ;in fix frequency mode we ignore arps, transpose and portamento
                moveq       #0,d3

.nochfix        btst        #2,5(a2)                        ;fixed detune flag
                beq.s       .nofixeddetc
                move.b      23(a2),d3                       ;include fixed detune
                ext.w       d3
.nofixeddetc

                btst        #2,1(a2)                        ;arp flag
                beq.s       .nocharp
                add.w       74(a2),d3                       ;add precalculated arpeggio

.nocharp        btst        #2,3(a2)                        ;transpose flag
                beq.s       .nochtransp

                move.b      60(a2),d0
                IFEQ    BUILD_BIN
                add.b       93(a2),d0
                ENDC
                ext.w       d0                              ;down as well as up
                add.w       d0,d3                           ;include transpose
.nochtransp

                moveq       #0,d4
                move.w      d3,d4                           ;d4 holds adjustment
                moveq       #0,d2
                move.b      127(a2),d2
                add.w       d2,d3                           ;add note number
                add.w       d3,d3                           ;words
                move.w      squa_freq_tab(pc,d3.w),d3


                btst        #2,(a2)                         ;test portamento flag
                beq.s       .noporta
                moveq       #0,d2
                move.w      54(a2),d2                       ;portamento accumulator
                add.w       88(a2),d2                       ;slide accumulator
                beq.s       .noporta
                ext.l       d2
                lea         porta_tune_tab(pc),a5
                add.w       d4,d4
                bmi.s       .scaleup

.scaledown      move.w      (a5,d4.w),d4                    ;porta tune table
                lsl.l       #5,d2                           ;*32 - compensatation for table
                divs        d4,d2
                bra.s       .endporta

.scaleup        neg.w       d4
                move.w      (a5,d4.w),d4                    ;porta tune table
                muls        d4,d2
                asr.w       #5,d2

.endporta       add.w       d2,d3                           ;include portamento accumulator
                btst        #12,d3
                beq.s       .noporta                        ;test for overflow
                bpl.s       .setlow
.sethigh        moveq       #0,d3
                move.w      d3,86(a2)                       ;stop slide
                bra.s       .noporta
.setlow         move.w      #$FFF,d3
                move.w      #0,86(a2)                       ;stop slide
.noporta

                btst        #2,5(a2)                        ;fixed detune flag
                beq.s       .nofixeddetf
                move.b      24(a2),d0
                ext.w       d0
                sub.w       d0,d3                           ;include fixed detune
.nofixeddetf


.chfixskip      btst        #2,2(a2)                        ;vib flag
                beq.s       .nochvib

                sub.w       76(a2),d3
.nochvib
                cmpi.w      #4095,d3                        ;test lower frequency limit
                ble.s       .end
                moveq       #0,d3                           ;if > 12 bits then clear, mixer will be muted

.end            rts

squa_freq_tab:  include     replay.s/squ_freq.s
porta_tune_tab: include     replay.s/porttune.s
                IFNE        FM_A+FMB_A+FMSB_A+FM_B+FMB_B+FMSB_B+FM_D+FMB_D+FMSB_D
fm_trans_tab:   include     replay.s/trans_fm.s
                ENDC

;..................................................................................
                IFNE    SYNCSQUARE_A+SID_A+SYNCBUZZER_A+FM_A+FMB_A+FMSB_A+PWM_A+SYNCSQUARE_B+SID_B+SYNCBUZZER_B+FM_B+FMB_B+FMSB_B+PWM_B+SYNCSQUARE_D+SID_D+SYNCBUZZER_D+FM_D+FMB_D+FMSB_D+PWM_D
gettimfreq:     moveq       #0,d3
                btst        #0,4(a2)                                ;fixed freq flag
                beq.s       .nochfix

                btst        #0,5(a2)
                beq.s       .fixfreqseq

.fixfreqfix     tst.b       37(a2)                                  ;make sure fixed sequence is zero
                bne.s       .fixfreqseq
                move.b      23(a2),d3
                lsl.w       #8,d3
                move.b      24(a2),d3
                bra         .chfixskip

.fixfreqseq     move.w      82(a2),d3                               ;d3 holds current fix freq adjustment
                cmpi.w      #$FFFF,d3                               ;if $FFFF then we ignore the fixed freq
                bne         .chfixskip                              ;in fix frequency mode we ignore arps, transpose and portamento
                moveq       #0,d3


.nochfix        btst        #0,5(a2)                                ;fixed detune flag
                beq.s       .nofixeddetc
                move.b      23(a2),d3                               ;include fixed detune
                ext.w       d3
.nofixeddetc

                btst        #0,1(a2)                                ;arp flag
                beq.s       .nocharp
                add.w       74(a2),d3

.nocharp        btst        #0,3(a2)                                ;transpose flag
                beq.s       .nochtransp

                move.b      60(a2),d0
                IFEQ    BUILD_BIN
                add.b       93(a2),d0
                ENDC
                ext.w       d0                                      ;down as well as up
                add.w       d0,d3                                   ;include transpose
.nochtransp

                moveq       #0,d4
                move.w      d3,d4                                   ;d4 holds adjustment
                moveq       #0,d2
                move.b      127(a2),d2
                add.w       d2,d3                                   ;add note number
                add.w       d3,d3                                   ;words
                lea         squa_freq_tab(pc),a5
                move.w      (a5,d3.w),d3

                btst        #0,(a2)                                 ;test portamento flag
                beq.s       .noporta
                moveq       #0,d2
                move.w      54(a2),d2                               ;portamento accumulator
                add.w       88(a2),d2                               ;slide accumulator
                beq.s       .noporta
                ext.l       d2
                lea         porta_tune_tab(pc),a5
                add.w       d4,d4
                bmi.s       .scaleup

.scaledown      move.w      (a5,d4.w),d4                            ;porta tune table
                lsl.l       #5,d2                                   ;*32 - compensatation for table
                divs        d4,d2
                bra.s       .endporta

.scaleup        neg.w       d4
                move.w      (a5,d4.w),d4                            ;porta tune table
                muls        d4,d2
                asr.w       #5,d2

.endporta       add.w       d2,d3                                   ;include portamento accumulator
                btst        #12,d3
                beq.s       .noporta                                ;test for overflow
                bpl.s       .setlow
.sethigh        moveq       #0,d3
                move.w      d3,86(a2)                               ;stop slide
                bra.s       .noporta
.setlow         move.w      #$FFF,d3
                move.w      #0,86(a2)                               ;stop slide
.noporta


                btst        #0,5(a2)                                ;fixed detune flag
                beq.s       .nofixeddetf
                move.b      24(a2),d0
                ext.w       d0
                sub.w       d0,d3                                   ;include fixed detune
.nofixeddetf


.chfixskip      btst        #0,2(a2)                                ;vib flag
                beq.s       .nochvib

                sub.w       76(a2),d3
.nochvib

.end            rts


pulsewidth_tune dc.w        PULWID_TUNE
timer_div_tab:  include     replay.s/timerdiv.s
                ENDC

;..................................................................................

getbuzzfreq:    moveq       #0,d3
                btst        #1,4(a2)                                    ;fixed freq flag
                beq.s       .nobuzfix

                btst        #1,5(a2)
                beq.s       .buzzfixseq

.buzzfixfix     tst.b       37(a2)                                      ;make sure fixed sequence is zero
                bne.s       .buzzfixseq
                move.b      23(a2),d3
                lsl.w       #8,d3
                move.b      24(a2),d3
                bra         .buzfixskip

.buzzfixseq     move.w      82(a2),d3                                   ;d3 holds current fix freq adjustment
                cmpi.w      #$FFFF,d3                                   ;if $FFFF then we ignore the fixed freq
                bne         .buzfixskip                                 ;in fix frequency mode we ignore arps, transpose and portamento
                moveq       #0,d3


.nobuzfix       btst        #1,5(a2)                                    ;fixed detune flag
                beq.s       .nofixeddetc
                move.b      23(a2),d3                                   ;include fixed detune
                ext.w       d3
.nofixeddetc:

                btst        #1,1(a2)                                    ;arp flag
                beq.s       .nobuzarp
                add.w       74(a2),d3

.nobuzarp       btst        #1,3(a2)                                    ;transpose flag
                beq.s       .nobuztrans

                move.b      60(a2),d0
                IFEQ    BUILD_BIN
                add.b       93(a2),d0
                ENDC
                ext.w       d0
                add.w       d0,d3                                       ;include transpose

.nobuztrans     moveq       #0,d4
                move.w      d3,d4                                       ;d4 holds adjustment
                moveq       #0,d2
                move.b      127(a2),d2                                  ;note number
                add.w       d2,d3
                add.w       d3,d3                                       ;words
                move.w      buzz_freq_tab(pc,d3.w),d3

                btst        #1,(a2)                                     ;test portamento flag
                beq.s       .noporta

                move.w      54(a2),d2                                   ;portamento accumulator
                add.w       88(a2),d2                                   ;slide accumulator
                beq.s       .noporta
                ext.l       d2
                lea         porta_tune_tab(pc),a5
                add.w       d4,d4
                bmi.s       .scaleup

.scaledown      move.w      (a5,d4.w),d4                                ;porta tune table
                lsl.l       #5,d2                                       ;*32 - compensatation for table
                divs        d4,d2
                bra.s       .endporta

.scaleup        neg.w       d4
                move.w      (a5,d4.w),d4                                ;porta tune table
                muls        d4,d2
                asr.w       #5,d2

.endporta       asr.w       #3,d2                                       ;shift so buzzer portamento is the same as the square wave
                addq.w      #1,d2                                       ;do rounding
                asr.w       #1,d2                                       ;d2 holds buzzer adjusted portamento accumulator
                add.w       d2,d3                                       ;include portamento accumulator
                bvc.s       .noporta                                    ;test for overflow
                bpl.s       .noporta
.sethigh        moveq       #0,d3
                move.w      d3,86(a2)                                   ;stop slide if positive overflow only for buzzer
.noporta

                btst        #1,5(a2)                                    ;fixed detune flag
                beq.s       .nofixeddetf
                moveq       #0,d0
                move.b      24(a2),d0
                ext.w       d0
                asr.w       #3,d0
                addq.w      #1,d0
                asr.w       #1,d0
                sub.w       d0,d3                                       ;include fixed detune
.nofixeddetf:


.buzfixskip:    btst        #1,2(a2)                                    ;vib flag
                beq.s       .nobuzvib

                moveq       #0,d0
                move.w      76(a2),d0                                   ;d0 holds current vibrato adjustment
                asr.w       #3,d0                                       ;shift so buzzer vibrato is the same as the square wave
                addq.w      #1,d0                                       ;do rounding
                asr.w       #1,d0                                       ;
                sub.w       d0,d3
.nobuzvib:

                move.b      d3,44+2(a1)                                 ; LSB
                move.w      d3,48+2(a1)                                 ; MSB (keep access to full word to allow it to be used by buzzer FM)
                rts

buzz_freq_tab:  include     replay.s/buz_freq.s


;..................................................................................
; interrupt routines

dummyinterrupt: rte

;......
                IFNE    INCLUDE_020
timlengthA_020: dc.b        1
                even
                ENDC


                IFNE    SYNCBUZZER_A
synbuzinteruptA ds.l        SYNCBUZZ_STEPS

                IFNE    INCLUDE_020
synbuzintA_020M macro       step
                move.l      \1*16+synbuzintA_SMC+2(pc),$FFFF8800.w
                cmpi.b      #\1+1,$BA55CAFE
                bls.s       \@
                move.l      \1*4+synbuzinteruptA+4(pc),$134.w
                rte
\@              move.l      synbuzinteruptA(pc),$134.w
                rte
                endm

COUNT           set         0
synbuzintA1_020:
                rept            SYNCBUZZ_STEPS
                synbuzintA_020M COUNT
COUNT           set             COUNT+1
                endr
                ENDC

                ds.b        256                                ;allow smc syncbuzzer routines moved away from boundary, boundaries are necessary to allow a move.w address write at the end of the SMC routine
synbuzintA_SMC:
                rept        SYNCBUZZ_STEPS
                move.l      #$0D00D0AD,$ffff8800.w
                move.w      #$BA55,$136.w                      ;setup next syncbuzz call
                rte
                endr
                ENDC
synbuzintA_SMCend


                IFNE    SYNCSQUARE_A
synsqinteruptA: ds.l        SYNSQU_STEPS

                IFNE    INCLUDE_020
synsqintA_020M: macro       step
                move.l      #$01000000,$ffff8800.w
                nop
                nop
                nop
                nop
                move.l      \1*44+synsqintA_SMC+12(pc),$ffff8800.w
                nop
                nop
                nop
                nop
                move.l      \1*44+synsqintA_SMC+22(pc),$ffff8800.w
                move.l      \1*44+synsqintA_SMC+30(pc),$ffff8800.w
                cmpi.b      #\1+1,$BA55CAFE
                bls.s       \@
                move.l      \1*4+synsqinteruptA+4(pc),$134.w
                rte
\@              move.l      synsqinteruptA(pc),$134.w
                rte
                endm

COUNT           set         0
ssintA1_020:
                rept            SYNSQU_STEPS
                synsqintA_020M  COUNT
COUNT           set             COUNT+1
                endr
                ENDC

                ds.b        256
synsqintA_SMC:
                rept        SYNSQU_STEPS
                move.l      #$01000000,$ffff8800.w             ; coarse                     +0
                nop                                            ;                            +8
                move.l      #$00000000,$ffff8800.w             ; fine                       +10
                nop                                            ;                            +18
                move.l      #$0000DEAD,$ffff8800.w             ; fine                       +20
                move.l      #$0100DEAD,$ffff8800.w             ; coarse                     +28
                move.w      #$BA55,$136.w                      ;setup next sync square call +36
                rte                                            ;                            +42
                endr
synsqintA_SMCend:
                ENDC


                IFNE    SID_A
sidinterruptsA: ds.l        SID_STEPS

                IFNE    INCLUDE_020
sidinterA_020M: macro       step
                move.l      \1*16+sidintersA_SMC+2(pc),$FFFF8800.w
                cmpi.b      #\1+1,$BA55CAFE
                bls.s       \@
                move.l      \1*4+sidinterruptsA+4(pc),$134.w
                rte
\@              move.l      sidinterruptsA(pc),$134.w
                rte
                endm

COUNT           set         0
sidinterA1_020:
                rept            SID_STEPS
                sidinterA_020M  COUNT
COUNT           set             COUNT+1
                endr
                ENDC


                ds.b        512                                ;allow smc sid routines moved away from boundary, boundaries are necessary to allow a move.w address write at the end of the SMC routine
sidintersA_SMC:
                rept    SID_STEPS
                move.l      #$0800DEAD,$FFFF8800.w             ;volume channel A
                move.w      #$BA55,$136.w                      ;setup next sid call
                rte
                endr
                ENDC
sidintersA_SMCend


                IFNE    DIGIDRUMS_A
digiinterruptA: ds.l        1
digiinterrvolA: ds.l        1

                IFNE    INCLUDE_020
digiinterA_020: pea         (a0)
                move.b      #$8,$FFFF8800.w                                ;select volume channel A
                move.l      digiinterA_SMC+8(pc),a0
                move.b      (a0),$FFFF8802.w
                bmi.s       .reachedend
                addq.l      #1,$BA55CAFE
                move.l      (sp)+,a0
                rte
.reachedend     move.b      #$D,$FFFF8802.w
                clr.b       $FFFFFA19.w
                move.l      (sp)+,a0
                rte

digiinvolA_020: movem.l     a0/d0,-(sp)
                move.b      #$8,$FFFF8800.w                                ;select volume channel A
                move.l      digiinvolA_SMC+12(pc),a0
                moveq       #0,d0
                move.b      (a0),d0
                bmi.s       .reachedend
                add.b       digiinvolA_SMC+21(pc),d0
                move.b      digivolscaleA(pc,d0.w),$FFFF8802.w
                addq.l      #1,$BA55CAFE
                movem.l     (sp)+,a0/d0
                rte
.reachedend     move.b      digiinvolA_SMC+41(pc),$FFFF8802.w
                clr.b       $FFFFFA19.w
                movem.l     (sp)+,a0/d0
                rte
                ENDC

digiinterA_SMC: move.b      #$8,$FFFF8800.w                         ;select volume channel A
                move.b      $DEAFFADE,$FFFF8802.w                   ;write volume
                bmi.s       .reachedend
.nextvalue      addq.l      #1,$BA55CAFE
                rte
.reachedend     move.b      #$D,$FFFF8802.w                         ;middle volume - avoid click
                clr.b       $FFFFFA19.w                             ;STOP timer A
                rte

digiinvolA_SMC: move.l      d0,-(sp)
                moveq       #0,d0
                move.b      #$8,$FFFF8800.w                         ;select volume channel A
                move.b      $DEAFFADE,d0
                bmi.s       .reachedend
                addi.b      #$F,d0                                  ;SMC volume
                move.b      digivolscaleA(pc,d0.w),$FFFF8802.w      ;write volume
.nextvalue      addq.l      #1,$BA55CAFE
                move.l      (sp)+,d0
                rte
.reachedend     move.b      #$D,$FFFF8802.w                         ;middle volume - avoid click SMC
                clr.b       $FFFFFA19.w                             ;STOP timer A
                move.l      (sp)+,d0
                rte

digivolscaleA:  dc.b        $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
                dc.b        $0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f
                even                                                ;ggn
                ENDC


                IFNE    FM_A
fminteruptA     ds.l        FM_STEPS

                IFNE    INCLUDE_020
fmintA_020M     macro       step
                move.l      \1*24+fmintA_SMC+2(pc),$FFFF8800.w
                move.l      \1*24+fmintA_SMC+10(pc),$FFFF8800.w
                cmpi.b      #\1+1,$BA55CAFE
                bls.s       \@
                move.l      \1*4+fminteruptA+4(pc),$134.w
                rte
\@              move.l      fminteruptA(pc),$134.w
                rte
                endm

COUNT           set         0
fmintA1_020:
                rept            FM_STEPS
                fmintA_020M     COUNT
COUNT           set             COUNT+1
                endr
                ENDC

                ds.b        256                                  ;allow smc FM routines moved away from boundary, boundaries are necessary to allow a move.w address write at the end of the SMC routine
fmintA_SMC:
                rept        FM_STEPS
                move.l      #$0000DEAD,$ffff8800.w
                move.l      #$0100DEAD,$ffff8800.w
                move.w      #$BA55,$136.w                        ;setup next FM call
                rte
                endr
                ENDC
fmintA_SMCend


                IFNE    FMB_A
fmbinteruptA    ds.l        FMB_STEPS

                IFNE    INCLUDE_020
fmbintA_020M    macro       step
                move.l      \1*24+fmbintA_SMC+2(pc),$FFFF8800.w
                move.l      \1*24+fmbintA_SMC+10(pc),$FFFF8800.w
                cmpi.b      #\1+1,$BA55CAFE
                bls.s       \@
                move.l      \1*4+fmbinteruptA+4(pc),$134.w
                rte
\@              move.l      fmbinteruptA(pc),$134.w
                rte
                endm

COUNT           set         0
fmbintA1_020:
                rept            FMB_STEPS
                fmbintA_020M    COUNT
COUNT           set             COUNT+1
                endr
                ENDC

                ds.b        256                                  ;allow smc FMB routines moved away from boundary, boundaries are necessary to allow a move.w address write at the end of the SMC routine
fmbintA_SMC:
                rept        FMB_STEPS
                move.l      #$0B00DEAD,$ffff8800.w
                move.l      #$0C00DEAD,$ffff8800.w
                move.w      #$BA55,$136.w                        ;setup next FMB call
                rte
                endr
                ENDC
fmbintA_SMCend


                IFNE    FMSB_A
fmsbinteruptA   ds.l        FMSB_STEPS

                IFNE    INCLUDE_020
fmsbintA_020M   macro       step
                move.l      \1*24+fmsbintA_SMC+2(pc),$FFFF8800.w
                move.l      \1*24+fmsbintA_SMC+10(pc),$FFFF8800.w
                move.l      \1*24+fmsbintA_SMC+18(pc),$FFFF8800.w
                cmpi.b      #\1+1,$BA55CAFE
                bls.s       \@
                move.l      \1*4+fmsbinteruptA+4(pc),$134.w
                rte
\@              move.l      fmsbinteruptA(pc),$134.w
                rte
                endm

COUNT           set         0
fmsbintA1_020:
                rept            FMSB_STEPS
                fmsbintA_020M   COUNT
COUNT           set             COUNT+1
                endr
                ENDC

                ds.b        256                                  ;allow smc FMSB routines moved away from boundary, boundaries are necessary to allow a move.w address write at the end of the SMC routine
fmsbintA_SMC:
                rept        FMSB_STEPS
                move.l      #$0D00D0AD,$ffff8800.w
                move.l      #$0B00DEAD,$ffff8800.w
                move.l      #$0C00DEAD,$ffff8800.w
                move.w      #$BA55,$136.w                        ;setup next FMSB call
                rte
                endr
                ENDC
fmsbintA_SMCend


                IFNE    PWM_A
pwminterruptsA  ds.l    2

                IFNE    INCLUDE_020
pwminterA_020_1 clr.b   $FFFFFA19.w
                move.b  pwminterA_SMC1+9(pc),$FFFFFA1F.w
                move.b  pwminterA_SMC1+15(pc),$FFFFFA19.w
                move.l  pwminterA_SMC1+20(pc),$FFFF8800.w
                move.l  pwminterruptsA+4(pc),$134.w
                rte

pwminterA_020_2 clr.b   $FFFFFA19.w
                move.b  pwminterA_SMC2+9(pc),$FFFFFA1F.w
                move.b  pwminterA_SMC2+15(pc),$FFFFFA19.w
                move.l  pwminterA_SMC2+20(pc),$FFFF8800.w
                move.l  pwminterruptsA(pc),$134.w
                rte
                ENDC

                ds.b        256                                 ;PWM allow pwm routines moved away from boundary, boundaries are necessary to allow a move.w address write at the end of the SMC routine
pwminterA_SMC1: move.b      #$00,$FFFFFA19.w                    ;stop timer, clr.b would be the same speed however the move.b makes this the same same size also for the timer D routine
                move.b      #$DE,$FFFFFA1F.w                    ;timer data register
                move.b      #$AD,$FFFFFA19.w                    ;timer divider
                move.l      #$0800DEAD,$FFFF8800.w              ;volume channel A
                move.w      #34,$136.w                          ;setup next pwm call
                rte

pwminterA_SMC2: move.b      #$00,$FFFFFA19.w                    ;stop timer
                move.b      #$DE,$FFFFFA1F.w                    ;timer data register
                move.b      #$AD,$FFFFFA19.w                    ;timer divider
                move.l      #$0800DEAD,$FFFF8800.w              ;volume channel A
                move.w      #34,$136.w                          ;setup next pwm call
                rte
                ENDC
pwminterA_SMCend


;......
                IFNE    INCLUDE_020
timlengthB_020: dc.b        1
                even
                ENDC


                IFNE    SYNCBUZZER_B
synbuzinteruptB ds.l        SYNCBUZZ_STEPS

                IFNE    INCLUDE_020
synbuzintB_020M macro       step
                move.l      \1*16+synbuzintB_SMC+2(pc),$FFFF8800.w
                cmpi.b      #\1+1,$BA55CAFE
                bls.s       \@
                move.l      \1*4+synbuzinteruptB+4(pc),$120.w
                rte
\@              move.l      synbuzinteruptB(pc),$120.w
                rte
                endm

COUNT           set         0
synbuzintB1_020:
                rept            SYNCBUZZ_STEPS
                synbuzintB_020M COUNT
COUNT           set             COUNT+1
                endr
                ENDC

                ds.b        256                                  ;allow smc syncbuzzer routines moved away from boundary, , boundaries are necessary to allow a move.w address write at the end of the SMC routine
synbuzintB_SMC:
                rept        SYNCBUZZ_STEPS
                move.l      #$0D00D0AD,$ffff8800.w
                move.w      #$BA55,$122.w                        ;setup next syncbuzz call
                rte
                endr
                ENDC
synbuzintB_SMCend


                IFNE    SYNCSQUARE_B
synsqinteruptB: ds.l        SYNSQU_STEPS

                IFNE    INCLUDE_020
synsqintB_020M: macro       step
                move.l      #$03000000,$ffff8800.w
                nop
                nop
                nop
                nop
                move.l      \1*44+synsqintB_SMC+12(pc),$ffff8800.w
                nop
                nop
                nop
                nop
                move.l      \1*44+synsqintB_SMC+22(pc),$ffff8800.w
                move.l      \1*44+synsqintB_SMC+30(pc),$ffff8800.w
                cmpi.b      #\1+1,$BA55CAFE
                bls.s       \@
                move.l      \1*4+synsqinteruptB+4(pc),$120.w
                rte
\@              move.l      synsqinteruptB(pc),$120.w
                rte
                endm

COUNT           set         0
ssintB1_020:
                rept            SYNSQU_STEPS
                synsqintB_020M  COUNT
COUNT           set             COUNT+1
                endr
                ENDC

                ds.b        256
synsqintB_SMC:
                rept        SYNSQU_STEPS
                move.l      #$03000000,$ffff8800.w
                nop
                move.l      #$02000000,$ffff8800.w
                nop
                move.l      #$0200DEAD,$ffff8800.w
                move.l      #$0300DEAD,$ffff8800.w
                move.w      #$BA55,$122.w                        ;setup next sync square call
                rte
                endr
synsqintB_SMCend:
                ENDC


                IFNE    SID_B
sidinterruptsB: ds.l        SID_STEPS

                IFNE        INCLUDE_020
sidinterB_020M: macro       step
                move.l      sidintersB_SMC+2+\1*16(pc),$FFFF8800.w
                cmpi.b      #\1+1,$BA55CAFE
                bls.s       \@
                move.l      sidinterruptsB+4+\1*4(pc),$120.w
                rte
\@              move.l      sidinterruptsB(pc),$120.w
                rte
                endm

COUNT           set        0
sidinterB1_020:
                rept            SID_STEPS
                sidinterB_020M  COUNT
COUNT           set             COUNT+1
                endr
                ENDC

                ds.b        512                                ;allow smc sid routines moved away from boundary, boundaries are necessary to allow a move.w address write at the end of the SMC routine
sidintersB_SMC:
                rept        SID_STEPS
                move.l      #$0900DEAD,$FFFF8800.w                ;volume channel B
                move.w      #$BA55,$122.w                        ;setup next sid call
                rte
                endr
                ENDC
sidintersB_SMCend


                IFNE    DIGIDRUMS_B
digiinterruptB: ds.l        1
digiinterrvolB: ds.l        1

                IFNE    INCLUDE_020
digiinterB_020: pea         (a0)
                move.b      #$9,$FFFF8800.w                                ;select volume channel B
                move.l      digiinterB_SMC+8(pc),a0
                move.b      (a0),$FFFF8802.w
                bmi.s       .reachedend
                addq.l      #1,$BA55CAFE
                move.l      (sp)+,a0
                rte
.reachedend     move.b      #$D,$FFFF8802.w
                clr.b       $FFFFFA1B.w
                move.l      (sp)+,a0
                rte

digiinvolB_020: movem.l     a0/d0,-(sp)
                move.b      #$9,$FFFF8800.w                                ;select volume channel B
                move.l      digiinvolB_SMC+12(pc),a0
                moveq       #0,d0
                move.b      (a0),d0
                bmi.s       .reachedend
                add.b       digiinvolB_SMC+21(pc),d0
                move.b      digivolscaleB(pc,d0.w),$FFFF8802.w
                addq.l      #1,$BA55CAFE
                movem.l     (sp)+,a0/d0
                rte
.reachedend     move.b      digiinvolB_SMC+41(pc),$FFFF8802.w
                clr.b       $FFFFFA1B.w
                movem.l     (sp)+,a0/d0
                rte
                ENDC

digiinterB_SMC: move.b      #$9,$FFFF8800.w                                ;select volume channel B
                move.b      $DEAFFADE,$FFFF8802.w                        ;write volume
                bmi.s       .reachedend
.nextvalue      addq.l      #1,$BA55CAFE
                rte
.reachedend     move.b      #$D,$FFFF8802.w                                ;middle volume - avoid click
                clr.b       $FFFFFA1B.w                                ;STOP timer B
                rte

digiinvolB_SMC: move.l      d0,-(sp)
                moveq       #0,d0
                move.b      #$9,$FFFF8800.w                                ;select volume channel B
                move.b      $DEAFFADE,d0
                bmi.s       .reachedend
                addi.b      #$F,d0
                move.b      digivolscaleB(pc,d0.w),$FFFF8802.w        ;write volume
.nextvalue      addq.l      #1,$BA55CAFE
                move.l      (sp)+,d0
                rte
.reachedend     move.b      #$D,$FFFF8802.w                                ;middle volume - avoid click
                clr.b       $FFFFFA1B.w                                ;STOP timer B
                move.l      (sp)+,d0
                rte

digivolscaleB:  dc.b        $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
                dc.b        $0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f
                even                                                    ;ggn
                ENDC


                IFNE    FM_B
fminteruptB     ds.l        FM_STEPS

                IFNE    INCLUDE_020
fmintB_020M     macro       step
                move.l      \1*24+fmintB_SMC+2(pc),$FFFF8800.w
                move.l      \1*24+fmintB_SMC+10(pc),$FFFF8800.w
                cmpi.b      #\1+1,$BA55CAFE
                bls.s       \@
                move.l      \1*4+fminteruptB+4(pc),$120.w
                rte
\@              move.l      fminteruptB(pc),$120.w
                rte
                endm

COUNT           set         0
fmintB1_020:
                rept            FM_STEPS
                fmintB_020M COUNT
COUNT           set             COUNT+1
                endr
                ENDC

                ds.b        256                                ;allow smc FM routines moved away from boundary, boundaries are necessary to allow a move.w address write at the end of the SMC routine
fmintB_SMC:
                rept        FM_STEPS
                move.l      #$0200DEAD,$ffff8800.w
                move.l      #$0300DEAD,$ffff8800.w
                move.w      #$BA55,$122.w                        ;setup next FM call
                rte
                endr
                ENDC
fmintB_SMCend


                IFNE    FMB_B
fmbinteruptB    ds.l        FMB_STEPS

                IFNE    INCLUDE_020
fmbintB_020M    macro       step
                move.l      \1*24+fmbintB_SMC+2(pc),$FFFF8800.w
                move.l      \1*24+fmbintB_SMC+10(pc),$FFFF8800.w
                cmpi.b      #\1+1,$BA55CAFE
                bls.s       \@
                move.l      \1*4+fmbinteruptB+4(pc),$120.w
                rte
\@              move.l      fmbinteruptB(pc),$120.w
                rte
                endm

COUNT           set         0
fmbintB1_020:
                rept            FMB_STEPS
                fmbintB_020M COUNT
COUNT           set             COUNT+1
                endr
                ENDC

                ds.b        256                                ;allow smc FMB routines moved away from boundary, boundaries are necessary to allow a move.w address write at the end of the SMC routine
fmbintB_SMC:
                rept        FMB_STEPS
                move.l      #$0B00DEAD,$ffff8800.w
                move.l      #$0C00DEAD,$ffff8800.w
                move.w      #$BA55,$122.w                      ;setup next FMB call
                rte
                endr
                ENDC
fmbintB_SMCend


                IFNE    FMSB_B
fmsbinteruptB   ds.l        FMSB_STEPS

                IFNE    INCLUDE_020
fmsbintB_020M   macro       step
                move.l      \1*24+fmsbintB_SMC+2(pc),$FFFF8800.w
                move.l      \1*24+fmsbintB_SMC+10(pc),$FFFF8800.w
                move.l      \1*24+fmsbintB_SMC+18(pc),$FFFF8800.w
                cmpi.b      #\1+1,$BA55CAFE
                bls.s       \@
                move.l      \1*4+fmsbinteruptB+4(pc),$120.w
                rte
\@              move.l      fmsbinteruptB(pc),$120.w
                rte
                endm

COUNT           set         0
fmsbintB1_020:
                rept            FMSB_STEPS
                fmsbintB_020M COUNT
COUNT           set             COUNT+1
                endr
                ENDC

                ds.b        256                                ;allow smc FMSB routines moved away from boundary, boundaries are necessary to allow a move.w address write at the end of the SMC routine
fmsbintB_SMC:
                rept        FMSB_STEPS
                move.l      #$0D00D0AD,$ffff8800.w
                move.l      #$0B00DEAD,$ffff8800.w
                move.l      #$0C00DEAD,$ffff8800.w
                move.w      #$BA55,$122.w                      ;setup next FMSB call
                rte
                endr
                ENDC
fmsbintB_SMCend


                IFNE    PWM_B
pwminterruptsB  ds.l    2

                IFNE    INCLUDE_020
pwminterB_020_1 clr.b   $FFFFFA1B.w
                move.b  pwminterB_SMC1+9(pc),$FFFFFA21.w
                move.b  pwminterB_SMC1+15(pc),$FFFFFA1B.w
                move.l  pwminterB_SMC1+20(pc),$FFFF8800.w
                move.l  pwminterruptsB+4(pc),$120.w
                rte

pwminterB_020_2 clr.b   $FFFFFA1B.w
                move.b  pwminterB_SMC2+9(pc),$FFFFFA21.w
                move.b  pwminterB_SMC2+15(pc),$FFFFFA1B.w
                move.l  pwminterB_SMC2+20(pc),$FFFF8800.w
                move.l  pwminterruptsB(pc),$120.w
                rte
                ENDC

                ds.b        256                                 ;PWM allow pwm routines moved away from boundary, boundaries are necessary to allow a move.w address write at the end of the SMC routine
pwminterB_SMC1: move.b      #$00,$FFFFFA1B.w                    ;stop timer
                move.b      #$DE,$FFFFFA21.w                    ;timer data register
                move.b      #$AD,$FFFFFA1B.w                    ;timer divider
                move.l      #$0900DEAD,$FFFF8800.w              ;volume channel B
                move.w      #34,$122.w                          ;setup next pwm call
                rte

pwminterB_SMC2: move.b      #$00,$FFFFFA1B.w                    ;stop timer
                move.b      #$DE,$FFFFFA21.w                    ;timer data register
                move.b      #$AD,$FFFFFA1B.w                    ;timer divider
                move.l      #$0900DEAD,$FFFF8800.w              ;volume channel B
                move.w      #34,$122.w                          ;setup next pwm call
                rte
                ENDC
pwminterB_SMCend

;......
                IFNE    INCLUDE_020
timlengthD_020: dc.b        1
                even
                ENDC


                IFNE    SYNCBUZZER_D
synbuzinteruptD:ds.l        SYNCBUZZ_STEPS

                IFNE        INCLUDE_020
synbuzintD_020M macro       step
                move.l      \1*16+synbuzintD_SMC+2(pc),$FFFF8800.w
                cmpi.b      #\1+1,$BA55CAFE
                bls.s       \@
                move.l      \1*4+synbuzinteruptD+4(pc),$110.w
                rte
\@              move.l      synbuzinteruptD(pc),$110.w
                rte
                endm

COUNT           set        0
synbuzintD1_020:
                rept            SYNCBUZZ_STEPS
                synbuzintD_020M COUNT
COUNT           set             COUNT+1
                endr
                ENDC

                ds.b        256                                ;allow smc syncbuzzer routines moved away from boundary, boundaries are necessary to allow a move.w address write at the end of the SMC routine
synbuzintD_SMC:
                rept        SYNCBUZZ_STEPS
                move.l      #$0D00D0AD,$ffff8800.w
                move.w      #$BA55,$112.w                        ;setup next syncbuzz call
                rte
                endr
                ENDC
synbuzintD_SMCend


                IFNE    SYNCSQUARE_D
synsqinteruptD: ds.l        SYNSQU_STEPS

                IFNE    INCLUDE_020
synsqintD_020M: macro       step
                move.l      #$05000000,$ffff8800.w
                nop
                nop
                nop
                nop
                move.l      \1*44+synsqintD_SMC+12(pc),$ffff8800.w
                nop
                nop
                nop
                nop
                move.l      \1*44+synsqintD_SMC+22(pc),$ffff8800.w
                move.l      \1*44+synsqintD_SMC+30(pc),$ffff8800.w
                cmpi.b      #\1+1,$BA55CAFE
                bls.s       \@
                move.l      \1*4+synsqinteruptD+4(pc),$110.w
                rte
\@              move.l      synsqinteruptD(pc),$110.w
                rte
                endm

COUNT           set         0
ssintD1_020:
                rept            SYNSQU_STEPS
                synsqintD_020M  COUNT
COUNT           set             COUNT+1
                endr
                ENDC

                ds.b        256
synsqintD_SMC:
                rept        SYNSQU_STEPS
                move.l      #$05000000,$ffff8800.w
                nop
                move.l      #$04000000,$ffff8800.w
                nop
                move.l      #$0400DEAD,$ffff8800.w
                move.l      #$0500DEAD,$ffff8800.w
                move.w      #$BA55,$112.w                        ;setup next sync square call
                rte
                endr
synsqintD_SMCend:
                ENDC


                IFNE    SID_D
sidinterruptsD: ds.l        SID_STEPS

                IFNE    INCLUDE_020
sidinterD_020M: macro       step
                move.l      sidintersD_SMC+2+\1*16(pc),$FFFF8800.w
                cmpi.b      #\1+1,$BA55CAFE
                bls.s       \@
                move.l      sidinterruptsD+4+\1*4(pc),$110.w
                rte
\@              move.l      sidinterruptsD(pc),$110.w
                rte
                endm

COUNT           set        0
sidinterD1_020:
                rept            SID_STEPS
                sidinterD_020M  COUNT
COUNT           set             COUNT+1
                endr
                ENDC

                ds.b        512                                ;allow smc sid routines moved away from boundary, boundaries are necessary to allow a move.w address write at the end of the SMC routine
sidintersD_SMC:
                rept    SID_STEPS
                move.l      #$0A00DEAD,$FFFF8800.w             ;volume channel C
                move.w      #$BA55,$112.w                      ;setup next sid call
                rte
                endr
                ENDC
sidintersD_SMCend


                IFNE    DIGIDRUMS_D
digiinterruptD: ds.l        1
digiinterrvolD: ds.l        1

                IFNE    INCLUDE_020
digiinterD_020: pea         (a0)
                move.b      #$A,$FFFF8800.w                                ;select volume channel C
                move.l      digiinterD_SMC+8(pc),a0
                move.b      (a0),$FFFF8802.w
                bmi.s       .reachedend
                addq.l      #1,$BA55CAFE
                move.l      (sp)+,a0
                rte
.reachedend     move.b      #$D,$FFFF8802.w
                andi.b      #%11110000,$FFFFFA1D.w                        ;STOP timer D
                move.l      (sp)+,a0
                rte

digiinvolD_020: movem.l     a0/d0,-(sp)
                move.b      #$A,$FFFF8800.w                                ;select volume channel C
                move.l      digiinvolD_SMC+12(pc),a0
                moveq       #0,d0
                move.b      (a0),d0
                bmi.s       .reachedend
                add.b       digiinvolD_SMC+21(pc),d0
                move.b      digivolscaleD(pc,d0.w),$FFFF8802.w
                addq.l      #1,$BA55CAFE
                movem.l     (sp)+,a0/d0
                rte
.reachedend     move.b      digiinvolD_SMC+41(pc),$FFFF8802.w
                andi.b      #%11110000,$FFFFFA1D.w              ;STOP timer D
                movem.l     (sp)+,a0/d0
                rte
                ENDC

digiinterD_SMC: move.b      #$A,$FFFF8800.w                     ;select volume channel C
                move.b      $DEAFFADE,$FFFF8802.w               ;write volume
                bmi.s       .reachedend
.nextvalue      addq.l      #1,$BA55CAFE
                rte
.reachedend     move.b      #$D,$FFFF8802.w                     ;middle volume - avoid click
                andi.b      #%11110000,$FFFFFA1D.w              ;STOP timer D
                rte

digiinvolD_SMC: move.l      d0,-(sp)
                moveq       #0,d0
                move.b      #$A,$FFFF8800.w                     ;select volume channel C
                move.b      $DEAFFADE,d0
                bmi.s       .reachedend
                addi.b      #$F,d0
                move.b      digivolscaleD(pc,d0.w),$FFFF8802.w  ;write volume
.nextvalue      addq.l      #1,$BA55CAFE
                move.l      (sp)+,d0
                rte
.reachedend     move.b      #$D,$FFFF8802.w                     ;middle volume - avoid click
                andi.b      #%11110000,$FFFFFA1D.w              ;STOP timer D
                move.l      (sp)+,d0
                rte

digivolscaleD:  dc.b        $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0
                dc.b        $0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f
                ENDC
                even


                IFNE    FM_D
fminteruptD:    ds.l        FM_STEPS

                IFNE        INCLUDE_020
fmintD_020M     macro       step
                move.l      \1*24+fmintD_SMC+2(pc),$FFFF8800.w
                move.l      \1*24+fmintD_SMC+10(pc),$FFFF8800.w
                cmpi.b      #\1+1,$BA55CAFE
                bls.s       \@
                move.l      \1*4+fminteruptD+4(pc),$110.w
                rte
\@              move.l      fminteruptD(pc),$110.w
                rte
                endm

COUNT           set        0
fmintD1_020:
                rept            FM_STEPS
                fmintD_020M COUNT
COUNT           set             COUNT+1
                endr
                ENDC

                ds.b        256                                  ;allow smc FM routines moved away from boundary, boundaries are necessary to allow a move.w address write at the end of the SMC routine
fmintD_SMC:
                rept        FM_STEPS
                move.l      #$0400DEAD,$ffff8800.w
                move.l      #$0500DEAD,$ffff8800.w
                move.w      #$BA55,$112.w                        ;setup next FM call
                rte
                endr
                ENDC
fmintD_SMCend


                IFNE    FMB_D
fmbinteruptD:   ds.l        FMB_STEPS

                IFNE        INCLUDE_020
fmbintD_020M    macro       step
                move.l      \1*24+fmbintD_SMC+2(pc),$FFFF8800.w
                move.l      \1*24+fmbintD_SMC+10(pc),$FFFF8800.w
                cmpi.b      #\1+1,$BA55CAFE
                bls.s       \@
                move.l      \1*4+fmbinteruptD+4(pc),$110.w
                rte
\@              move.l      fmbinteruptD(pc),$110.w
                rte
                endm

COUNT           set        0
fmbintD1_020:
                rept            FMB_STEPS
                fmbintD_020M COUNT
COUNT           set             COUNT+1
                endr
                ENDC

                ds.b        256                                  ;allow smc FMB routines moved away from boundary, boundaries are necessary to allow a move.w address write at the end of the SMC routine
fmbintD_SMC:
                rept        FMB_STEPS
                move.l      #$0B00DEAD,$ffff8800.w
                move.l      #$0C00DEAD,$ffff8800.w
                move.w      #$BA55,$112.w                        ;setup next FMB call
                rte
                endr
                ENDC
fmbintD_SMCend

                IFNE    FMSB_D
fmsbinteruptD:  ds.l        FMSB_STEPS

                IFNE        INCLUDE_020
fmsbintD_020M   macro       step
                move.l      \1*24+fmsbintD_SMC+2(pc),$FFFF8800.w
                move.l      \1*24+fmsbintD_SMC+10(pc),$FFFF8800.w
                move.l      \1*24+fmsbintD_SMC+18(pc),$FFFF8800.w
                cmpi.b      #\1+1,$BA55CAFE
                bls.s       \@
                move.l      \1*4+fmsbinteruptD+4(pc),$110.w
                rte
\@              move.l      fmsbinteruptD(pc),$110.w
                rte
                endm

COUNT           set        0
fmsbintD1_020:
                rept            FMSB_STEPS
                fmsbintD_020M COUNT
COUNT           set             COUNT+1
                endr
                ENDC

                ds.b        256                                  ;allow smc FMSB routines moved away from boundary, boundaries are necessary to allow a move.w address write at the end of the SMC routine
fmsbintD_SMC:
                rept        FMSB_STEPS
                move.l      #$0D00D0AD,$ffff8800.w
                move.l      #$0B00DEAD,$ffff8800.w
                move.l      #$0C00DEAD,$ffff8800.w
                move.w      #$BA55,$112.w                        ;setup next FMSB call
                rte
                endr
                ENDC
fmsbintD_SMCend


                IFNE    PWM_D
pwminterruptsD  ds.l    2

                IFNE    INCLUDE_020
pwminterD_020_1 move.w  d0,-(sp)
                andi.b  #$F0,$FFFFFA1D.w
                move.b  pwminterD_SMC1+9(pc),$FFFFFA25.w
                move.b  pwminterD_SMC1+15(pc),d0
                or.b    d0,$FFFFFA1D.w
                move.l  pwminterD_SMC1+20(pc),$FFFF8800.w
                move.l  pwminterruptsD+4(pc),$110.w
                move.w  (sp)+,d0
                rte

pwminterD_020_2 move.w  d0,-(sp)
                andi.b  #$F0,$FFFFFA1D.w
                move.b  pwminterD_SMC2+9(pc),$FFFFFA25.w
                move.b  pwminterD_SMC2+15(pc),d0
                or.b    d0,$FFFFFA1D.w
                move.l  pwminterD_SMC2+20(pc),$FFFF8800.w
                move.l  pwminterruptsD(pc),$110.w
                move.w  (sp)+,d0
                rte
                ENDC

                ds.b        256                                 ;PWM allow pwm routines moved away from boundary, boundaries are necessary to allow a move.w address write at the end of the SMC routine
pwminterD_SMC1: andi.b      #$F0,$FFFFFA1D.w                    ;stop timer
                move.b      #$DE,$FFFFFA25.w                    ;timer data register
                ori.b       #$00,$FFFFFA1D.w                    ;timer divider
                move.l      #$0A00DEAD,$FFFF8800.w              ;volume channel C
                move.w      #34,$112.w                          ;setup next pwm call
                rte

pwminterD_SMC2: andi.b      #$F0,$FFFFFA1D.w                    ;stop timer
                move.b      #$DE,$FFFFFA25.w                    ;timer data register
                ori.b       #$00,$FFFFFA1D.w                    ;timer divider
                move.l      #$0A00DEAD,$FFFF8800.w              ;volume channel C
                move.w      #34,$112.w                          ;setup next pwm call
                rte
                ENDC
pwminterD_SMCend


interruptbackup ds.b        interruptbackup-dummyinterrupt
;..................................................................................
; temporary storage of ym registers
YMtemp:         dc.l        $00000000           ;+0         fA
                dc.l        $01000000           ;+4         fA
                dc.l        $02000000           ;+8         fB
                dc.l        $03000000           ;+12        fB
                dc.l        $04000000           ;+16        fC
                dc.l        $05000000           ;+20        fC
                dc.l        $06000000           ;+24        fNoise
                dc.l        $07000000           ;+28        Mixer
                dc.l        $08000000           ;+32        vA
                dc.l        $09000000           ;+36        vB
                dc.l        $0A000000           ;+40        vC
                dc.l        $0B000000           ;+44        fBuzz
                dc.l        $0C000000           ;+48        fBuzz
                dc.w        $0                  ;+52        buzzer wave channel
                even

;instrument buffers
                ;ds.b       1                   ;+0         Portamento flag
                ;ds.b       1                   ;+1         Arpeggio flag
                ;ds.b       1                   ;+2         Vibrato flag
                ;ds.b       1                   ;+3         Transpose flag
                ;ds.b       1                   ;+4         Fixed freq flag
                ;ds.b       1                   ;+5         Fixed detune flag
                ;ds.b       ?                   ;+6-15      reserved

                ;ds.b       1                   ;+16        Sequence Speed
                ;ds.b       1                   ;+17        Pulse width
                ;ds.b       1                   ;+18        Buzzer wave $0->$F
                ;ds.b       1                   ;+19        Start sync
                ;ds.b       1                   ;+20        Digi sample
                ;ds.b       1                   ;+21        Digi rate
                ;ds.b       1                   ;+22        Instrument volume
                ;ds.b       1                   ;+23        Fixed detune coarse
                ;ds.b       1                   ;+24        Fixed detune fine
                ;ds.b       ?                   ;+25-31     reserved

                ;ds.b       1                   ;+32        Volume sequence
                ;ds.b       1                   ;+33        Arpeggio sequence
                ;ds.b       1                   ;+34        Vibrato sequence
                ;ds.b       1                   ;+35        Mixer sequence
                ;ds.b       1                   ;+36        Noise freq sequence
                ;ds.b       1                   ;+37        Fixed freq sequence
                ;ds.b       1                   ;+38        Timer waveform sequence
                ;ds.b       1                   ;+39        PWM sequence
                ;ds.b       ?                   ;+40-47     reserved                   Normal instrument structure, not including name

                ;ds.b       1                   ;+48        Noise transpose amount     Extra data needed for replay routine
                ;ds.b       1                   ;+49        sequence speed count
                ;ds.b       1                   ;+50        start sync done
                ;ds.b       1                   ;+51        volume column
                ;ds.w       1                   ;+52        portamento rate
                ;                               ;+53          "         "
                ;ds.w       1                   ;+54        portamento accumulator
                ;                               ;+55          "                "
                ;ds.b       1                   ;+56        Xtra arpeggio 1
                ;ds.b       1                   ;+57        Xtra arpeggio 2
                ;ds.b       1                   ;+58        Xtra arpeggio counter
                ;ds.b       1                   ;+59        digi playing
                ;ds.b       1                   ;+60        transpose
                ;ds.b       1                   ;+61        RLE counter

                ;ds.b       1                   ;+62        Volume seq count
                ;ds.b       1                   ;+63        Arpeggio seq count
                ;ds.b       1                   ;+64        Vibrato seq count
                ;ds.b       1                   ;+65        Mixer seq count
                ;ds.b       1                   ;+66        Noise seq count
                ;ds.b       1                   ;+67        Fixed wave seq count
                ;ds.b       1                   ;+68        PWM seq count

                ;ds.b       1                   ;+69        9ulse command set
                ;ds.w       1                   ;+70        reserved
                ;                               ;+71        reserved

                ;ds.b       1                   ;+72        Volume precalc
                ;ds.b       1
                ;ds.b       1                   ;+74        Arpeggio precalc
                ;ds.b       1
                ;ds.b       1                   ;+76        Vibrato precalc
                ;ds.b       1
                ;ds.b       1                   ;+78        Mixer precalc
                ;ds.b       1
                ;ds.b       1                   ;+80        Noise precalc
                ;ds.b       1
                ;ds.b       1                   ;+82        Fixed precalc
                ;ds.b       1
                ;ds.b       1                   ;+84        PWM precalc

                ;ds.b       1                   ;+85        sequence completed flags
                                                ;           b0=vol b1=arp b2=vib b3=mix b4=noise b5=fix d6=pwm

                ;ds.w       1                   ;+86        slide rate
                ;                               ;+87          "    "
                ;ds.w       1                   ;+88        slide accumulator
                ;                               ;+89          "           "

                ;ds.b       ?                   ;+90-91     reserved

                ;ds.b       1                   ;+92        track noise transpose (not cleared)
                ;ds.b       1                   ;+93        track transpose       (not cleared)
                ;ds.b       1                   ;+94        track volume          (not cleared)

                ;ds.b       1                   ;+95        PWM slide rate        (not cleared)
                ;ds.w       1                   ;+96        PWM slide accumulator (not cleared)
                ;                               ;+97          "               "   (not cleared)

                ;ds.b       ?                   ;+98-121    reserved !not cleared!

                ;ds.b       1                   ;+122       current instrument
                ;ds.b       1                   ;+123       old sid ym volume
                ;ds.b       1                   ;+124       old sid timer seq
                ;ds.b       1                   ;+125       old syncbuzzer seq
                ;ds.b       1                   ;+126       reserved
                ;ds.b       1                   ;+127       current note


stopclearstart:
instrumentstart:
instrumentA:    ds.b        128
instrumentB:    ds.b        128
instrumentC:    ds.b        128


                ;STe DMA Instrument
                ;ds.b       1                   ;+ 0 1 note
                ;ds.b       1                   ;+ 1 1 sample
                ;ds.b       1                   ;+ 2 1 volume

                ;ds.b       1                   ;+ 3 2 note
                ;ds.b       1                   ;+ 4 2 sample
                ;ds.b       1                   ;+ 4 2 sample
                ;ds.b       1                   ;+ 5 2 volume

                ;ds.b       1                   ;+ 6 ? reserved
                ;ds.b       1                   ;+ 7 - RLE counter

                ;ds.b       1                   ;+ 8 1 playing
                ;ds.b       1                   ;+ 9 2 playing

                ;ds.b       1                   ;+10 1 current sample pointer
                ;ds.b       1                   ;+11
                ;ds.b       1                   ;+12
                ;ds.b       1                   ;+13
                ;ds.b       1                   ;+14 1 current sample length left to play
                ;ds.b       1                   ;+15

                ;ds.b       1                   ;+16 2 current sample pointer
                ;ds.b       1                   ;+17
                ;ds.b       1                   ;+18
                ;ds.b       1                   ;+19
                ;ds.b       1                   ;+20 2 current sample length left to play
                ;ds.b       1                   ;+21

                ;ds.b       1                   ;+22 1 track transpose
                ;ds.b       1                   ;+23 2 track transpose
                ;ds.b       1                   ;+24 1 track volume
                ;ds.b       1                   ;+25 2 track volume

instrumentD:    ds.b        26
instrumentend:

                even
pattern_table:  ds.l        256                 ;pointers to the data for each pattern
stopclearend:

;..................................................................................
; pointers to voice and song data used by replay rout
voicedatapoint: dc.b        "TRIM"              ; not recalculated on SNDH save
trakerdatapoint dc.b        "TRIM"              ; recalculated on SNDH save

                IFNE    BUILD_BIN
trakerdatalen:  dc.b        "TRIM"              ;only required by pattern pointer calculation
                ENDC
