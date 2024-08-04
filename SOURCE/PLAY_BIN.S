;
;       Test binary replay functions
;
;       gwEm 2024
;

        section text
;................................................................
top:    move.l  4(sp),a5                ; address to basepage
        move.l  $0c(a5),d0              ; length of text segment
        add.l   $14(a5),d0              ; length of data segment
        add.l   $1c(a5),d0              ; length of bss segment
        add.l   #$1000,d0               ; length of stackpointer
        add.l   #$100,d0                ; length of basepage
        move.l  a5,d1                   ; address to basepage
        add.l   d0,d1                   ; end of program
        and.l   #-2,d1                  ; make address even
        move.l  d1,sp                   ; new stackspace

        move.l  d0,-(sp)                ; mshrink()
        move.l  a5,-(sp)                ;
        move.w  d0,-(sp)                ;
        move.w  #$4a,-(sp)              ;
        trap    #1                      ;
        lea     12(sp),sp               ;

        clr.l   -(sp)                   ; supervisor mode
        move.w  #$20,-(sp)              ;
        trap    #1                      ;
        addq.l  #6,sp                   ;
        move.l  d0,oldusp               ; store old user stack pointer

        move.b  $484.w,oldcon
        clr.b   $484.w			        ; keyclick off

        move.l  $114.w,oldtc            ; store old timer C vector
        move.l  #timer_c,$114.w         ; steal timer C

        lea     top(pc),a0              ; setup offsets
        add.l   #voiceoffset-top,a0     ;
        move.l  #voice-voiceoffset,(a0)+;
        move.l  #song-songoffset,(a0)+  ;
        move.l  #song_end-song,(a0)+    ;

        bsr     MUSIC+0                 ; init music

.loop   move.w  #7,-(sp)                ; wait for a key
        trap    #1                      ;
        addq.l  #2,sp                   ;
        cmpi.b  #$20,d0                 ; space quits
        beq.s   .end                    ;

;        moveq   #80,d0                  ; global volume
;        bsr     MUSIC+12                ;

        moveq   #100,d0                 ; note
        moveq   #1,d1                   ; instrument
        moveq   #2,d2                   ; channel
        bsr     MUSIC+16                ; manual play note

;        moveq   #3,d0                   ; next song position
;        bsr     MUSIC+20                ;

        bra.s   .loop

.end    bsr     MUSIC+4                 ; de-init music

        move.b  oldcon(pc),$484.w       ; restore key click
        move.l  oldtc,$114.w            ; restore timer C

        move.l  oldusp(pc),-(sp)        ; user mode
        move.w  #$20,-(sp)              ;
        trap    #1                      ;
        addq.l  #6,sp                   ;

        clr.w   -(sp)                   ; pterm()
        move.w  #$4c,-(sp)              ;
        trap    #1                      ;

oldtc:  ds.l    1
oldusp: ds.l    1
oldcon: ds.b    1
        even


;................................................................
timer_c sub.w   #50,tccount             ; you can use any replay rate here less than or equal to 200Hz, in this case its 50Hz
        bgt.s   .nocall                 ; dont call zak this time
        add.w   #200,tccount            ; syntheses any freq from 200hz system tick

        move.w  sr,-(sp)                ; store status word - system friendly, but not needed in a demo i guess
        move.w  #$2500,sr               ; enable timer interrupts
        bsr     MUSIC+8                 ; call music
        move.w  (sp)+,sr                ; restore status word

.nocall move.l  oldtc(pc),-(sp)         ; go to old vector (system friendly ;) )
        rts

tccount ds.w    200


;................................................................
MUSIC:              incbin  MYM_REPL.BIN
                    even
voiceoffset:        ds.l    1
songoffset:         ds.l    1
songlength:         ds.l    1

voice:              incbin  example3.myv ; example music by gwEm
                    even                 ;
song:               incbin  example3.mys ;
song_end:                                ;
                    even                 ;
