                section bss

;.........................................................................................
; 8 pointers to digi samples (included in voiceset save)
digi0point:     ds.l    1
digi1point:     ds.l    1
digi2point:     ds.l    1
digi3point:     ds.l    1
digi4point:     ds.l    1
digi5point:     ds.l    1
digi6point:     ds.l    1
digi7point:     ds.l    1           ;32 bytes

;..........................................................................................
TAGinstru:      ds.l    2           ; MYM?INST

;..........................................................................................
instrumentdata: ;ds.b   16          ;+00-15 name                16 = 15 chars + 1 null

                ;ds.b   1           ;+16    Portamento flag
                ;ds.b   1           ;+17    Arpeggio flag
                ;ds.b   1           ;+18    Vibrato flag
                ;ds.b   1           ;+19    Transpose flag
                ;ds.b   1           ;+20    Fixed freq flag
                ;ds.b   1           ;+21    Fixed detune flag
                ;ds.b   ?           ;+22-31 reserved

                ;ds.b   1           ;+32    Sequence Speed
                ;ds.b   1           ;+33    Pulse width
                ;ds.b   1           ;+34    Buzzer wave           $0->$F
                ;ds.b   1           ;+35    Buzzer startsync
                ;ds.b   1           ;+36    Digi sample
                ;ds.b   1           ;+37    Digi rate             default = $20
                ;ds.b   1           ;+38    Instr Vol             default = $0->F
                ;ds.b   1           ;+39    Fixed detune coarse
                ;ds.b   1           ;+40    Fixed detune fine
                ;ds.b   1           ;+41    Frequency resolution  default = 0 = full resolution, 1 = buzzer resolution
                ;ds.b   1           ;+42-47 reserved

                ;ds.b   1           ;+48    Volume sequence
                ;ds.b   1           ;+49    Arppegio sequence
                ;ds.b   1           ;+50    Vibrato sequence
                ;ds.b   1           ;+51    Mixer sequence
                ;ds.b   1           ;+52    Noise freq sequence
                ;ds.b   1           ;+53    Fixed freq sequence
                ;ds.b   1           ;+54    Timer waveform sequence
                ;ds.b   1           ;+55    PWM sequence
                ;ds.b   ?           ;+56-63 reserved

                ds.b    64*$20      ; $20 instruments each 64 bytes
                                    ; 2048 bytes    = 2k

;..........................................................................................

digiparams:     ;ds.w   1           ; +00 length
                ;ds.b   1           ; +02 reserved
                ;ds.b   1           ; +03 reserved
                ds.b    8*4         ; 32 bytes

;..........................................................................................

sequencedata:   ;ds.w   SEQ_LENGTH-1   ; data in words
                ;ds.b   1              ; length    1 byte
                ;ds.b   1              ; repeat    1 byte
                ds.w    SEQ_LENGTH*256 ; 256 sequences
                                       ; 16384 bytes    = 16k
instrumentdataend:

;..........................................................................................
;Editor state
TAGtracker:     ds.l    2           ; MYM?TRAK

trackerdatastart:
                ;...+0
currentsongpos: ds.b    1           ;+0
currentpattpos: ds.b    1           ;+1
currpattposedit ds.b    1           ;+2
oldpattposedit: ds.b    1           ;+3

currentpattern1 ds.b    1           ;+4
currentpattern2 ds.b    1           ;+5
currentpattern3 ds.b    1           ;+6
currentpattern4 ds.b    1           ;+7

add:            ds.b    1           ;+8
pastemode:      ds.b    1           ;+9    0 = overwrite, 1 = overlay, 2 = underlay
lowlevelmask:   ds.b    1           ;+10
scrollpattern:  ds.b    1           ;+11
octave:         ds.b    1           ;+12

songspeed:      ds.b    1           ;+13
timercfreq:     ds.b    1           ;+14    50->200 = timercfreq

currentinstedit ds.b    1           ;+15
destinstedit:   ds.b    1           ;+16
instrumentbank: ds.b    1           ;+17

editorcolourr1: ds.b    1           ;+18    highlight
editorcolourg1: ds.b    1           ;+19
editorcolourb1: ds.b    1           ;+20
editorcolourr2: ds.b    1           ;+21    maus
editorcolourg2: ds.b    1           ;+22
editorcolourb2: ds.b    1           ;+23
mauspointertype ds.b    1           ;+24

playing:        ds.b    1           ;+25
recording:      ds.b    1           ;+26
patternplay:    ds.b    1           ;+27

currentseqedit: ds.b    1           ;+28
destseqedit:    ds.b    1           ;+29
seqeditpositon: ds.b    1           ;+30

midienables:    ds.b    1           ;+31   bit 0 = internal=0 / external=1
                                    ;      bit 1 = midi=0     / sync24=1
                                    ;      bit 2 = midicontrollers

midiinst1:      ds.b    1           ;+32
midiinst2:      ds.b    1           ;+33
midiinst3:      ds.b    1           ;+34
midiinst4:      ds.b    1           ;+35

timerenables:   ds.b    1           ;+36    bit2=A bit1=B bit0=D
channelmutes:   ds.b    1           ;+37    bit4=5 bit3=4 bit2=3 bit1=2 bit0=1

nextsongpos:    ds.b    1           ;+38
livemode:       ds.b    1           ;+39

midichan1:      ds.b    1           ;+40
midichan2:      ds.b    1           ;+41
midichan3:      ds.b    1           ;+42
midichan4:      ds.b    1           ;+43

trackpattpos1:  ds.b    1           ;+44
trackpattpos2:  ds.b    1           ;+45
trackpattpos3:  ds.b    1           ;+46
trackpattpos4:  ds.b    1           ;+47

songspeedcount: ds.b    1           ;+48
ste_dmasound:   ds.b    1           ;+49  0, 1, 2, native channels, midi

midiinst5:      ds.b    1           ;+50
midichan5:      ds.b    1           ;+51

nextpattern1:   ds.b    1           ;+52
nextpattern2:   ds.b    1           ;+53
nextpattern3:   ds.b    1           ;+54
nextpattern4:   ds.b    1           ;+55

stedmavolume:   ds.b    1           ;+56

midilatency:    ds.b    1           ;+57
editorvolume:   ds.b    1           ;+58

                ds.b    5           ; dont forget to add some extra bytes for 64 total
                ;ds.b   64
                even
;..........................................................................................
                ;...+64
songdata:       ds.b    256*4       ; +0    256 entries * 4 tracks
songlength:     ds.b    1           ; +1024
songrepeat:     ds.b    1           ; +1025

;..........................................................................................
                ;...+1090
patterndata:    ;                   ; TRACK
                ;ds.b   1           ; 0 note number          ; 0 note   A
                ;ds.b   1           ; 1 instrument number    ; 1 instru A
                ;ds.b   1           ; 2 volume               ; 2 volume A
                ;ds.b   1           ; 3 effect 1 type
                ;ds.b   1           ; 4 effect 1 number
                ;ds.b   1           ; 5 effect 2 type
                ;ds.b   1           ; 6 effect 2 number
                ;ds.b   1           ; 7 RLE byte - number of empty lines to next used line

                ds.b    8*$40*256   ; 8 bytes, 1 track, $40 lines, 256 patterns
                ; 131072 bytes (128k)
trackerdataend:
;..........................................................................................
TAGdigi:        ds.l    2           ; MYM?DIGI

digistart:
digi0:          ds.b    DIGI_LENGTH
digi1:          ds.b    DIGI_LENGTH
digi2:          ds.b    DIGI_LENGTH
digi3:          ds.b    DIGI_LENGTH
digi4:          ds.b    DIGI_LENGTH
digi5:          ds.b    DIGI_LENGTH
digi6:          ds.b    DIGI_LENGTH
digi7:          ds.b    DIGI_LENGTH
digiend:
