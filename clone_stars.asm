;
; CLONE STARS
;

; Code and graphics by TMR
; Music by Sack


; A quick, written-from-scratch copy of the Atari 8-bit demo Atari Stars
; Demo by the GPS - coded for C64CrapDebunk.Wordpress.com

; Notes: this source is formatted for the Xasm cross assembler from
; https://github.com/pfusik/xasm
; Compression is handled with Exomizer 2 which can be downloaded at
; http://hem.bredband.net/magli143/exo/

; build.bat will call both to create an assembled file and then the
; crunched release version.


; Memory Map
; $3400 - $4dff		music
; $4e00 - $4fff		display list
; $5000 - $57ff		program code/data
; $5800 - $5fff		player/missile data
; $6000 - $7fff		bitmapped picture
; $8000 -		scrolling message


; Pull in the binary data
		org $3400
		opt h-
		ins "binary\commando.xex"
		opt h+

		org $6100
		ins "binary\sp_bitmap.raw"


; Atari 8-bit register declarations
atract		equ $4d

; Shadow registers
vdslst		equ $0200
sdmctl		equ $022f
sdlstl		equ $0230
sdlsth		equ $0231
gprior		equ $026f

pcolr0		equ $02c0
pcolr1		equ $02c1
pcolr2		equ $02c2
pcolr3		equ $02c3
color0		equ $02c4
color1		equ $02c5
color2		equ $02c6
color3		equ $02c7
color4		equ $02c8
chbas		equ $02f4

; Registers
hposp0		equ $d000
hposp1		equ $d001
hposp2		equ $d002
hposp3		equ $d003
hposm0		equ $d004
hposm1		equ $d005
hposm2		equ $d006
hposm3		equ $d007
sizep0		equ $d008
sizep1		equ $d009
sizep2		equ $d00a
sizep3		equ $d00b
sizem		equ $d00c

colpm0		equ $d012
colpm1		equ $d013
colpm2		equ $d014
colpm3		equ $d015
colpf0		equ $d016
colpf1		equ $d017
colpf2		equ $d018
colpf3		equ $d019
colbk		equ $d01a
prior		equ $d01b
vdelay		equ $d01c
gractl		equ $d01d

dmactl		equ $d400
hscrol		equ $d404
vscrol		equ $d405
pmbase		equ $d407
chbase		equ $d409
wsync		equ $d40a
nmien		equ $d40e


; Label assignments
scroll_x	equ $0600
star_spd_count	equ $0601
player_ram	equ $5800


; Display list
		org $4e00
dlist		dta $70,$70,$30+$80

; Upper text line - static
		dta $47,<line_text,>line_text

; First half of the bitmapped image
		dta $4e,$40,$60
		dta $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		dta $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		dta $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		dta $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		dta $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		dta $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		dta $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		dta $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e

		dta $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		dta $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		dta $0e,$0e,$0e

; Second half of the bitmapped image
		dta $4e,$00,$70
		dta $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		dta $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		dta $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		dta $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		dta $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		dta $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		dta $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		dta $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e

		dta $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		dta $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		dta $0e,$0e,$0e

; Lower text line - scrolling message
		dta $57
scroll_pos	dta <scroll_text,>scroll_text

; End of the screen, loop back to the start
		dta $41,<dlist,>dlist


; Entry point for the code
		run $5000
		org $5000


; Set up the music driver
		lda #$00
		ldx #$00
		ldy #$40
		jsr $3400

; Set up vertical blank interrupt
		lda #$06
		ldx #>vblank
		ldy #<vblank
		jsr $e45c

; Set up display list / DLI
dl_init		lda #<dlist
		sta sdlstl
		lda #>dlist
		sta sdlsth

		lda #<dli
		sta vdslst+$00
		lda #>dli
		sta vdslst+$01
		lda #$c0
		sta nmien

; Set up shadow regisers for the playfield colours
		lda #$00
		sta color4
		lda #$12
		sta color0
		lda #$08
		sta color1
		lda #$0c
		sta color2

; initialise the player/missile graphics
		lda #>player_ram
		sta pmbase

		lda #$3f		; also engages overscan
		sta sdmctl
		lda #$03
		sta gractl		; enable player/missile DMA
		lda #$14
		sta gprior

		ldx #$00
		lda #$01
player_set	sta player_ram+$400,x
		sta player_ram+$500,x
		inx
		bne player_set

; Reset the scrolling message
		lda #$00
		sta scroll_x

		lda #<scroll_text
		sta scroll_pos+$00
		lda #>scroll_text
		sta scroll_pos+$01

; Reset timer for the slower starfield
		lda #$00
		sta star_spd_count


; Infinite loop - all of the code is executing on the interrupt
		jmp *


; Vertical blank interrupt - suppress attract mode timer
vblank		lda #$00
		sta atract

; Position the first missile off screen
		lda #$00
		sta hposp0

; Play the music
		jsr $3403

		jmp $e45f


; Display list interrupt
dli		pha
		txa
		pha
		tya
		pha

; Split the colours for the text line
		ldx #$00
text_splitter	lda text_colours+$00,x
		ldy text_backgrnd,x
		sta wsync
		sty colbk
		sta colpf0
		lda text_colours+$10,x
		sta colpf1
		inx
		cpx #$10
		bne text_splitter

		lda #$12
		sta colpf0
		lda #$04
		sta colpm1

		lda #$08
		sta colpf1
		lda #$0c
		sta colpf2

; Render the starfield (and sneakily update it at the same time!)
		ldx #$00
star_splitter	lda star_x_pos_1,x
		clc
		adc star_x_speeds_1,x
		sta star_x_pos_1,x
		ldy star_colours,x
		sta wsync
		sty colpm0
		sta hposp0
		lda star_x_pos_2,x
		sta hposp1
		inx
		cpx #$a7
		bne star_splitter

; Hide the starfield players and set the horizontal scroll register
		lda #$00
		sta hposp0
		sta hposp1

		lda scroll_x
		eor #$07
		sta hscrol


; Split the colours for the text line
		ldx #$00
scroll_splitter	lda scroll_colours+$00,x
		ldy text_backgrnd,x
		sta wsync
		sty colbk
		sta colpf0
		lda scroll_colours+$10,x
		sta colpf1
		inx
		cpx #$10
		bne scroll_splitter


; Update the scrolling message
		ldx scroll_x
		inx
		cpx #$08
		bne sx_xb

		inc scroll_pos+$00
		bne *+$05
		inc scroll_pos+$01

		lda scroll_pos+$00
		cmp <scroll_end
		bne sx_xb-$02
		lda scroll_pos+$01
		cmp >scroll_end
		bne sx_xb-$02

		lda #<scroll_text
		sta scroll_pos+$00
		lda #>scroll_text
		sta scroll_pos+$01

		ldx #$00
sx_xb		stx scroll_x

; Move the slower stars (every second frame)
		ldx star_spd_count
		inx
		cpx #$02
		bne ssc_xb

		ldx #$00
star_update	inc star_x_pos_2,x
		inx
		cpx #$a7
		bne star_update

		ldx #$00
ssc_xb		stx star_spd_count

		pla
		tay
		pla
		tax
		pla
		rti


; Starfield X positions
		org [[*/$100]+$01]*$100
star_x_pos_1	dta $7d,$6e,$d3,$c9,$4d,$99,$c9,$01
		dta $4c,$f4,$75,$ff,$f6,$d7,$6c,$ab
		dta $e3,$a0,$03,$af,$8b,$0b,$ad,$f0
		dta $c1,$14,$98,$d6,$6f,$e3,$f3,$0b
		dta $aa,$cb,$0b,$43,$b8,$c8,$9b,$d9
		dta $87,$5c,$75,$59,$a9,$a2,$7e,$aa
		dta $59,$97,$96,$ee,$1e,$bb,$b0,$6e
		dta $49,$a2,$f0,$9c,$27,$b5,$3a,$52

		dta $b7,$c6,$43,$10,$0d,$6f,$26,$1d
		dta $5c,$26,$a2,$2d,$9b,$66,$02,$84
		dta $a5,$10,$a8,$a6,$8f,$3b,$7e,$eb
		dta $9c,$4b,$7e,$75,$ac,$b1,$86,$17
		dta $c4,$69,$c1,$e1,$eb,$7e,$46,$1b
		dta $ab,$1d,$6a,$c9,$54,$b3,$94,$37
		dta $a4,$64,$27,$47,$e5,$6c,$36,$25
		dta $75,$31,$6d,$40,$39,$ee,$f8,$8f

		dta $54,$80,$2f,$94,$a2,$9d,$ce,$6e
		dta $40,$47,$4e,$ef,$b5,$d2,$c7,$78
		dta $6d,$3b,$e7,$9a,$fc,$0e,$e0,$59
		dta $86,$b8,$c2,$8c,$72,$18,$74,$da
		dta $82,$cb,$f1,$98,$b6,$a5,$b1,$24

		org [[*/$100]+$01]*$100
star_x_pos_2	dta $b7,$c6,$43,$10,$0d,$6f,$26,$1d
		dta $54,$80,$2f,$94,$a2,$9d,$ce,$6e
		dta $7d,$6e,$d3,$c9,$4d,$99,$c9,$01
		dta $5c,$26,$a2,$2d,$9b,$66,$02,$84
		dta $40,$47,$4e,$ef,$b5,$d2,$c7,$78
		dta $4c,$f4,$75,$ff,$f6,$d7,$6c,$ab
		dta $a5,$10,$a8,$a6,$8f,$3b,$7e,$eb
		dta $6d,$3b,$e7,$9a,$fc,$0e,$e0,$59

		dta $e3,$a0,$03,$af,$8b,$0b,$ad,$f0
		dta $9c,$4b,$7e,$75,$ac,$b1,$86,$17
		dta $c4,$69,$c1,$e1,$eb,$7e,$46,$1b
		dta $c1,$14,$98,$d6,$6f,$e3,$f3,$0b
		dta $86,$b8,$c2,$8c,$72,$18,$74,$da
		dta $aa,$cb,$0b,$43,$b8,$c8,$9b,$d9
		dta $ab,$1d,$6a,$c9,$54,$b3,$94,$37
		dta $a4,$64,$27,$47,$e5,$6c,$36,$25

		dta $87,$5c,$75,$59,$a9,$a2,$7e,$aa
		dta $82,$cb,$f1,$98,$b6,$a5,$b1,$24
		dta $59,$97,$96,$ee,$1e,$bb,$b0,$6e
		dta $49,$a2,$f0,$9c,$27,$b5,$3a,$52
		dta $75,$31,$6d,$40,$39,$ee,$f8,$8f

; Starfield X speeds
		org [[*/$100]+$01]*$100
star_x_speeds_1	dta $03,$01,$03,$01,$01,$02,$03,$03
		dta $03,$02,$02,$02,$01,$03,$02,$03
		dta $03,$02,$01,$02,$01,$03,$01,$03
		dta $02,$01,$02,$03,$01,$02,$01,$02
		dta $01,$03,$03,$02,$03,$03,$02,$03
		dta $02,$03,$02,$03,$02,$03,$02,$03
		dta $01,$02,$01,$02,$02,$02,$01,$02
		dta $02,$02,$01,$03,$02,$02,$03,$01

		dta $03,$01,$01,$02,$01,$03,$01,$03
		dta $01,$01,$01,$02,$01,$02,$02,$02
		dta $03,$03,$02,$02,$01,$03,$02,$01
		dta $01,$02,$01,$01,$02,$03,$02,$01
		dta $02,$02,$01,$02,$01,$03,$03,$02
		dta $03,$01,$03,$03,$03,$02,$03,$03
		dta $03,$01,$01,$01,$02,$02,$01,$03
		dta $02,$01,$02,$02,$03,$01,$02,$03

		dta $02,$01,$03,$02,$01,$03,$03,$01
		dta $03,$02,$02,$03,$02,$01,$02,$02
		dta $02,$02,$03,$02,$03,$03,$02,$03
		dta $02,$01,$03,$03,$02,$02,$01,$03
		dta $03,$03,$02,$02,$03,$03,$03,$03

; Starfield colours
		org [[*/$100]+$01]*$100
star_colours	dta $4c,$f4,$75,$ff,$f6,$d7,$6c,$ab
		dta $e3,$a0,$03,$ff,$8b,$0b,$ad,$f0
		dta $c1,$14,$98,$d6,$6f,$e3,$f3,$0b
		dta $aa,$cb,$0b,$43,$b8,$c8,$9b,$d9
		dta $87,$5c,$75,$59,$a9,$a2,$7e,$aa
		dta $59,$97,$96,$ee,$1e,$bb,$b0,$6e
		dta $49,$a2,$f0,$9c,$27,$b5,$3a,$52
		dta $b7,$c6,$43,$10,$0d,$6f,$26,$1d

		dta $5c,$26,$a2,$2d,$9b,$66,$02,$84
		dta $a5,$10,$a8,$a6,$8f,$3b,$7e,$eb
		dta $9c,$4b,$7e,$75,$ac,$b1,$86,$17
		dta $c4,$69,$c1,$e1,$eb,$7e,$46,$1b
		dta $ab,$1d,$6a,$c9,$54,$b3,$94,$37
		dta $a4,$64,$27,$47,$e5,$6c,$36,$25
		dta $75,$31,$6d,$40,$39,$ee,$f8,$8f
		dta $54,$80,$2f,$94,$a2,$9d,$ce,$6e

		dta $40,$47,$4e,$ef,$b5,$d2,$c7,$78
		dta $6d,$3b,$e7,$9a,$fc,$0e,$e0,$59
		dta $86,$b8,$c2,$8c,$72,$18,$74,$da
		dta $82,$cb,$f1,$98,$b6,$a5,$b1,$24
		dta $7d,$6e,$d3,$c9,$4d,$99,$c9,$01


; Foreground colours for the text lines
		org [[*/$100]+$01]*$100
text_colours	dta $f0,$f2,$f4,$f6,$f8,$fa,$fc,$fe
		dta $2e,$2c,$2a,$28,$26,$24,$22,$20

		dta $60,$62,$64,$66,$68,$6a,$6c,$6e
		dta $8e,$8c,$8a,$88,$86,$84,$82,$80

; Foreground colours for the scroller
scroll_colours	dta $a0,$a2,$a4,$a6,$a8,$aa,$ac,$ae
		dta $ce,$cc,$ca,$c8,$c6,$c4,$c2,$c0

		dta $20,$22,$24,$26,$28,$2a,$2c,$2e
		dta $4e,$4c,$4a,$48,$46,$44,$42,$40

; Background colours for the text lines and scroller
text_backgrnd	dta $04,$04,$00,$02,$02,$02,$02,$02
		dta $02,$02,$02,$02,$02,$04,$00,$00


; Static text line at the top of the screen
		org $8000
line_text	dta d"  * clone stars demo *  "

scroll_text	dta d"                                        "
		dta d"welcome to the CLONE STARS DEMO from C64CD!"
		dta d"    "

		dta d"CODING AND GRAPHICS CONVERSION BY tmr WITH A COVER OF "
		dta d"ROB HUBBARD'S COMMANDO MUSIC DONE BY sack"
		dta d"    "

		dta d"THIS IS BASED ON THE atari stars demo BY spike of the gps "
		dta d"WHICH WAS BUILT FROM SCRATCH WITH JUST A TEENSY PEEK AT THE "
		dta d"ORIGINAL'S DISPLAY LIST TO MAKE SURE I WAS RIGHT ABOUT HOW "
		dta d"THE SCROLLER WORKED."
		dta d"    "

		dta d"THERE ARE A FEW LITTLE COSMETIC ""IMPROVEMENTS"" SUCH AS "
		dta d"HORIZONTAL OVERSCAN, THROWING IN A LITTLE MORE COLOUR "
		dta d"AMONGST THE STARS AND USING THE SAME MODE FOR THE TWO "
		dta d"TEXT LINES, GENERALLY SPEAKING AT LEAST, THIS IS A QUITE "
		dta d"AUTHENTIC CLONE EVEN DOWN TO CHANGING THE LMS TO MOVE THIS "
		dta d"SCROLL - I DON'T USUALLY DO THAT!"
		dta d"    "

		dta d"A COUPLE OF c64cd HELLOS GO OUT TO:  "
		dta d"the harlow cracking service, "
		dta d"ROB HUBBARD, "
		dta d"happy demomaker, "
		dta d"STOAT & TIM, "
		dta d"yak "
		dta d"AND THE GPS."
		dta d"    "

		dta d"AND AS ALWAYS, ANTI-GREETINGS TO C64HATER!"
		dta d"    "

		dta d"END OF SCROLLING MESSAGE - THANKS FOR WATCHING!"

scroll_end	dta d"                                        "