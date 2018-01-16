    PROCESSOR 6502
    INCLUDE "vcs.h"
    INCLUDE "macro.h"

MAX_X_POS		= 160
X_INITIAL		= 10
X_STEPS			= 4
SPRITE_HEIGHT	= 8
P0_COLOR		= $2A
P1_COLOR		= $7A
NUM_SCANLINES	= 242	; PAL


	SEG.U variables
	ORG $80
		
P0_PRESSED		ds 1
P1_PRESSED		ds 1
P0_X			ds 1
P1_X			ds 1
P0_WIN			ds 1
P1_WIN			ds 1
WELCOME_COLOR	ds 1
WELCOME_DISMISS	ds 1

	SEG				; End of uninitialised segment - start of ROM binary
    ORG $F000       ; Start of "cart area" (see Atari memory map)

Init:
	CLEAN_START
    lda #$00		; Background color (black)
    sta COLUBK
    lda #$00
    sta CTRLPF
    lda #P0_COLOR	; Player 1 color
    sta COLUP0
    lda #P1_COLOR	; Player 2 color
    sta COLUP1
    lda #X_INITIAL			; Initial positions
    sta P0_X
    sta P1_X
	lda	#0
	sta	WELCOME_DISMISS
    jmp WelcomeStartFrame
    
;	Helper function that skips x > 0 scanlines, x = 0 after this method
SkipScanlines:
	sta	WSYNC
	dex
	bne	SkipScanlines
	rts

;	SPLASH KERNEL
WelcomeStartFrame:
	VERTICAL_SYNC
	lda	#43
	sta	TIM64T		; Timer fires after around 2798 cycles, before end of last VBLANK scanline
	
WelcomePrepare:		; We'll use the first VBLANK scanline for setup
	lda	WELCOME_DISMISS
	bne GamePrepare
WelcomeAnimateColor:
	ldx	WELCOME_COLOR	; load and animate color
	stx COLUPF
	inc	WELCOME_COLOR
WelcomeButtonCheck:
	lda	INPT4			; D7 set = button pressed
	bpl	WelcomeP0Pressed
WelcomeWaitForVBLANKEnd:
	lda	INTIM
	bne	WelcomeWaitForVBLANKEnd
	sta	WSYNC
    sta VBLANK
    ldx #0          ; X will count visible scanlines, let's reset it
    lda #0          ; Vertical blank is done, we can "turn on" the beam
    jmp	WelcomeScanline
WelcomeP0Pressed:
	lda	INPT5
	bpl	WelcomeBothPressed
	jmp	WelcomeWaitForVBLANKEnd
WelcomeBothPressed:
	lda #1
	sta	WELCOME_DISMISS
	jmp	WelcomeWaitForVBLANKEnd
WelcomeScanline:
    cpx #128        ; "HUND" = (4 chars x 8 lines) x 4 scanlines = 128
    bcs WelcomeScanlineEnd
    txa
    lsr
    lsr
    tay             ; y := x / 4
    lda WelcomePhrase,y
    sta PF1
WelcomeScanlineEnd:
	SLEEP	5		; Turn off playfield so we display only left side of scree
	lda #0			
	sta PF1
    sta WSYNC       ; Wait for scanline end
    inx             ; Increase counter; repeat untill we got all kernel scanlines
    cpx #(NUM_SCANLINES-1)
    bne WelcomeScanline
WelcomeOverscan:
	lda #%01000010  ; D1=1
    sta VBLANK  	; VBLANK D1=1 turns image output off
    ldx #30
    jsr	SkipScanlines
    jmp WelcomeStartFrame
    
;	GAME KERNEL
GameStartFrame:
	VERTICAL_SYNC
	lda	#43
	sta	TIM64T
GamePrepare:		; VBLANK: 37 scanlines
	ldy	#0			; y: sprite row counter
GameP0ButtonCheck:
    lda	INPT4
    bpl	GameP0ButtonPressed
    lda	P0_PRESSED				; Button is currently not pressed
    bne	GameP0ButtonReleased	; Button previously pressed -> now released
    jmp	GameP0Done
GameP0ButtonPressed:
	inc	P0_PRESSED				; Allow wrap around
	jmp	GameP0Done
GameP0ButtonReleased:
	inc	P0_X					; Advance player 1
	lda	#0
	sta	P0_PRESSED				; Restore pressed state
GameP0Done:
GameP1ButtonCheck:
    lda INPT5
    bpl	GameP1ButtonPressed
    lda	P1_PRESSED				; Button is currently not pressed	
    bne	GameP1ButtonReleased	; Button previously pressed -> now released
    jmp	GameP1Done
GameP1ButtonPressed:
	inc	P1_PRESSED				; Allow wrap around
	jmp	GameP1Done
GameP1ButtonReleased:
	inc	P1_X					; Advance player 2
	lda	#0
	sta	P1_PRESSED				; Restore pressed state
GameP1Done:
GameHorzPos:
	lda	P0_X
	ldx	#0
	jsr	HorizontalPosition
	lda	P1_X
	ldx	#1
	jsr	HorizontalPosition
	ldy #0			; y: sprite row counter
	ldx	#0			; x: visible scan line counter
GameWaitForVBLANKEnd:
	lda	INTIM
	bne	GameWaitForVBLANKEnd
	sta	WSYNC
	sta	HMOVE
	sta VBLANK		; Turn on beam, a = 0 (timer value) 
GameScanline:				; Start of visible game area
	cpx #60
	beq GameDog0
	cpx #120
	beq GameDog1
GameScanlineEnd:
	inx
    sta WSYNC
    cpx #(NUM_SCANLINES-1)
    bne GameScanline
    jmp GameOverscan
    
; First dog
GameDog0:
	lda Hund0,y	
	sta GRP0
	sta WSYNC
	sta WSYNC
	iny
	cpy	#SPRITE_HEIGHT
	bcc	GameDog0
GameDog0Done:
	ldx	#(60 + 2 * SPRITE_HEIGHT)	; We started drawing first dog at scanline 30, 8 pixels on 2 lines
	ldy	#0
	jmp GameScanline
	
; Second dog
GameDog1:
	lda Hund0,y
	sta GRP1
	sta WSYNC	
	sta WSYNC
	iny
	cpy	#SPRITE_HEIGHT
	bcc	GameDog1
GameDog1Done:
	ldx	#(120 + 2 * 8)	; We started drawing second dog at scanline 90, 8 pixels on 2 lines
	jmp GameScanline

; Overscan: 30 scan lines
GameOverscan:
    lda #%00000010  ; D1=1
    sta VBLANK  	; VBLANK D1=1 turns image output off
    sta WSYNC
GameResultCalc:
	lda	P0_X
	cmp	#MAX_X_POS
	beq	BackToWelcome
	lda	P1_X
	cmp	#MAX_X_POS
	beq	BackToWelcome
	ldx #29
    jsr	SkipScanlines
	jmp GameStartFrame 
BackToWelcome:
	lda	#X_INITIAL
	sta	P0_X
	sta	P1_X
    lda	#0
    sta	GRP0
    sta	GRP1
	sta	WELCOME_DISMISS
	ldx #29
    jsr	SkipScanlines
    jmp	WelcomeStartFrame

HorizontalPosition:
	sta WSYNC                   ; 00     Sync to start of scanline.
	sec                         ; 02     Set the carry flag so no borrow will be applied during the division.
.divideby15
	sbc #15                     ; 04     Waste the necessary amount of time dividing X-pos by 15!
	bcs .divideby15             ; 06/07  11/16/21/26/31/36/41/46/51/56/61/66

	tay
	lda fineAdjustTable,y       ; 13 -> Consume 5 cycles by guaranteeing we cross a page boundary
	sta HMP0,x

	sta RESP0,x                 ; 21/ 26/31/36/41/46/51/56/61/66/71 - Set the rough position.
	rts

; This table converts the "remainder" of the division by 15 (-1 to -15) to the correct
; fine adjustment value. This table is on a page boundary to guarantee the processor
; will cross a page boundary and waste a cycle in order to be at the precise position
; for a RESP0,x write
	ORG $FE00

fineAdjustBegin:
    DC.B %01110000 ; Left 7
    DC.B %01100000 ; Left 6
    DC.B %01010000 ; Left 5
    DC.B %01000000 ; Left 4
    DC.B %00110000 ; Left 3
    DC.B %00100000 ; Left 2
    DC.B %00010000 ; Left 1
    DC.B %00000000 ; No movement
    DC.B %11110000 ; Right 1
    DC.B %11100000 ; Right 2
    DC.B %11010000 ; Right 3
    DC.B %11000000 ; Right 4
    DC.B %10110000 ; Right 5
    DC.B %10100000 ; Right 6
    DC.B %10010000 ; Right 7
fineAdjustTable EQU fineAdjustBegin - %11110001 ; NOTE: %11110001 = -15

	ORG $FEC0
	
WelcomePhrase:
    .BYTE %00000000 ; H
    .BYTE %01000010
    .BYTE %01000010
    .BYTE %01111110
    .BYTE %01000010
    .BYTE %01000010
    .BYTE %01000010
    .BYTE %00000000
    .BYTE %00000000 ; U
    .BYTE %01000010
    .BYTE %01000010
    .BYTE %01000010
    .BYTE %01000010
    .BYTE %01000010
    .BYTE %01111110
    .BYTE %00000000
    .BYTE %00000000 ; N
    .BYTE %01000010
    .BYTE %01100010
    .BYTE %01010010
    .BYTE %01001010
    .BYTE %01000110
    .BYTE %01000010
    .BYTE %00000000
    .BYTE %00000000 ; D
    .BYTE %01111000
    .BYTE %01000100
    .BYTE %01000010
    .BYTE %01000010
    .BYTE %01000100
    .BYTE %01111000
    .BYTE %00000000 ; Last byte written to PF1 (important, ensures last line of doesn't bleed)

Hund0:
	.BYTE %00000000
	.BYTE %00000100
	.BYTE %00000110
	.BYTE %00111110
	.BYTE %01111100
	.BYTE %00100010
	.BYTE %00010100
	.BYTE %00000000
Hund1:
	.BYTE %00000000
	.BYTE %00000100
	.BYTE %00000110
	.BYTE %10111110
	.BYTE %01111100
	.BYTE %00100100
	.BYTE %01000010
	.BYTE %00000000

InterruptVectors:
    ORG $FFFA             ; Cart config (so 6502 can start it up)

    .WORD Init      ;     0xFFFA: NMI
    .WORD Init      ;     0xFFFC: RESET
    .WORD Init      ;     0xFFFE: IRQ

    END
