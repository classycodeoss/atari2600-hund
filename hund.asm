    PROCESSOR 6502
    INCLUDE "vcs.h"
    INCLUDE "macro.h"

STATE			= $80
P0_PRESS_CNT	= $81
P1_PRESS_CNT	= $82
P0_X			= $83
P1_X			= $84
WELCOME_COLOR   = $85
WELCOME_DISMISS	= $86
WELCOME_BTN_CNT	= $87
P0_WIN			= $88
P1_WIN			= $89

MAX_X_POS		= 10
WELCOME_DISMISS_PRESSES	= 4
STEP_PRESSES	= 10
SPRITE_HEIGHT	= 8
P0_COLOR		= $2A
P1_COLOR		= $7A
NUM_SCANLINES	= 242	; PAL

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
    lda #1			; Step counter initial values
    sta P0_X
    sta P1_X
    lda	#STEP_PRESSES
    sta	P0_PRESS_CNT
    sta	P1_PRESS_CNT
	lda	#0
	sta	WELCOME_DISMISS
    lda #WELCOME_DISMISS_PRESSES
    sta WELCOME_BTN_CNT
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
WelcomePostButtonCheck:
    ldx	#37
    jsr SkipScanlines
    sta VBLANK
    ldx #0          ; X will count visible scanlines, let's reset it
    lda #0          ; Vertical blank is done, we can "turn on" the beam
    jmp	WelcomeScanline
WelcomeP0Pressed:
	lda	INPT5
	bpl	WelcomeBothPressed
	jmp	WelcomePostButtonCheck
WelcomeBothPressed:
	lda #1
	sta	WELCOME_DISMISS
	jmp	WelcomePostButtonCheck
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
	
GamePrepare:		; VBLANK: 37 scanlines
	lda	#0
	ldy	#0			; y: sprite row counter
GameP0ButtonCheck:
    lda	INPT4
    bpl	GameP0ButtonPressed
GameP0Done:
	sta WSYNC		; 36 scanlines of VBLANK remaining
GameP1ButtonCheck:
    lda INPT5
    bpl	GameP1ButtonPressed
GameP1Done:
	sta	WSYNC		; 35 scanlines of VBLANK remaining
GamePostButtonCheck:
	lda	#0
	ldy #0			; y: sprite row counter
    ldx	#35			; Remaining scanlines in vertical blank
    jsr	SkipScanlines
    sta VBLANK		; Vertical blank is done, we can "turn on" the beam
    ldx	#0			; x: visible scan line counter
    jmp	GameScanline
GameP0ButtonPressed:
	dec	P0_PRESS_CNT
	beq	GameP0Advance
	jmp	GameP0Done
GameP0Advance:
	inc	P0_X
	lda	#STEP_PRESSES
	sta	P0_PRESS_CNT
	jmp	GameP0Done
GameP1ButtonPressed:
	dec	P1_PRESS_CNT
	beq	GameP1Advance
	jmp	GameP1Done
GameP1Advance
	inc	P1_X
	lda	#STEP_PRESSES
	sta	P1_PRESS_CNT
	jmp	GameP1Done
GameScanline:				; Start of visible game area
	cpx #60
	beq GameDog0Pre
	cpx #120
	beq GameDog1Pre
GameScanlineEnd:
	inx
    sta WSYNC
    cpx #(NUM_SCANLINES-1)
    bne GameScanline
    jmp GameOverscan
    
; First dog
GameDog0Pre:
	ldx P0_X	; (previous value of x = 30)
GameDog0:
	lda Hund0,y	
	sta GRP0
GameDog0Delay:
	dex
	bne	GameDog0Delay
	sta RESP0
	sta WSYNC
	sta WSYNC
	iny
	cpy	#SPRITE_HEIGHT
	bcc	GameDog0Pre
GameDog0Done:
	ldx	#(60 + 2 * SPRITE_HEIGHT)	; We started drawing first dog at scanline 30, 8 pixels on 2 lines
	ldy	#0
	jmp GameScanline
	
; Second dog
GameDog1Pre:
	ldx P1_X	; (previous value of x = 90)
GameDog1:
	lda Hund0,y
	sta GRP1
GameDog1Delay:
	dex
	bne	GameDog1Delay
	sta RESP1
	sta WSYNC
	sta WSYNC
	iny
	cpy	#SPRITE_HEIGHT
	bcc	GameDog1Pre
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
	lda	#1
	sta	P0_X
	sta	P1_X
    lda	#0
    sta	GRP0
    sta	GRP1
	sta	WELCOME_DISMISS
    lda #WELCOME_DISMISS_PRESSES
    sta WELCOME_BTN_CNT
	ldx #29
    jsr	SkipScanlines
    jmp	WelcomeStartFrame

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
