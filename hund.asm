    PROCESSOR 6502
    INCLUDE "vcs.h"
    INCLUDE "macro.h"

STATE			= $80
P0_STEPS		= $81
P1_STEPS		= $82

WELCOME_COLOR	= $82
P0_COLOR		= $F8
P1_COLOR		= $EE

    ORG $F000       ; Start of "cart area" (see Atari memory map)

Init:
	ldx #0
	lda #0
Clear:
    sta 0,x
    inx
    bne Clear		; Will fall through when x wraps around after 0XFF
CommonInit:
    lda #$00		; Background color (black)
    sta COLUBK
    lda #$00
    sta CTRLPF
    jmp GameStartFrame
    ;jmp WelcomeStartFrame

VerticalSync:
    lda #%00000010	; D1=1 for VSYNC
    sta WSYNC
    sta VSYNC   	; D1=1, turns on Vertical Sync signal
    sta WSYNC
    sta WSYNC
    lda #0
    sta WSYNC
    sta VSYNC   ; D1=0 turn off Vertical Sync signal
    rts

VerticalBlank:    
    ldx #37
VerticalBlankLoop:        
    sta WSYNC
    dex
    bne VerticalBlankLoop
    rts
    
OverScan:
    lda #%01000010  ; D1=1
    sta VBLANK  	; VBLANK D1=1 turns image output off
    ldx #30
OverScanLoop:
	sta WSYNC
	dex
	bne OverScanLoop
	rts
    
WelcomeStartFrame:
	jsr VerticalSync

PrepareWelcome:   ; We'll use the first VBLANK scanline for setup
    lda #WELCOME_COLOR
    sta COLUPF
    ldx #0          ; X will count visible scanlines, let's reset it
    jsr VerticalBlank
    lda #0          ; Vertical blank is done, we can "turn on" the beam
    sta VBLANK

WelcomeScanline:
    cpx #128        ; "HUND" = (4 chars x 8 lines) x 4 scanlines = 128
    bcs WelcomeScanlineEnd ;
    txa             ; 
    lsr             ;
    lsr				;
    tay             ; y := x / 4
    lda WelcomePhrase,y
    sta PF1         ; Put the value on PF bits 4-11 (0-3 is PF0, 12-15 is PF2)
    
WelcomeScanlineEnd:
	nop
	nop
	nop
	lda #0			; don't display right half of PF
	sta PF1
    sta WSYNC       ; Wait for scanline end
    inx             ; Increase counter; repeat untill we got all kernel scanlines
    cpx #191		; 192 is the last visible scanline
    bne WelcomeScanline

WelcomeOverscan:
	jsr OverScan
    jmp WelcomeStartFrame

GameStartFrame:
	jsr VerticalSync
GamePrepare:		; VBLANK
    lda #P0_COLOR
    sta COLUP0
    lda #P1_COLOR
    sta COLUP1
    ldx #0          ; x: visible scan line counter
    ldy #0			; y: sprite row counter
    jsr VerticalBlank
    lda #0 			; Vertical blank is done, we can "turn on" the beam
    sta VBLANK
GameScanline:
	cpx #90				; 2		[2]
	bcs GameDog1		; 2		[4]
	cpx #30				; 2		[6]
	bcs GameDog0		; 2		[8]
	jmp GameScanlineEnd
GameDog0:
	cpx #38
	bcs GameScanlineEnd
	lda Hund0,y
	sta GRP0
	sta RESP0
	iny
	jmp GameScanlineEnd
GameDog1:
	cpx #98
	bcs GameScanlineEnd
	lda Hund0,y
	sta GRP1
	sleep 4
	sta RESP1
	iny
	jmp GameScanlineEnd
GameScanlineEnd:
    sta WSYNC
    inx
    cpx #191		; 192 is the last visible scanline
    bne GameScanline
GameOverscan:
	jsr OverScan
	jmp GameStartFrame 

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
