    PROCESSOR 6502
    INCLUDE "vcs.h"
    INCLUDE "macro.h"

NUM_SCANLINES	= 242	; 242 scanlines for PAL, 192 for NTSC

    ORG $F000       ; Start of "cart area" (see Atari memory map)

;	Vertical sync

StartFrame:
	lda	#%00000010	; D1 = 1: Vertical sync 
	sta	VSYNC
	sta	WSYNC
	sta	WSYNC
	sta	WSYNC
	lda	#0
	sta	VSYNC		; End of vertical sync, "turn on 
	
;	Vertical blank, where game logic would go
	
Vblank:
	REPEAT 37       ; Wait until this (and the other 36) vertical blank
        sta WSYNC   ; scanlines are finished
    REPEND
    lda	#0
    sta	VBLANK		; Vertical blank is done, "turn on" the beam
    ldx	#0			; Counter for visible frames

;	The kernel, where drawing occurs

Scanline:
	lda #$00		; Black background
	sta COLUBK
	stx COLUP0		; Player color equals scanline count
	lda	#%01010101	; Bit pattern to display as player, usually a sprite
	sta	GRP0
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	sta	RESP0
	inx				; Increase scan line counter
ScanlineEnd:
	sta WSYNC       ; Wait for scanline end
	cpx #(NUM_SCANLINES - 1)
    bne Scanline	; x != NUM_SCANLINES - 1, go to next scanline

;	Overscan, where more game logic could go

Overscan:
	lda #%01000010  ; "turn off" the beam again...
    sta VBLANK      ;
    REPEAT 30       ; ...for 30 overscan scanlines...
        sta WSYNC
    REPEND
    jmp StartFrame  ; ...and start it over!
    
    
;	Cart configuration area, required for the 6502 can start our game

    ORG $FFFA

    .WORD StartFrame      ;     0xFFFA: NMI
    .WORD StartFrame      ;     0xFFFC: RESET
    .WORD StartFrame      ;     0xFFFE: IRQ

    END
