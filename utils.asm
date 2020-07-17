
; *******************************************************************
; *																	*
; * Utility routine:	Simple pseudo-random number generator.		*
; *																	*
; * Steps a pointer through the ROM (held in seed), returning		*
; * the contents of the byte at that location.						*
; *																	*
; *******************************************************************

F_random:
	LD HL,(seed)        ; Pointer
	LD A,H
	AND 31              ; keep it within first 8k of ROM.
	LD H,A
	LD A,(HL)           ; Get "random" number from location.
	INC HL              ; Increment pointer.
	LD (seed),HL
	RET

seed   DEFW 0

; *******************************************************************
; *																	*
; * Utility routine:	makes a bold font located at 60000			*
; *																	*
; *******************************************************************

F_boldFont:
	LD HL,15616			;ROM font
	LD DE,60000			;address of our font
	LD BC,768			;96 chars * 8 rows to alter
font1
	LD A,(HL)			;get bitmap
	RLCA				;rotate it left
	OR (HL)				;combine two images
	LD (DE),A			;write to new font
	INC HL				;next byte of old
	INC DE				;next byte of new
	DEC BC				;decrement counter
	LD A,B				;high byte
	OR C				;combine with low byte
	JR NZ,font1			;repeat until BC=zero
	LD HL,60000-256		;font minus 32*8
	LD (23606),HL		;point to new font
	RET

; *******************************************************************
; *																	*
; * Utility routine: 	Calculate address of attribute for			*
; *						character at (dispx, dispy).				*
; *																	*
; *******************************************************************

atadd
	LD A,C              ; vertical coordinate.
	RRCA                ; multiply by 32.
	RRCA                ; Shifting right with carry 3 times is
	RRCA                ; quicker than shifting left 5 times.
	LD E,A
	AND 3
	ADD A,88            ; 88x256=address of attributes.
	LD D,A
	LD A,E
	AND 224
	LD E,A
	LD A,B              ; horizontal position.
	ADD A,E
	LD E,A              ; DE=address of attributes.
	LD A,(DE)           ; return with attribute in accumulator.
	RET

; *******************************************************************
; *																	*
; * Utility routine: 	Pitch bend sound routine					*
; *																	*
; *******************************************************************

sound1
	;PUSH AF
	;PUSH HL
	;PUSH BC
	;PUSH DE
	PUSH IX				; CALL 949 changes IX

	LD HL,100			; starting pitch.
	LD B,5				; length of pitch bend.
snd1loop
	push bc
	push hl             ; store pitch.
	ld de,3             ; very short duration.
	call 949            ; ROM beeper routine.
	pop hl              ; restore pitch.
	inc hl              ; pitch going up.
	pop bc
	djnz snd1loop           ; repeat.

	POP IX
	;POP DE
	;POP BC
	;POP HL
	;POP AF

	RET
	
; *******************************************************************
; *																	*
; * Utility routine: 	Generic delay routine						*
; *																	*
; *******************************************************************

delay
	LD B, NUMBER_OF_HALTS
delay0
	HALT
	DJNZ delay0
	RET

; *******************************************************************
; *																	*
; * Utility routine: 	Score routine								*
; *																	*
; *******************************************************************	
	
score	DEFB	'000000'

uscor  ld a,(hl)           ; current value of digit.
       add a,b             ; add points to this digit.
       ld (hl),a           ; place new digit back in string.
       cp 58               ; more than ASCII value '9'?
       ret c               ; no - relax.
       sub 10              ; subtract 10.
       ld (hl),a           ; put new character back in string.
uscor0 dec hl              ; previous character in string.
       inc (hl)            ; up this by one.
       ld a,(hl)           ; what's the new value?
       cp 58               ; gone past ASCII nine?
       ret c               ; no, scoring done.
       sub 10              ; down by ten.
       ld (hl),a           ; put it back
       jp uscor0           ; go round again.

; *******************************************************************
; *																	*
; * Utility routine: 	High score routine							*
; *																	*
; *******************************************************************	   

hiscr	DEFB	'000000'

hscor	;PUSH HL				; preserve HL
		
		LD HL, hiscr		; point HL at player's score
		LD A,(HL)			; load first character of score
		LD HL, score		; point HL at current high score
		CP (HL)				; subtract hiscr char 1 from A
		JP C, updHiScr		; update the high score
		
		LD HL, hiscr		; point HL at player's score
		INC HL				; 2nd char of score
		LD A,(HL)			; load 2nd character of score
		LD HL, score		; point HL at current high score
		INC HL				; 2nd char of score
		CP (HL)				; subtract hiscr char 2 from A
		JP C, updHiScr		; update the high score
		
		LD HL, hiscr		; point HL at player's score
		INC HL				; 2nd char of score
		INC HL				; 3rd char of score
		LD A,(HL)			; load 3rd character of score
		LD HL, score		; point HL at current high score
		INC HL				; 2nd char of score
		INC HL				; 3rd char of score
		CP (HL)				; subtract hiscr char 3 from A
		JP C, updHiScr		; update the high score
		
		LD HL, hiscr		; point HL at player's score
		INC HL				; 2nd char of score
		INC HL				; 3rd char of score
		INC HL				; 4th char of score
		LD A,(HL)			; load 4th character of score
		LD HL, score		; point HL at current high score
		INC HL				; 2nd char of score
		INC HL				; 3rd char of score
		INC HL				; 4th char of score
		CP (HL)				; subtract hiscr char 4 from A
		JP C, updHiScr		; update the high score
		
		LD HL, hiscr		; point HL at player's score
		INC HL				; 2nd char of score
		INC HL				; 3rd char of score
		INC HL				; 4th char of score
		INC HL				; 5th char of score
		LD A,(HL)			; load 5th character of score
		LD HL, score		; point HL at current high score
		INC HL				; 2nd char of score
		INC HL				; 3rd char of score
		INC HL				; 4th char of score
		INC HL				; 5th char of score
		CP (HL)				; subtract hiscr char 5 from A
		JP C, updHiScr		; update the high score
		
		LD HL, hiscr		; point HL at player's score
		INC HL				; 2nd char of score
		INC HL				; 3rd char of score
		INC HL				; 4th char of score
		INC HL				; 5th char of score
		INC HL				; 6th char of score
		LD A,(HL)			; load 6th character of score
		LD HL, score		; point HL at current high score
		INC HL				; 2nd char of score
		INC HL				; 3rd char of score
		INC HL				; 4th char of score
		INC HL				; 5th char of score
		INC HL				; 6th char of score
		CP (HL)				; subtract hiscr char 6 from A
		JP C, updHiScr		; update the high score

		;POP HL				; restore HL
		RET					; score must have been the same so do nothing
		
updHiScr
		PUSH DE
		PUSH BC
		
		LD HL, score
		LD DE, hiscr
		LD BC,6
		LDIR
		
		LD A,22				; now to display the high score:
		RST 16
		LD A, 21			; vertical coord
		RST 16
		LD A, 13			; horizontal coord
		RST 16
		LD A,7				; non-BRIGHT white ink
		LD (23695),A		; ATTR-T
		LD DE,hiscr			; point HL at the high score
		LD BC, 6			; 6 ASCII "digits" in the score
		CALL $203C
		
		POP BC
		POP DE
		
		;POP HL				; restore HL
		
		RET
	   
; *******************************************************************
; *																	*
; * Utility routine: 	Waits for a key to be pressed routine		*
; *																	*
; *******************************************************************	   

F_pressAnyKey:
       LD HL,23560			; LAST K system variable.
       LD ( HL ),0			; put null value there.
keyloop   LD A,( HL )			; new value of LAST K.
       CP 0					; is it still zero?
       JR z,keyloop			; yes, so no key pressed.
       RET					; key was pressed.