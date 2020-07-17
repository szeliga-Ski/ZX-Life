; *******************************************************************
; *																	*
;{ * Subroutines													*
; *																	*
; *******************************************************************

INCLUDE "utils.asm"

displayArray1:	;{ debug routine that prints the contents of the invaders table
	PUSH IX
	PUSH BC
	PUSH DE
	
	LD B, GRID_SIZE
	LD IX, array1
	
	dtLoop
		PUSH BC
		
		LD A,22				; element 0
		RST 16
		LD A,GRID_SIZE
		SUB B
		RST 16
		LD A,0
		RST 16
		LD A,YELLOW			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX)
		LD B,0
		CALL 6683
		POP BC
		
		LD A,22				; element 1
		RST 16
		LD A,GRID_SIZE
		SUB B
		RST 16
		LD A,2
		RST 16
		LD A,YELLOW			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+1)
		LD B,0
		CALL 6683
		POP BC
		
		LD A,22				; element 2
		RST 16
		LD A,GRID_SIZE
		SUB B
		RST 16
		LD A,4
		RST 16
		LD A,YELLOW			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+2)
		LD B,0
		CALL 6683
		POP BC
		
		LD A,22				; element 3
		RST 16
		LD A,GRID_SIZE
		SUB B
		RST 16
		LD A,6
		RST 16
		LD A,YELLOW			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+3)
		LD B,0
		CALL 6683
		POP BC
		
		LD A,22				; element 4
		RST 16
		LD A,GRID_SIZE
		SUB B
		RST 16
		LD A,8
		RST 16
		LD A,YELLOW			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+4)
		LD B,0
		CALL 6683
		POP BC
		
		LD A,22				; element 5
		RST 16
		LD A,GRID_SIZE
		SUB B
		RST 16
		LD A,10
		RST 16
		LD A,YELLOW			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+2)
		LD B,0
		CALL 6683
		POP BC
		
		LD A,22				; element 6
		RST 16
		LD A,GRID_SIZE
		SUB B
		RST 16
		LD A,12
		RST 16
		LD A,YELLOW			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+5)
		LD B,0
		CALL 6683
		POP BC
		
		LD A,22				; element 7
		RST 16
		LD A,GRID_SIZE
		SUB B
		RST 16
		LD A,14
		RST 16
		LD A,YELLOW			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+6)
		LD B,0
		CALL 6683
		POP BC
		
		LD DE,3
		ADD IX, DE
		POP BC
		;DJNZ dtLoop
		CP B	; see if B is 0
		JP NZ, dtLoop
		
	POP DE
	POP BC
	POP IX
	
	CALL F_pressAnyKey
	RET
;}

; ;{ Move player left.

; mpl
	; LD HL,ply			; remember, y is the horizontal coord!
	; LD A,(HL)			; what's the current value?
	; AND A				; is it zero?
	; RET Z				; yes - we can't go any further left.
	; DEC (HL)				; subtract 1 from y coordinate.
	; RET
; ;}

; ;{ Move player right.

; mpr
	; LD HL,ply			; remember, y is the horizontal coord!
	; LD A,(HL)			; what's the current value?
	; CP 31				; is it at the right edge (31)?
	; RET Z				; yes - we can't go any further right.
	; INC (HL)				; ADD 1 to y coordinate.
	; RET
; ;}

; ;{ Move player up.

; mpu
	; LD HL,plx			; remember, x is the vertical coord!
	; LD A,(HL)			; what's the current value?
	; CP 18					; is it at upper limit (4)?
	; RET Z				; yes - we can go no further then.
	; DEC (HL)				; subtract 1 from x coordinate.
	; RET
; ;}

; ;{ Move player down.

; mpd
	; LD HL,plx			; remember, x is the vertical coord!
	; LD A,(HL)			; what's the current value?
	; CP 18				; is it already at the bottom (18)?
	; RET Z				; yes - we can't go down any more.
	; INC (HL)				; ADD 1 to x coordinate.
	; RET
; ;}

; ;{  fire a missile

; fire
	; LD A, (pbx)			; bullet's vertical coord
	; INC A				; 255 is default value, increment to zero
	; RET NZ				; bullet on-screen, can't fire again
	; LD HL, (plx)		; player coordinates
	; DEC L				; 1 square higher up the screen
	; LD (pbx), HL		; set bullet coordinates
	; RET
; ;}


; ;{ See if a player bullet has hit an invader
; bchk
	; LD A, (pbx)			; bullet's vertical coord
	; INC A				; return if no bullet on-screen
	; RET Z
	; LD BC, (pbx)		; get coord's
	; CALL atadd			; find attribute here
	; CP WHITE			; space invaders are bright white on black
	; JR Z, hinv			; hit a space invader!
	; RET
		
; hinv					; disable the invader and bullet; update score
	; CALL sound1			; play sound effect for hitting an invader
	; LD A,22				; AT control code
	; RST 16
	; LD A,(pbx)			; bullet vertical coord
	; RST 16
	; LD A, (pby)			; bullet horizontal coord
	; RST 16
	; CALL wspace			; set INK colour to cyan
; ;}
	
	; ;{ also kill the invader that was hit
	; ;  loop through the table of invaders and find the one that
	; ;  has the same coords as the bullet and then set its 
	; ;  status to 255 to disable it
	
	; ; LD B,MAX_INVADERS_PER_ROW	
	; ; SLA B				; TWO rows of invaders
	; ; SLA B				; THREE rows of invaders
	; LD B, 30				; Three rows of 10 invaders
	; LD IX, segmnt
; search
	; PUSH BC				; preserve loop counter
	; LD A, ( IX )		; load status byte of current invader
	; INC A				; see if the status is 255
	; JR Z,cont			; yes - invader is inactive so ignore
	; LD A, ( IX+1 )		; invader vertical coord
	; LD HL,pbx			; player bullet vertical coord
	; CP (HL)				; compare vertical coord's
	; JR NZ,cont			; vertical coordinates are different so ignore
	; LD A, ( IX+2 )		; invader horizontal coordinate
	; LD HL, pby			; player bullet horizontal coordinate
	; CP (HL)				; compare horizontal coordinates
	; JR NZ, cont			; horizontal coordinates are different so ignore
	; ; we now have the invader that was hit!
	; LD (IX),INACTIVE	; disable the invader
	; POP BC
	; JP kilbul			; we've finished searching for the invader that was hit
; cont
	; LD DE, 3
	; ADD IX, DE
	; POP BC
	; DJNZ search
	
; kilbul	
	; LD A, INACTIVE		; x-coord of 255 switches bullet off
	; LD (pbx),A			; destroy bullet
	
	; LD B, 1				; add one to the player's score per invader
	; LD HL,score+5		; point HL at the digit we want to add one to
	; CALL uscor			; update the player's "score" variable
	
	; LD A,22				; now to display the player's score:
	; RST 16
	; LD A, 21			; vertical coord
	; RST 16
	; LD A, 1				; horizontal coord
	; RST 16
	; LD A,7				; non-BRIGHT white ink
	; LD (23695),A		; ATTR-T
	; LD DE,score			; point HL at the score
	; LD BC, 6			; 6 ASCII "digits" in the score
	; CALL $203C
	
	; CALL hscor			; see if we need to update the high score
	
	; ;CALL displayTable ; *** DEBUG ***
	
	; RET
; ;}
		
; ;{ move the bullet up the screen one character at a time

; moveb
	; LD A, (pbx)
	; INC A
	; RET Z				; there's no bullet on the screen
	; SUB 2				; one row up (we already added one to test A)
	; LD (pbx),A
	; RET
; ;}

; ;{ Set up the x AND y coordinates for the player's gunbase position,
; ;  this routine is called prior to display AND deletion of gunbase.

; basexy
	; LD A,22             ; AT code.
	; RST 16
	; LD A,(plx)          ; player vertical coord.
	; RST 16              ; set vertical position of player.
	; LD A,(ply)          ; player's horizontal position.
	; RST 16              ; set the horizontal coord.
	; RET
; ;}
	   
; ;{ Show player at current print position.

; splayr
	; LD A,GREEN			; green ink (4) on black paper (0), bright (64).
	; LD (23695),A        ; set our temporary screen colours.
	; LD A,UDG_A			; ASCII code for User Defined Graphic 'A'.
	; RST 16              ; draw player.
	; RET
; ;}

; ;{ Display player bullet   
; pbull
	; LD A, (pbx)			; bullet vertical coord
	; INC A
	; RET Z				; no bullet on the screen
	; CALL bullxy
	; LD A,16				; INK control character
	; RST 16
	; LD A,6				; yellow
	; RST 16
	; LD A,UDG_D			; UDG 'D' for player bullets
	; RST 16
	; RET
; ;}

; ;{ Delete player's bullet		
; dbull
	; LD A, (pbx)
	; INC A
	; RET Z				; no bullet
	; CALL bullxy			; set PRINT coordinates of bullet
; wspace
	; LD A,CYAN           ; cyan ink (5) on black paper (0), bright (64).
	; LD (23695),A        ; set our temporary screen colours.
	; LD A,SPACE			; SPACE character.
	; RST 16              ; display space.
	; RET
; ;}
		
; ;{ Set up the x and y coordinates for the player's bullet position,
; ;  this routine is called prior to display and deletion of bullets.

; bullxy
	; LD A,22             ; AT code.
	; RST 16
	; LD A,(pbx)          ; player bullet vertical coord.
	; RST 16              ; set vertical position of player.
	; LD A,(pby)          ; bullet's horizontal position.
	; RST 16              ; set the horizontal coord.
	; RET
; ;}

; ;{ See if we need to drop invaders down a row and reverse their direction

; dropAndReverse

	; ; if we are going RIGHT, see if anybody is in column 31
	
	; LD IX, segmnt		; point at invaders table
	; ; LD B, MAX_INVADERS_PER_ROW	; set up loop counter
	; ; SLA B				; TWO rows of invaders
	; ; SLA B				; THREE rows of invaders
	; LD B, 30				; Three rows of 10 invaders
	; LD HL, invDirection	; point HL at direction variable
	; LD A, (HL)
	; CP RIGHT
	; JP NZ, checkLeft	; we're not going right so go left!
	
	; ; OK  - we're going right then so loop to see if any of the
	; ; invaders is in column 31. If so, set the "reverse direction" flag	
; dAndRLoopRight
	; PUSH BC				; preserve loop counter	
	; LD A, ( IX )		; load status byte of current invader
	; INC A				; see if the status is 255
	; JR Z,cont0			; yes - invader is inactive so ignore this invader	
	; LD A, ( IX+2 )		; invader horizontal coordinate
	; CP 31				; we are at the right edge of the display
	; JR NZ, cont0		; not at right edge so continue searching
	; ; we now have an invader at the right edge of the display so set flag
	; LD HL, flagDropAndFlip
	; LD ( HL ), TRUE
	; LD HL, invDirection	; reverse the invader direction
	; LD A, ( HL )		
	; XOR 1
	; LD ( HL ), A		; store the reversed direction
	; POP BC
	; RET					; we found one so return
	
; cont0
	; LD DE, 3
	; ADD IX, DE
	; POP BC
	; DJNZ dAndRLoopRight
	; RET					; none of the invaders going right is at the edge
	
; checkLeft
	; ; if we are going LEFT, see if anybody is in column 0
; dAndRLoopLeft
	; PUSH BC				; preserve loop counter	
	; LD A, ( IX )		; load status byte of current invader
	; INC A				; see if the status is 255
	; JR Z,cont1			; yes - invader is inactive so ignore this invader	
	; LD A, ( IX+2 )		; invader horizontal coordinate
	; CP 0				; we are at the right edge of the display
	; JR NZ, cont1		; not at right edge so continue searching
	; ; we now have an invader at the right edge of the display so set flag
	; LD HL, flagDropAndFlip
	; LD ( HL ), TRUE
	; LD HL, invDirection	; reverse the invader direction
	; LD A, ( HL )		
	; XOR 1
	; LD ( HL ), A		; store the reversed direction
	; POP BC
	; RET					; we found one so return
	
; cont1
	; LD DE, 3
	; ADD IX, DE
	; POP BC
	; DJNZ dAndRLoopLeft
	; RET					; none of the invaders going right is at the edge
; ;}

; ;{ drops every invader down a row
; dropDown
	; ; LD B, MAX_INVADERS_PER_ROW	; loop counter
	; ; SLA B				; TWO rows of invaders
	; ; SLA B				; THREE rows of invaders
	; LD B, 30				; Three rows of 10 invaders
	; LD IX, segmnt		; point at invaders table
	
; dropDown0
	; PUSH BC				; preserve loop counter	
	; LD A, ( IX )		; load status byte of current invader
	; INC A				; see if the status is 255
	; JR Z,contDD			; yes - invader is inactive so ignore this invader
	
	; ; erase this invader before it is moved
	; CALL invcol			; new invader position collision check
	; CALL invxy          ; set up invader coordinates.			
	; CALL wspace         ; display A space, cyan ink on black.
	; CALL invcol			; new invader position collision check
	
	; LD A, ( IX+1 )		; invader vertical coordinate
	; CP 18				; player is on row 18
	; JR Z, dd1			; at bottom of play area
	
	; LD A, ( IX+1)		; increment the vertical coordinate
	; INC A				;
	; LD ( IX+1), A		;
	; CALL invcol			; new invader position collision check
	; JR contDD			;
	
; dd1	; we are going to move the invader to the top of the screen
	; LD ( IX + 1), 0
	
; contDD
	; LD DE, 3
	; ADD IX, DE
	; POP BC
	; DJNZ dropDown0
	
	; LD HL, flagDropAndFlip	; reset flag to drop down
	; LD ( HL ), FALSE	;
	; RET					; all invaders moved down
; ;}

; ;{ Set print coordinates for an invader pointed to by IX

; invxy
	; LD A,22             ; ASCII code for AT character.
	; RST 16              ; display AT code.
	; LD A,(IX+1)         ; get segment x coordinate.
	; RST 16              ; display coordinate code.
	; LD A,(IX+2)         ; get segment y coordinate.
	; RST 16              ; display coordinate code.
	; RET
; ;}
; ; ******************************************************************

; proinv	
	; CALL invcol			; invader collision detection		
	; LD A,(IX)           ; check if invader was switched off
	; INC A               ; by collision detection routine.
	; RET Z               ; it was, so this invader is now dead.
	
	; CALL invxy          ; set up invader coordinates.			
	; CALL wspace         ; display A space, cyan ink on black.
	; CALL invmov         ; move invader.
	; CALL invcol			; new invader position collision check
	
	; LD A,(IX)           ; check if invader was switched off
	; INC A               ; by collision detection routine.
	; RET Z               ; it was, so this invader is now dead.
	; CALL invxy          ; set up invader coordinates.
	; LD A,WHITE			; attribute code 7 = white invader.
	; LD (23695),A        ; set temporary attributes.
	; LD A,UDG_C			; UDG 'C' to display invader.
	; RST 16
	; RET
	   
; invmov
	; LD A, (invDirection) ; see which direction the invaders are moving
	; AND A				; LEFT=0, RIGHT=1	
	; JR Z,segml          ; going left - jump to that bit of code.

; ;{ so segment is going right then!

; segmr
	; LD A,(IX+2)			; horizontal coord.
	; INC (IX+2)          ; no obstacles, so move right.
	; RET
; ;}


; ;{ so segment is going left then!

; segml
	; LD A,(IX+2)         ; y coord.
	; DEC (IX+2)          ; no obstacles, so move left.
	; RET
; ;}


; ;{ Space invader collision detection.
; ;  Checks for collisions with player and player's bullets.

; invcol
	; LD A,(ply)          ; player y position.
	; CP ( IX+2 )			; is it identical to segment y coord?
	; JR NZ,bulcol		; y coords differ, try bullet instead.
	; LD A,(plx)			; player x coord.
	; CP ( IX+1 )			; same as segment?
	; JR NZ,bulcol		; x coords differ, try bullet instead.

; ; So we have a collision with the player.

; killpl
	; LD (dead),A         ; set flag to say that player is now dead.
	; RET

; ; Next let's check for a collision with the player's bullet.

; bulcol
	; LD A,(pbx)          ; bullet x coords.
	; INC A				; at default value?
	; RET Z				; yes, no bullet to check for.
	; DEC A ; *** REMOVE TEST? ***
	; CP ( IX+1 )			; is bullet x coord same as segment x coord?
	; RET NZ				; no, so no collision.
	; LD A,(pby)          ; bullet y position.
	; CP ( IX+2 )			; is it identical to segment y coord?
	; RET NZ			 	; no - no collision this time.

; ; So we have a collision with the player's bullet.

	; LD ( IX ),INACTIVE	; kill the invader
	; CALL sound1			; invader hit sound effect
	; CALL dbull			; delete bullet from the screen
	; CALL kilbul         ; kill the bullet
	; RET
; ;}

; prPlayerDead
	; LD A,22				;{ print message 1
	; RST 16				;
	; XOR A				;  vert pos
	; RST 16				;
	; XOR A				;  horiz pos
	; RST 16				;
	; LD A,RED			; in red ink
	; LD (23695),A		; ATTR-T
	; LD DE, strPlyDead
	; LD BC, 32			; 32 characters
	; CALL $203C			;}
	; RET