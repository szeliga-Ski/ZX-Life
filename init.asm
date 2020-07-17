;INCLUDE "subroutines.asm"

JP init

; *******************************************************************
; *																	*
;{ * Subroutines													*
; *																	*
; *******************************************************************

F_run:	;{ the main loop

	; *********** set the static "Generations=" label ***************
	LD HL, xPosStr			; set print x coord
	LD (HL), 17
	LD HL, yPosStr			; set print y coord
	LD (HL), 0
	LD DE, strGeneration	; point at string
	LD BC, GENERATION_LEN	; set string length
	CALL F_printStr			; display "Generation="

	; *********** set up loop control *******************************
	LD B, ITERATIONS		; initialise number of generations
	LD C, 0		

runLoop:	
	PUSH BC					; store generation value
	CALL F_displayGenNum	; display generation number	
	LD A,B				; see if we need to pack ==> if B<10 then print a space
	CP 10
	JR C, contLoop
	LD A,SPACE			; SPACE character.
	RST 16              ; display space.
	
contLoop:	
	LD B, GRID_SIZE		; outer loop -- row counter
outerLoop:
	PUSH BC				; store outer loop counter 'B'
	LD A, B
	LD (outer),A		; store outer loop counter in RAM too
	
	; reset array2 if this is the start of a population
	CP GRID_SIZE
	JR NZ, contOuterLoop
	LD HL, array2		; HL = start address of block
	LD E,L				; DE = HL + 1
	LD D,H	
	INC DE
	LD (HL), 0			; initialise first byte of block
	LD BC, 64			; BC = length of block in bytes
	LDIR				; fill memory
	
contOuterLoop:
	CALL F_displayYCoord ; display the current y coord	
	LD B, GRID_SIZE		; inner loop - column counter
innerLoop:
	PUSH BC				; preserve inner loop counter
	LD A, B
	LD (inner), A		; store inner loop counter in RAM too
	CALL F_displayXCoord
	LD HL, count		; reset the count of adjacent cells
	LD (HL), 0			;
	
	; *******************************************************************************
	; calculate offset into array1 = array1 + GRID_SIZE*(outerLoop-1) + (innerLoop-1)
	LD D, GRID_SIZE	; D = number of elements in a row
	LD A, (outer)		; get a copy of the outer loop counter
	DEC A
	LD E, A				; E = -1 + outer loop counter which was in B when written out	
	CALL F_Multiply		; HL = GRID_SIZE*(outerLoop-1) = D * E	
	LD A, L				; get result
	LD HL, (inner)		; add inner loop
	ADD A, L			; ...
	DEC A				; and subtract 1 - we now have the offset!
	LD (arrayOffset), A	; store it in RAM

	; *******************************************************************************
	
	; we are starting at the bottom right of the board
	; and working up to the top left because of the way
	; the loop counters work!
checkLeft:				; *************		check left		*************
	POP BC				; restore inner loop counter
	PUSH BC				; ... and immediately preserve
	LD A, B				; skip 'checkLeft' if we are at
	CP 1 ;0				; the left boundary otherwise
	JR Z, checkRight	; continue & check to the left	
	LD D, 0				; load the array element at
	LD A, (arrayOffset) ; the current offset
	DEC A				; look to the left!
	LD E, A				;
	LD HL, array1		;
	ADD HL, DE			;
	LD A, (HL)			; A contains array element to the left	
	CP TRUE				; see if it is set
	JR Z, clIncCount	; it was set so jump and add 1 to the count
	JP checkRight		; it was not set so go to next check
	
clIncCount:
	LD HL, count		; increment the count
	LD A, (HL)			; for the current
	INC A				; array item and then
	LD (HL),A			; store updated count
	
checkRight:				; *************		check right		*************
	POP BC				; restore inner loop counter
	PUSH BC				; ... and immediately preserve
	LD A, B				; skip 'checkRight' if we are at
	CP GRID_SIZE		; the right boundary otherwise
	JR Z, checkUp		; continue & check right	
	LD D, 0				; load the array element at
	LD A, (arrayOffset) ; the current offset
	INC A				; look to the right!
	LD E, A				;
	LD HL, array1		;
	ADD HL, DE			;
	LD A, (HL)			; A contains array element to the right	
	CP TRUE				; see if it is set
	JR Z, crIncCount	; it was set so jump and add 1 to the count
	JP checkUp			; it was not set so go to next check
crIncCount:
	LD HL, count		; increment the count
	LD A, (HL)			; for the current
	INC A				; array item and then
	LD (HL),A			; store updated count
	
checkUp:				; *************		check up	 	**************
	POP BC				; restore inner loop counter
	PUSH BC				; ... and immediately preserve	
	LD A, (outer)		; check outer loop counter
	CP 1				; and skip this if we are on
	JR Z, checkDown		; the first row
	LD D, GRID_SIZE		; load the array element at
	LD A, (arrayOffset) ; the current offset
	SUB D				; subtract GRID_SIZE to look up a row
	LD E, A				;
	LD D, 0				;
	LD HL, array1		;
	ADD HL, DE			;
	LD A, (HL)			; A contains array element above	
	CP TRUE				; see if it is set
	JR Z, cuIncCount	; jump & increment count
	JP checkDown		; not set, so go to next check	
cuIncCount:
	LD HL, count		; increment the count
	LD A, (HL)			; for the current
	INC A				; array item and then
	LD (HL),A			; store updated count
	
checkDown:				; *************		check down		*************
	POP BC				; restore inner loop counter
	PUSH BC				; ... and immediately preserve	
	LD A, (outer)		; check outer loop counter
	CP GRID_SIZE		; and skip this if we are on
	JR Z, checkUpLeft	; the first row
	LD D, GRID_SIZE		; load the array element at
	LD A, (arrayOffset) ; the current offset
	ADD A, D			; add GRID_SIZE to look down a row
	LD E, A				;
	LD D, 0				;
	LD HL, array1		;
	ADD HL, DE			;
	LD A, (HL)			; A contains array element above	
	CP TRUE				; see if it is set
	JR Z, cdIncCount	; jump & increment count
	JP checkUpLeft		; not set, so go to next check	
cdIncCount:
	LD HL, count		; increment the count
	LD A, (HL)			; for the current
	INC A				; array item and then
	LD (HL),A			; store updated count

checkUpLeft:		 ; *************  check up & left	*************
	; skip this check if we are on the top row or in the leftmost column
	POP BC				; restore inner loop counter
	PUSH BC				; ... and immediately preserve	
	LD A, (outer)		; check outer loop counter
	CP 1				; and skip this if we are on
	JR Z, checkUpRight	; the first row
	LD A, (inner)		; check for 1st column
	CP 1				;
	JR Z, checkUpRight	; go to next check
	LD D, GRID_SIZE		; load the array element at
	LD A, (arrayOffset) ; the current offset
	SUB D				; subtract GRID_SIZE to look up a row	
	DEC A				; subtract 1 to look left one space
	LD E, A				;
	LD D, 0				;
	LD HL, array1		;
	ADD HL, DE			;
	LD A, (HL)			; A contains array element above	
	CP TRUE				; see if it is set
	JR Z, culIncCount	; jump & increment count
	JP checkUpRight		; not set, so go to next check	
culIncCount:
	LD HL, count		; increment the count
	LD A, (HL)			; for the current
	INC A				; array item and then
	LD (HL),A			; store updated count

checkUpRight:		 ; *************	 check up & right   *************
	; skip this check if we are on the top row or in the rightmost column
	POP BC				; restore inner loop counter
	PUSH BC				; ... and immediately preserve	
	LD A, (outer)		; check outer loop counter
	CP 1				; and skip this if we are on
	JR Z, checkDownLeft	; the first row
	LD A, (inner)		; check for final column
	CP GRID_SIZE		;
	JR Z, checkDownLeft	; go to next check
	LD D, GRID_SIZE		; load the array element at
	LD A, (arrayOffset) ; the current offset
	SUB D				; subtract GRID_SIZE to look up a row	
	INC A				; add 1 to look right one space
	LD E, A				;
	LD D, 0				;
	LD HL, array1		;
	ADD HL, DE			;
	LD A, (HL)			; A contains array element above	
	CP TRUE				; see if it is set
	JR Z, curIncCount	; jump & increment count
	JP checkDownLeft	; not set, so go to next check	
curIncCount:
	LD HL, count		; increment the count
	LD A, (HL)			; for the current
	INC A				; array item and then
	LD (HL),A			; store updated count
	

checkDownLeft:		 ; *************	 check down & left  *************
	; skip this check if we are on the bottom row or in the leftmost column
	POP BC				; restore inner loop counter
	PUSH BC				; ... and immediately preserve	
	LD A, (outer)		; check outer loop counter
	CP GRID_SIZE		; and skip this if we are on
	JR Z, checkDownRight; the last row
	LD A, (inner)		; check for 1st column
	CP 1				;
	JR Z, checkDownRight; go to next check
	LD D, GRID_SIZE		; load the array element at
	LD A, (arrayOffset) ; the current offset
	ADD A, D			; add GRID_SIZE to look down a row	
	DEC A				; subtract 1 to look left one space
	LD E, A				;
	LD D, 0				;
	LD HL, array1		;
	ADD HL, DE			;
	LD A, (HL)			; A contains array element above	
	CP TRUE				; see if it is set
	JR Z, cdlIncCount	; jump & increment count
	JP checkDownRight	; not set, so go to next check	
cdlIncCount:
	LD HL, count		; increment the count
	LD A, (HL)			; for the current
	INC A				; array item and then
	LD (HL),A			; store updated count

checkDownRight:		 ; *************	 check down & right *************
	; skip this check if we are on the bottom row or in the rightmost column
	POP BC				; restore inner loop counter
	PUSH BC				; ... and immediately preserve	
	LD A, (outer)		; check outer loop counter
	CP GRID_SIZE		; and skip this if we are on
	JR Z, endOfChecks	; the last row
	LD A, (inner)		; check for 1st column
	CP GRID_SIZE		;
	JR Z, endOfChecks	; go to next check
	LD D, GRID_SIZE		; load the array element at
	LD A, (arrayOffset) ; the current offset
	ADD A, D			; add GRID_SIZE to look up a row	
	INC A				; add 1 to look right one space
	LD E, A				;
	LD D, 0				;
	LD HL, array1		;
	ADD HL, DE			;
	LD A, (HL)			; A contains array element above	
	CP TRUE				; see if it is set
	JR Z, cdrIncCount	; jump & increment count
	JP endOfChecks		; not set, so go to next check	
cdrIncCount:
	LD HL, count		; increment the count
	LD A, (HL)			; for the current
	INC A				; array item and then
	LD (HL),A			; store updated count

endOfChecks: ; now decide whether it progresses to next generation
	CALL F_displayCount ; debug	

	; if cell=0 but count=3 then it might propagate
	
	LD A, (arrayOffset)
	LD HL, array1
	LD D, 0
	LD E, A
	ADD HL, DE
	LD A, (HL)			; A contains current cell value
	CP 0				; is it dead?
	JR Z, ec1			; cell contains 0, so now check for count=3
	JR ec2				; it is alive, so do other checks instead of this one
ec1:					; cell is dead
	LD A, (count)
	CP 3
	JR Z, copy3			; yes, count=3 so need to copy
	JR nextItem			; dead and count!=3 so go to next item
	
	; ******** if count<2 then do not copy
ec2:
	; ******** if count=2 or count=3 then copy
	LD A, (count)
	CP 2
	JR Z, copy2
	
	LD A, (count)
	CP 3
	JR Z, copy3
	
	JR nextItem
copy2:
copy3:
	LD HL, array2		; point HL at array 2
	LD A, (arrayOffset)	; get offset
	LD D, 0				;
	LD E, A				;
	ADD HL, DE			; add it to HL
	LD (HL), TRUE		; set new population member
	JR nextItem

nextItem:
	;CALL F_pressAnyKey		; debug
	;CALL F_displayArray2	; display the initial population
	;CALL F_pressAnyKey		; debug

	
	POP BC				; restore inner loop counter
	DEC B
	JP NZ, innerLoop

	POP BC				; restore the outer loop counter
	DEC B
	JP NZ, outerLoop

	; outer loop is finished so copy array2 to array1
	;CALL F_pressAnyKey		; debug
	LD HL, array2
	LD DE, array1
	LD BC, 64			; BC = length of block in bytes
	LDIR				; fill memory DE=HL
	
	;CALL F_pressAnyKey		; debug
	CALL F_displayArray1	; display the initial population
	;CALL F_pressAnyKey		; debug
	;CALL F_displayArray2	; display the initial population
	;CALL F_pressAnyKey		; debug
	
	
	
	POP BC				; restore the generation value
	DEC B
	JP NZ, runLoop

	CALL F_displayArray1	; display the initial population
	;CALL F_displayArray2	; display the initial population
	
	; print "Press 1 to run again, 5 to exit"
	LD HL, xPosStr				; set print x coord
	LD (HL), 0
	LD HL, yPosStr				; set print y coord
	LD (HL), 18
	LD DE, strEndMessage		; point at string
	LD BC, ENDMESSAGE_LEN		; set string length
	LD A, WHITE					; set print colour
	LD (printColour), A			;
	CALL F_printStr				; display "Press 1 to ..."
	LD A, GREEN					; reset print colour
	LD (printColour), A			;
	CALL F_pressAnyKey			; wait for a key press
	; delete "Press 1 ..." message
	LD HL, xPosStr				; set print x coord
	LD (HL), 0
	LD HL, yPosStr				; set print y coord
	LD (HL), 18
	LD DE, strBlankMessage		; point at blank string
	LD BC, ENDMESSAGE_LEN		; set string length
	CALL F_printStr				; print blank string
	
J_checkKeys:
	LD BC,63486       	; keyboard row 1-5/joystick port 2.
    IN A,(C)            ; see what keys are pressed.
    RRA                 ; outermost bit = key 1.
    JP NC, init		    ; it's being pressed, go back to the beginning
    RRA                 ; next bit along (value 2) = key 2.
    RRA                 ; next bit (value 4) = key 3.
    RRA                 ; next bit (value 8) reads key 4.
	RRA                 ; next bit (value 8) reads key 5.
    JR NC, J_end        ; it's being pressed => exit

J_end:	
	RET

;}
 
	
; *****************************************************************	

F_printCell:	;{ prints a UDG based on the value of A
		LD A,C				; decide whether to print UDG for 0 or 1
		CP FALSE			; see if A contains 0
		JR NZ, notZero
		LD A, UDG_A			; ASCII code for User Defined Graphic 'A'
		RST 16				; draw blank cell
		RET					; done
	notZero
		LD A, UDG_B
		RST 16				; print populated cell		
		RET					; done
;}  end of F_printCell


; *******************************************************************
; *																	*
; * Utility routine: 	Waits for a key to be pressed routine		*
; *																	*
; *******************************************************************	   

F_pressAnyKey:
	PUSH AF	; preserve A
	PUSH BC
	PUSH DE

       LD HL,23560			; LAST K system variable.
       LD ( HL ),0			; put null value there.
keyloop   LD A,( HL )			; new value of LAST K.
       CP 0					; is it still zero?
       JR z,keyloop			; yes, so no key pressed.
	   
	POP DE
	POP BC
	POP AF
       RET					; key was pressed.
	   
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


	
F_blackScreen:
;{ Set up the bold font
	CALL F_boldFont
	LD HL,blocks        ; address of user-defined graphics data.
	LD (23675),HL       ; make UDGs point to it.
;}
;{ We want a black screen.
	LD A,GREEN     		; green ink (4) on black paper (0), bright (64).
	LD (23693),A        ; set our screen colours - ATTR-P
	XOR A               ; quick way to load accumulator with zero.
	CALL $229B 			; 8859 - set permanent border colours.
;}

;{ Clear the screen
	CALL $0DAF          ; ROM routine - clears screen, opens chan 2.
	LD A,2				; upper screen
	CALL $1601			; open channel		
;}

	LD HL, xPosStr				; set print x coord
	LD (HL), 4
	LD HL, yPosStr				; set print y coord
	LD (HL), 21
	LD DE, strBasedOn			; point at string
	LD BC, BASED_ON_LEN			; set string length
	CALL F_printStr				; display "Based on ..."
	
	;CALL F_pressAnyKey
	RET
;} end of F_blackScreen


;{ Display a message with 	DE=string address,
;							BC=string length
;			and xPosStr & yPosStr must be set
;							
F_printStr:
	PUSH BC
	PUSH DE
	
	LD A,22				;{ print AT control code
	RST 16				;
	
	LD HL, yPosStr		; set vert pos
	LD A, (HL)
	RST 16				;
	
	LD HL, xPosStr		; set horiz pos
	LD A, (HL)
	RST 16				;
	
	LD A,GREEN			; in green ink
	LD (23695),A		; ATTR-T

	CALL $203C			;}
	
	POP DE
	POP BC
	RET
;}

;{ Create initial population
F_initPop:
	PUSH BC				; preserve registers
	PUSH AF				; 
	PUSH DE				; 
	LD B,POP_SIZE		; loop for each population member
	
initPop:
	
	call F_random		; puts a random number in A
	AND GRID_MASK		; make sure it is less than the grid size
	LD D, A				; store column in D
	call F_random		; puts a random number in A for row number
	AND GRID_MASK		; make sure it is less than the grid size
	RLCA				; multiply row by 8 since there are 8 elements (GRID_SIZE)
	RLCA
	RLCA				; A now points into the correct row
	ADD A, D			; add column offset => A points at correct array index
	LD HL, array1		; point HL at the start of the array
	LD D,0				; load DE with the offset into the array
	LD E,A
	ADD HL, DE			; HL now points at the array item
	LD A, (HL)			; see if current array item has already been set
	OR A				; see if A is 1
	JP NZ, initPop		; ... it was already set so try again...
	LD (HL), TRUE		; set the current array item to be populated
	
	DJNZ initPop		; continue loop
	
	CALL F_displayArray1	; display the initial population
	;CALL F_displayArray2	; display the initial population
	
	POP DE				; restore registers
	POP AF				;
	POP BC				;
	RET
;}

F_displayArray1:	;{ debug routine that prints the contents of the array1 table
	PUSH IX
	PUSH BC
	PUSH DE
	
	LD B, GRID_SIZE
	LD IX, array1
	
	dtLoop
		PUSH BC
		
		LD A,22				; column 0 ************************
		RST 16
		LD A,GRID_SIZE		; vertical position
		SUB B				; calculate vertical print coord
		RST 16				; set it
		LD A,1				; horizontal position
		RST 16				; set it
		LD A,GREEN			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX)			; load BC with the 'number' to print
		LD B,0
		CALL F_printCell
		POP BC
		
		LD A,22				; column 1 ************************
		RST 16
		LD A,GRID_SIZE		; vertical position
		SUB B
		RST 16
		LD A,2				; horizontal position
		RST 16
		LD A,CYAN			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+1)
		LD B,0
		CALL F_printCell
		POP BC
		
		LD A,22				; column 2 ************************
		RST 16
		LD A,GRID_SIZE		; vertical position
		SUB B
		RST 16
		LD A,3				; horizontal position
		RST 16
		LD A,RED			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+2)
		LD B,0
		CALL F_printCell
		POP BC
		
		LD A,22				; column 3 ************************
		RST 16
		LD A,GRID_SIZE		; vertical position
		SUB B
		RST 16
		LD A,4				; horizontal position
		RST 16
		LD A,MAGENTA			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+3)
		LD B,0
		CALL F_printCell
		POP BC
		
		LD A,22				; column 4 ************************
		RST 16
		LD A,GRID_SIZE		; vertical position
		SUB B
		RST 16
		LD A,5				; horizontal position
		RST 16
		LD A,WHITE			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+4)
		LD B,0
		CALL F_printCell
		POP BC
		
		LD A,22				; column 5 ************************
		RST 16
		LD A,GRID_SIZE		; vertical position
		SUB B
		RST 16
		LD A,6				; horizontal position
		RST 16
		LD A,YELLOW			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+5)
		LD B,0
		CALL F_printCell
		POP BC
		
		LD A,22				; column 6 ************************
		RST 16
		LD A,GRID_SIZE		; vertical position
		SUB B
		RST 16
		LD A,7				; horizontal position
		RST 16
		LD A,GREEN			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+6)
		LD B,0
		CALL F_printCell
		POP BC
		
		LD A,22				; column 7 ************************
		RST 16
		LD A,GRID_SIZE		; vertical position
		SUB B
		RST 16
		LD A,8				; horizontal position
		RST 16
		LD A,CYAN			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+7)
		LD B,0
		CALL F_printCell
		POP BC
		
		LD DE,GRID_SIZE
		ADD IX, DE
		POP BC
		;DJNZ dtLoop
		DEC B				; see if B contains 0, loop if B>0
		LD A,B 				; 
		OR A				; compares A with 0
		JP NZ, dtLoop		; loop if necessary
		
	POP DE
	POP BC
	POP IX
	
	;CALL F_pressAnyKey
	RET
;}

F_displayArray2:	;{ debug routine that prints the contents of the array2 table
	PUSH IX
	PUSH BC
	PUSH DE
	
	LD B, GRID_SIZE
	LD IX, array2
	
	dtLoop2:
		PUSH BC
		
		LD A,22				; column 0 ************************
		RST 16
		LD A,17	; vertical position
		SUB B				; calculate vertical print coord
		RST 16				; set it
		LD A,1				; horizontal position
		RST 16				; set it
		LD A,GREEN			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX)			; load BC with the 'number' to print
		LD B,0
		CALL F_printCell
		POP BC
		
		LD A,22				; column 1 ************************
		RST 16
		LD A,17		; vertical position
		SUB B
		RST 16
		LD A,2				; horizontal position
		RST 16
		LD A,CYAN			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+1)
		LD B,0
		CALL F_printCell
		POP BC
		
		LD A,22				; column 2 ************************
		RST 16
		LD A,17		; vertical position
		SUB B
		RST 16
		LD A,3				; horizontal position
		RST 16
		LD A,RED			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+2)
		LD B,0
		CALL F_printCell
		POP BC
		
		LD A,22				; column 3 ************************
		RST 16
		LD A,17		; vertical position
		SUB B
		RST 16
		LD A,4				; horizontal position
		RST 16
		LD A,MAGENTA			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+3)
		LD B,0
		CALL F_printCell
		POP BC
		
		LD A,22				; column 4 ************************
		RST 16
		LD A,17		; vertical position
		SUB B
		RST 16
		LD A,5				; horizontal position
		RST 16
		LD A,WHITE			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+4)
		LD B,0
		CALL F_printCell
		POP BC
		
		LD A,22				; column 5 ************************
		RST 16
		LD A,17		; vertical position
		SUB B
		RST 16
		LD A,6				; horizontal position
		RST 16
		LD A,YELLOW			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+5)
		LD B,0
		CALL F_printCell
		POP BC
		
		LD A,22				; column 6 ************************
		RST 16
		LD A,17		; vertical position
		SUB B
		RST 16
		LD A,7				; horizontal position
		RST 16
		LD A,GREEN			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+6)
		LD B,0
		CALL F_printCell
		POP BC
		
		LD A,22				; column 7 ************************
		RST 16
		LD A,17		; vertical position
		SUB B
		RST 16
		LD A,8				; horizontal position
		RST 16
		LD A,CYAN			; in yellow ink
		LD (23695),A		; ATTR-T
		PUSH BC
		LD C,(IX+7)
		LD B,0
		CALL F_printCell
		POP BC
		
		LD DE,GRID_SIZE
		ADD IX, DE
		POP BC
		;DJNZ dtLoop
		DEC B				; see if B contains 0, loop if B>0
		LD A,B 				; 
		OR A				; compares A with 0
		JP NZ, dtLoop2		; loop if necessary
		
	POP DE
	POP BC
	POP IX
	
	;CALL F_pressAnyKey
	RET
;}

;{ *****************************************************************
  
 F_displayYCoord: 
	LD A,22				; ***** display current y coord
	RST 16				; set AT code
	LD A, 2				; vertical position
	RST 16
	LD A,19				; horizontal position
	RST 16
	LD C,B				; Now print the actual y value
	LD B,0				; SWAP B and C for ROM call
	CALL 6683			; display x coord	
	LD HL, xPosStr		; set print x coord
	LD (HL), 17
	LD HL, yPosStr		; set print y coord
	LD (HL), 2
	LD DE, strYCoord	; point at string
	LD BC, YCOORD_LEN	; set string length
	CALL F_printStr		; display "y="	
	RET
	
; ******************************************************************

F_displayXCoord:
	LD A,22				; ***** display current x coord, then its label
	RST 16				; set AT code
	LD A, 1				; vertical position
	RST 16
	LD A,19				; horizontal position
	RST 16
	LD C,B				; Now print the actual x value
	LD B,0				; SWAP B and C for ROM call
	CALL 6683			; display x coord	
	LD HL, xPosStr		; set print x coord
	LD (HL), 17
	LD HL, yPosStr		; set print y coord
	LD (HL), 1
	LD DE, strXCoord	; point at string
	LD BC, XCOORD_LEN	; set string length
	CALL F_printStr		; display "x="
	RET

; ******************************************************************

F_displayOffset:
PUSH AF	; preserve A
PUSH BC
PUSH DE

;{ Display a message with 	DE=string address,
;							BC=string length
;			and xPosStr & yPosStr must be set
;							
	LD DE, str3Spaces
	LD BC, STR3SPACES_LEN
	LD A, 0
	LD (xPosStr), A
	LD A, 10
	LD (yPosStr), A
	CALL F_printStr

	LD A,22					; AT code
	RST 16
	LD A, 10				; vertical coord
	RST 16
	LD A, 0					; horizontal coord	
	RST 16
	LD HL, arrayOffset
	LD A, (HL)
	LD C, A
	LD B, 0
	CALL 6683				; display number in BC
	
POP DE
POP BC
POP AF ; restore A
	RET

; ******************************************************************

F_displayCount:
	PUSH AF	; preserve A
	PUSH BC
	PUSH DE

;{ Display a message with 	DE=string address,
;							BC=string length
;			and xPosStr & yPosStr must be set
;							
	LD DE, strCount
	LD BC, COUNT_LEN
	LD A, 17
	LD (xPosStr), A
	LD A, 3
	LD (yPosStr), A
	CALL F_printStr

	LD A,22					; AT code
	RST 16
	LD A, 3					; vertical coord
	RST 16
	LD A, 23				; horizontal coord	
	RST 16
	LD HL, count
	LD A, (HL)
	LD C, A
	LD B, 0
	CALL 6683				; display number in BC
	
	POP DE
	POP BC
	POP AF ; restore A
	RET

; ******************************************************************
	
F_displayGenNum:
	LD A,22						; AT code
	RST 16
	LD A, 0						; vertical coord
	RST 16
	LD A, 28					; horizontal coord	
	RST 16
	LD C, B						; swap B/C so that ROM routine displays OK
	LD B, 0
	CALL 6683					; display generation number in BC
	RET
	
; *******************************************************************
; *																	*
; * Utility routine: 	Multiplies HL = D * E               		*
; *																	*
; *******************************************************************	   

;{
F_Multiply:               ; this routine performs the operation HL=D*E
  ld hl,0               ; HL is used to accumulate the result
  ld a,d                ; checking one of the factors; returning if it is zero
  or a
  ret z
  ld b,d                ; one factor is in B
  ld d,h                ; clearing D (H is zero), so DE holds the other factor
MulLoop:                ; adding DE to HL exactly B times
  add hl,de
  djnz MulLoop
  ret
;}