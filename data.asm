; *******************************************************************
; *																	*
;{ * Data															*
; *																	*
; *******************************************************************

BLACK					EQU		64		; ATTR-P bright black ink on black paper
BLUE					EQU		65		; ATTR-P bright blue ink on black paper
RED						EQU		66		; ATTR-P bright red ink on black paper
MAGENTA					EQU		67		; ATTR-P bright magenta ink on black paper
GREEN					EQU		68		; ATTR-P bright green ink on black paper
CYAN					EQU		69		; ATTR-P bright cyan ink on black paper
YELLOW					EQU		70		; ATTR-P bright yellow ink on black paper
WHITE					EQU		71		; ATTR-P bright white ink on black paper

NUMBER_OF_HALTS			EQU		3		; used by "delay" routine

SPACE					EQU		32		; space character code
UDG_A					EQU		144		; UDG 'A'
UDG_B					EQU		145		; UDG 'B'

; UDG graphics.

blocks DEFB 255,129,129,129,129,129,129,255	; UDG 'A' = empty box
       DEFB 255,255,255,255,255,255,255,255	; UDG 'B' = filled-in square

; game messages

strBasedOn			DEFB 'Based on Conway''s ''LIFE'''	; 24 chars
BASED_ON_LEN		EQU 24
strInit				DEFB 'Initialising...'				; 15 chars
INIT_LEN			EQU 15
strGeneration		DEFB 'Gen''s left='					; 11 chars
GENERATION_LEN		EQU 11
strCount			DEFB 'Count='						; 6 chars
COUNT_LEN			EQU 6
strXCoord			DEFB 'x='
XCOORD_LEN			EQU 2
strYCoord			DEFB 'y='
str3Spaces			DEFB '   '
STR3SPACES_LEN		EQU 3
strEndMessage		DEFB 'Press 1 to run again, 5 to exit' ; 31 chars
strBlankMessage		DEFB '                               ' ; 31 chars
ENDMESSAGE_LEN		EQU 31

YCOORD_LEN			EQU 2
xPosStr				DEFB 0								; used to print string
yPosStr				DEFB 0								; used to print string
count				DEFB 0								; used to count adjacent cells
arrayOffset			DEFB 0								; used to index into an array
outer				DEFB 0								; outer loop counter
inner				DEFB 0								; inner loop counter
printColour			DEFB GREEN							; used in F_printStr

; The two arrays used in the main loop

array1 DEFS 64,0	; for a 8x8 array filled with zeroes - use GRID_SIZE
array2 DEFS 64,0	; for a 8x8 array filled with zeroes

FALSE			EQU		0		; Boolean value
TRUE			EQU		1		; Boolean value
POP_SIZE		EQU		24		; the initial number of live cells
ITERATIONS		EQU		10		; the number of generations to run
GRID_SIZE		EQU		8		; the number of rows & columns
GRID_MASK		EQU		7		; to make sure RND() is less than GRID_SIZE

;} End of data