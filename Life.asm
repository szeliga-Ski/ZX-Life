ORG 32768

INCLUDE "init.asm"
 
init:
CALL F_blackScreen
CALL F_initPop
CALL F_run				; call the main loop

RET

INCLUDE "data.asm"		; include data definitions
	
END 32768				; makes TAP file auto-run


