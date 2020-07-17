; Delete the player.

	CALL basexy         ; set the x AND y positions of the player.
	CALL wspace         ; display space over player.

; Now we've deleted the player we can move him before redisplaying him
; at his new coordinates.

	LD BC,63486			; keyboard row 1-5/joystick port 2.
	IN A,( C )			; see what keys are pressed.
	RRA					; outermost bit = key 1.
	PUSH AF				; remember the value.
	CALL NC,mpl			; it's being pressed, move left.
	POP AF				; restore accumulator.
	RRA					; next bit along (value 2) = key 2.
	PUSH AF				; remember the value.
	CALL NC,mpr			; being pressed, so move right.
	POP AF				; restore accumulator.
	RRA					; next bit (value 4) = key 3.
	PUSH AF				; remember the value.
	CALL NC,mpd			; being pressed, so move down.
	POP AF				; restore accumulator.
	RRA					; next bit (value 8) reads key 4.
	PUSH AF				; remember the value
	CALL NC,mpu			; it's being pressed, move up.
	POP AF				; restore A
	RRA					; last bit (value 16) reads key 5.
	CALL NC,fire		; it's being pressed, fire

; Now he's moved we can redisplay the player.

	CALL basexy         ; set the x AND y positions of the player.
	CALL splayr         ; show player.
   
; See if the player's bullet has hit anything

	CALL bchk			; check bullet position
	CALL dbull			; delete bullets
	CALL moveb			; move bullets
	CALL bchk			; check new position of bullets
	CALL pbull			; print bullet at new position