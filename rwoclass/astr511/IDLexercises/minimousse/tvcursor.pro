PRO TVCURSOR,X,Y,BLINK,IMAGE  = image
;+
; NAME:
;	TVCURSOR
; PURPOSE:
;	Toggle the cursor on or off, or move cursor to coordinates X,Y.
; CALLING SEQUENCE:
;	TVCURSOR		;Enable the cursor.
;	TVCURSOR, ON_OFF        ;Turn cursor on or off.
;	TVCURSOR, X, Y, [/IMAGE ]	;Move cursor to coordinates X,Y
; OPTIONAL INPUTS:
;	ON_OFF - If 0, disable the cursor; otherwise, enable it, scalar
;	X      -  X coordinate where cursor is moved to, scalar
;	Y      -  Y coordinate where cursor is moved to, scalar
; OUTPUTS:
;	None.
; OPTIONAL INPUT KEYWORD:
;	IMAGE - If this keyword is set and non-zero, then the X,Y coordinates
;		are assumed to be in image coordinates, which may be zoomed or
;		or offset with respect to the window coordinates.
; SIDE EFFECTS:
;	Cursor is enabled.
; PROCEDURE:
;	TVCRS is used to activate the cursor at the specified coordinates
;	(if any).
; SYSTEM VARIABLES:
;	If !QUIET = 1 then no informational messages will displayed
; REVISION HISTORY:
;	Written W. Landsman, August 1986
;	Modified for workstations.  M. Greason, STX, June 1990.
;	Print the cursor position  W. Landsman       Feb 1994
;	Accept one element vector W. Landsman        May 1995
;	Change to X windows if not already there
;-
 On_error,2
 npar = N_params()

 if (!D.FLAGS AND 256) NE 256 then begin 
	set_plot,'X'
	message,/INF, 'Output device set to X windows'
 endif

 wshow,!D.window
 IF npar GE 2 THEN BEGIN 

    if (N_elements(x) NE 1) or (N_elements(y) NE 1) then message, $
	'ERROR - Cursor X,Y position must be specified as scalars'
    if keyword_set(image) then begin
	zoom_xy,x,y,xtv,ytv
	tvcrs,xtv,ytv,/device
    endif else tvcrs, x(0), y(0), /device
    message,/INF, "CURSOR enabled at (" + strtrim(x,2) + $
        "," + strtrim(y,2) + ")"

 ENDIF ELSE BEGIN
    IF npar LT 1 THEN x = 1
    tvcrs, x
    if x EQ 1 then begin
          cursor, xx, yy, /DEV, /NOWAIT
          message,/INF, "CURSOR enabled at (" + strtrim(xx,2) + $
                                     "," + strtrim(yy,2) + ")"
    endif 
 ENDELSE

 RETURN
 END
