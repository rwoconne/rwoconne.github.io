PRO CONTRAST,cont                    ;Interactive CONTRAST enhancement
;+
; NAME:
;	CONTRAST
; PURPOSE:
;	Use the mouse to change the slope and Y intercept of the color
;	lookup table on an X windows display. 
;
; CALLING SEQUENCE:
;	CONTRAST, [ cont ]
;
; OPTIONAL INPUT:
;	CONT - If this optional parameter is supplied and non-zero, then
;		CONTRAST is initialized by placing the cursor at the position 
;		of the previous call to CONTRAST.    Otherwise, CONTRAST 
;		initializes with the cursor in the center of the image.
;
; OUTPUT:
;	None
;
; SIDE EFFECTS:
;	Mouse button enabled:	LEFT - Reverses black and white
;				RIGHT - Exit
;
; PROCEDURE:
;	The y position (0-!D.Y_SIZE) is converted into a slope of range -10 
;	to 10 by using a tangent function.  The x position (0-!D.X_SIZE) is 
;	converted into a Y-intercept range of -128 to 128.
;
; COMMON BLOCKS:
;	The COLORS common block (see LOADCT) store the modified color table
;	The CONTRAST common block stores the final position of the cursor.
;
; REVISION HISTORY:  
;	Adapted from IVAS version on contrast, B. Pfarr, STX, 1/91
;	Added CONT parameter   W. Landsman       STX    August, 1991
;-
On_error,2                            ;Return to caller
common colors,r,g,b,r_new,g_new,b_new
common contrast,imp,isl

if (N_elements(r) NE !D.N_COLORS) then tvlct,r,g,b,/get

pi_4 = !pi/4 & ramp = findgen(256)        ;Useful constants

if (!D.FLAGS and 256) EQ 256 then wshow,!D.WINDOW     ;Bring up current window

y_mid = !D.Y_VSIZE/2 & xscale = 256./!D.X_VSIZE
if (N_params() EQ 1 ) and (N_elements(imp) EQ 1) then $
        tvcrs,imp,isl   else begin 
   tvlct,r,g,b
   tvcrs,!D.X_VSIZE/2,y_mid	;Initialize at slope of 1, midpoint of 128
endelse

print,'Move cursor UP   to increase contrast
print,'Move cursor LEFT to increase zero level'
print,'Hit LEFT Mouse button to reverse black and white'
print,'Press RIGHT Mouse button to exit CONTRAST'
cursor,/device,imp_old,isl_old,0                       ;Current cursor position

while !ERR ne 0 do cursor,/device,imp_old,isl_old,0    ;Empty the mouse buffer  
plus = 1b
Tb:	wait,0.2	                    ;let others use the CPU
        if ( !err EQ 4) then goto, cleanup
	cursor,/device,imp,isl,2	         ;read cursor position
        IF !err eq 1 THEN BEGIN             ;Reverse black and white?
                plus = 1b - plus
                dis = 1
        endif else dis = abs(imp_old - imp) + abs(isl_old - isl)
        if dis GE 1 then begin
            imp_old = imp & isl_old = isl
            slope = tan(isl*pi_4/y_mid)
            y_int = imp*xscale-128
            x = slope*(indgen(256) +y_int) >0 <255
            r_new = r(x)
            g_new = g(x)
            b_new = b(x)
 	;Load linear scale to ITT
	    if not plus then begin
               tvlct,reverse(r_new),reverse(g_new),reverse(b_new)
            endif else tvlct,r_new,g_new,b_new
        endif
goto,tb
cleanup:
return
end
