PRO CROLL,RATE,DIRECTION                    ;Roll Color Lookup table
;+
; NAME:
;    CROLL
; PURPOSE:
;    Roll the current color lookup table at a fixed rate.  
; CALLING SEQUENCE:
;    CROLL		;Roll the color table every 0.1 seconds
;    CROLL,RATE         ;Specify roll rate in units of 0.1 seconds
;    CROLL,RATE,DIRECTION	;Specify direction of roll
; OPTIONAL INPUTS:
;    RATE - Rate in units of 0.1 seconds at which the color table
;           is updated.
;    DIRECTION - If present and non-zero, then color table rolls to the left.
;                Otherwise color table rolls to the right
; OUTPUTS:
;    None.
; PROCEDURE:
;    The color table is read with SAVLUT.  Each color vector is shifted 
;    incrementally from 0 to 255.
; EXAMPLES:
;    CROLL           ;Roll to the right, update every 0.1 seconds
;    CROLL,0.5       ;Roll to the right, update every 0.05 seconds
;    CROLL,2,1       ;Roll to the left,  update every 0.2 seconds
; REVISION HISTORY:
;    W. B. Landsman, ST Systems          February, 1988
;    Modified for workstations.  M. Greason, STX, June 1990.
;-
common tv,chan,zoom,xroam,yroam
common colors,r,g,b,r_c,g_c,b_c
;
;			Set defaults.
;
if n_params(0) eq 0 then rate = 1
if n_params(0) lt 2 then direction = 0
if direction ne 0 then direction = -1 else direction = 1
;
;			Initialize.
;
print,'Press any key to exit CROLL'
shft = direction
savlut, r, g, b
if n_elements(r) le 0 then begin
   r = indgen(256) & g=r & b=r
endif
;
;			Loop until a key is struck, rolling the color table
;			continuously.
;
while get_kbrd(0) eq '' do begin 
   wait,.1*rate					; wait awhile
   red = shift(r,shft) & blue = shift(b,shft)
   green = shift(g,shft)
   tvlct,red,green,blue
   shft = ((shft + direction) mod 256)
endwhile
;
return
end
