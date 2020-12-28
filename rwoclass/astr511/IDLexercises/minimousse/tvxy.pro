pro tvxy,hdr,x,y,a,d,PRINT=print    ;Get Ra and Dec of current cursor position
;+
; NAME:
;	   TVXY
; PURPOSE:
;	Get the RA and Declination of the current cursor position
;	Place the cursor at the desired position before calling this routine
;
; CALLING SEQUENCE:
;	TVXY, Hdr, [ x, y, ra, dec, /PRINT ]	
;
; INPUTS:
;    Hdr   - Image header appropiate to the current image plane
;
; OPTIONAL OUTPUTS:
;	X - Cursor position, image coordinates, scalar
;	Y - Cursor position, image coordiantes, scalar
;	RA - Right ascension in decimal degrees, scalar
;	DEC - Declination in decimal degrees, scalar
;
; OPTIONAL INPUT KEYWORD:
;	PRINT - If this keyword is set then results are always displayed at the
;		terminal.
; NOTES:
;	If less than 3 parameters are supplied, or if the keyword PRINT is set, 
;	then RA and Dec is printed in both decimal and sexigesimal format.
;
;	If the cursor is already on the active window, then the results are 
;	printed without waiting.  Otherwise, TVXY prompts for the user to press
;	a mousse button.
; REVISION HISTORY:
;	W. Landsman                 STX          January,1988
;	Updated to use ASTROMETRY structures.  J D Offenberg, Hughes STX, Jan 1993
;-
 On_error,2
 npar = N_params()
 if npar eq 0 then begin
	print,'Syntax - TVXY, hdr, [ x, y, ra, dec, /PRINT]'
	return
 endif

 if (!D.FLAGS AND 256) EQ 256 then wshow,!D.WINDOW

 extast,hdr,astr,noparams

 if noparams lt 0 then $	;Does astrometry exist in header?
	message,'Header does not contain astrometry info'


 tvcursor,1                                      ;Make sure cusror enabled
 cursor,xtv,ytv,/DEVICE,/NOWAIT			;Get cursor position
 if (xtv lt 0) or (ytv lt 0) then begin
      print,'Put the cursor on Window '+strtrim(!D.WINDOW,2)+ $
           '; press a mouse button'
      cursor,xtv,ytv,/WAIT,/DEV
 endif

 unzoom_xy,xtv,ytv,x,y                   ;Get unroamed, unzoomed coordinates
 xyad,hdr,x,y,a,d
 if (npar lt 3) or keyword_set(PRINT) then begin
   fmt = '(2I5,2F8.2,3x,2F8.4,A)'
   print,'     TV          IMAGE'
   print,'   X    Y      X       Y        RA      DEC       RA         DEC'
   print,form=fmt,xtv,ytv,x,y,a,d,adstring([a,d])
 endif

 return
 end
