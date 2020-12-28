pro warplist, ch, chref, x, y, xref, yref, BOXRAD = boxrad
;+
; NAME:
;    WARPLIST
; PURPOSE:
;    Allows user to cursor-select corresponding sources as shown on two
;    different display channels.  Lists of corresponding X, Y coordinates
;    returned.  Source located at maximum pixel within box surrounding
;    cursor position.   Use CWARPLIST to use centroid to define position
;    rather than maximum pixel value.
; CALLING SEQUENCE:
;    warplist, ch, chref, x, y, xref, yref, [ BOXRAD = ]
; INPUT PARAMETERS:
;    ch     = channel containing one of the images ('object image')
;    chref  = channel containing other image ('reference image')
; OUTPUT PARAMETERS:
;    x, y   = coordinates of sources on object image
;    xref, yref = corresponding coordinates on reference image
; OPTIONAL INPUT PARAMETER:
;    boxrad [keyword] = 'radius' of search box;  e.g., 5 (default) gives
;                        box 11 pixels on a side
; SIDE EFFECTS:
;    Displays boxes around selected sources.
;    !QUIET is set by WARPLIST to keep the display clean, it is reset before
;           turning
; PROCEDURES USED:
;    CTVRD, CHAN
; MODIFICATION HISTORY:
;    Written by R. S. Hill, ST Systems Corp., 10 April 1991
;    Symbols plotted at final computed positions, not user-selected ones.
;       RSH, STX, 2 May 1991
;-
On_error,2
if N_params() LE 0 then begin
   print,'Syntax - warplist, ch, chref, x, y, xref, yref, [ BOXRAD= ]'
   return
endif
qsave = !quiet & !quiet = 1
if not keyword_set( BOXRAD ) then boxrad=5
key = '0'
x = 0 & y = 0 & xref = 0 & yref = 0
side = 2*boxrad+1
WHILE (key NE 'q') AND (key NE 'Q') DO BEGIN
   chan,ch
   print,'Select source on channel ',ch
   cursor, xx0, yy0, /down, /device
   ctvrd, source, xx0-boxrad, yy0-boxrad, side, side
   m = max(source, maxsub)
   yybox = fix(maxsub/side)
   yy = yybox + yy0
   xx = maxsub - side*yybox + xx0
   plots,xx,yy,/device,psym=1
   chan,chref
   print,'Select source on channel ',chref
   cursor, xx0r, yy0r, /down, /device   
   ctvrd, source, xx0r-boxrad, yy0r-boxrad, side, side
   m = max(source, maxsub)
   yyboxr = fix(maxsub/side)
   yyref = yyboxr + yy0r
   xxref = maxsub - side*yyboxr + xx0r
   plots,xxref,yyref,/device,psym=1
   x = [x,xx] & y = [y,yy] & xref = [xref,xxref] & yref = [yref,yyref]
   print,'Current match:  (ref) = ',xxref,yyref,'   (object) = ',xx,yy
   print,'Enter Q to quit or anything else to continue'
   key = get_kbrd(1)
ENDWHILE
x = x(1:*) & y = y(1:*) & xref = xref(1:*) & yref = yref(1:*)
!quiet = qsave
return
end
