PRO REFSCL,length,over=over,x=x,y=y,horiz=horiz
;+
; NAME:
;	REFSCL
; PURPOSE:
;	To display a reference scale, indicating the behavior of the color
;	table.     The reference scale can either be placed in a separate
;	window (default) or overlaid on the current  image
;
; CALLING SEQUENCE:
;	refscl, [length, /OVER, X = , Y = , HORIZ = ]
;
; OPTIONAL INPUTS:
;	length - scalar specifying the length of the reference scale.
;		The default is to set the length equal to the number of
;		colors (given by !D.N_COLORS).
;
; OPTIONAL KEYWORD INPUTS:
;	OVER - if set, refscl is overdrawn in same chan as image
;	X    - if set, refscl is drawn starting at this x coord on image
;	Y    - if set, refscl is drawn starting at this y coord on image
;	HORIZ- if set, refscl is drawn horizontally in right margin of image
;		This keyword has meaning only if /OVER is also set
;
; OUTPUTS:
;	None.
;
; SIDE EFFECTS:
;	An L x 20 window is created, where L is the length parameter   
;
; RESTRICTIONS:
;	An IDL window must be available to be opened.  This procedure may
;	overwrite a window already in existence if a free one is not available.
;
; PROCEDURE:
;	The reference scale array (L,20), with each element containing its
;	column number, is created.  A window is opened and the array is
;	displayed, using TV.  CHAN is then used to make the previously active
;	window the active window.
;
; MODIFICATION HISTORY:
;	Written by Michael R. Greason, STX, 7 June 1990.
;       Added over,x,y, horiz, keywords, B. Pfarr, 4/91
;       Added length parameter W. Landsman     3/92
;-
On_error,2

COMMON TV, chan, zoom, xroam, yroam
common images,x0,y0,xsize,ysize
;
;			Create the reference scale array.
ref1 = bindgen(!D.N_COLORS)
ref = bytarr(!D.N_COLORS,20,/NOZERO)
for i=0,19 do ref(0,i) = ref1
if N_elements(length) EQ 0 then length = !D.N_COLORS
if length NE !D.N_COLORS then $
    ref = congrid(ref,length,20)
;
;			Open the window and display.
;
opnd = window_state()
if (not keyword_set(over)) then begin
  w = 7
  while opnd(w) do w = w+1
  titl = "IDL " + strtrim(string(w),2) + " -- Ref. Scale"
  window,w, xsize=length, ysize=20, retain=2, title=titl
  tv,ref
endif else begin
  if !D.WINDOW EQ -1 then message,'No images are displayed'
  device,get_screen_size = w_sz
  if (not keyword_set(horiz)) then begin
     if (N_elements(x) NE 1) then x= ((xsize(chan) - length)/2)>1
     if (N_elements(y) NE 1) then y =20
     tv,ref,x,y,chan
     tvbox,[length+2,20],x+length/2,y+10
  endif else begin
     ref = transpose(ref)
     if (not keyword_set(x)) then x= xsize(chan) - 20
     if (not keyword_set(y)) then y =((ysize(chan) -length)/2) > 0 
     tv,ref,x,y,chan
     tvbox,[20,length],x+10,y+length/2
  endelse     
endelse
;
;			Restore the originally active window.
;
if opnd(chan) then chan, chan
;
RETURN
END
