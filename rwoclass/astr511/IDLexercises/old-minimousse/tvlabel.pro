pro tvlabel,x,y,text,size,angle,color
;+ 
; NAME:
;	TVLABEL
; PURPOSE:
;	Write text to workstation window.  The procedure is identical to XYOUTS 
;	except for the following
;	(1)  User will be prompted, if necessary, for the position, size and
;		content of the text to be written.  Position may be specified by
;		the current cursor position.
;	(2)  TVLABEL assumes X and Y positions are given in unzoomed,
;		unroamed device coordinates.
;
; CALLING SEQUENCE:
;    TVLABEL                 ;Prompt for text position, content, and size
;    TVLABEL,X,Y             ;Prompt for text content and size
;    TVLABEL,X,Y,TEXT                          ;Prompt for text size
;    TVLABEL,X,Y,TEXT,SIZE                     ;No prompts
;    TVLABEL,X,Y,TEXT,SIZE,ANGLE,COLOR         ;Full Specification
;
; OPTIONAL INPUTS:
;	X,Y  - scalars giving the X and Y position of the lower left hand
;		corner of the text.  If omitted, user will be prompted to
;		either supply X and Y or use the current cursor position
;		User will also be prompted if X or Y are outside valid
;		screen area.
;	TEXT - Character string giving text to be written to the image display
;		Full Calligraphic character set is allowed
;	SIZE - Height of text in (not necessarily integral) multiples of the 
;		smallest size.  If SIZE = 1 then the text will be 6 pixels high.
;	ANGLE- Angle of text in degrees counterclockwise from horizontal
;		Default is 0
;	COLOR- Determines the image plane and color of the text (0 - 255).
;
; OUTPUTS:
;	None
; RESTRICTIONS:
;	TVLABEL assumes that the roam and zoom of the graphics overlay
;	is identical with that of the current image plane.
; REVISION HISTORY
;    Written    W. Landsman               June, 1988
;    Modified for workstations.  M. Greason, STX, June 1990.
;-
;
 npar = N_params()
 if npar lt 6 then color = !P.COLOR
 if npar lt 5 then angle = 0

 if npar lt 3 then begin 
   text = ''
   print,'TVLABEL - Write text to the image display
   read,'Enter text: ',text
 endif

 if npar lt 2 then getpos = 1 else $
   if ((x lt 0) or (x gt !d.x_vsize-1) or (y lt 0) or (y gt !d.y_vsize-1)) $
   then getpos = 1 else getpos = 0

 if getpos then begin
   ans = ''
   GETXY:   print,'Enter X,Y coordinates of left-hand corner of text'
   read,'or hit [RETURN] to place at cursor position',ans
   if ans eq '' then cursor,x,y,0,/device else begin
   pos = getopt(ans,'F')
   if ( N_elements(pos) EQ 2 ) then begin
      x = pos(0) & y = pos(1)
   endif else begin
      print,string(7B),'INPUT ERROR - Expecting 2 numbers or [RETURN]'
      goto, GETXY  
   endelse
   endelse
endif

if npar LT 4 then read, $
     'Enter (not necessarily integral) size of text (>1): ',size

 xyouts,x,y,text,size=size,orientation=angle,color=color,/device

 return
 end
