pro wzoom, inc, interp = interp
;+
; NAME:
;	WZOOM
; PURPOSE:
;	Expands entire image (or graphics) from active window by integral 
;	multiples to specified amount, up to the largest possible square.
;
; CALLING SEQUENCE:
;	Wzoom, Inc, [ INTERP = ] 
;
; INPUT:
;	Inc = increment, integer scalar.   Set Inc=1 to double window size,
;		Inc = 2 do triple window size, etc.
;
; KEYWORD INPUT:
;	INTERP = If this keyword is set and  non-zero then bilinear
;		interpolation is used for the zoomed image, otherwise pixel 
;		replication is used.
;
; COMMON BLOCKS:
;	Reads the current channel from the TV common block.
;	Updates the zoom factor in the TV block.
;
; SIDE EFFECTS:
;	Original image is lost as it is blown up. (See "Hint to Users")  The 
;	originally selected window is resized to hold new image. 
;
; EXAMPLE:
;	Zoom up the current (60 x 60) window by duplicating each pixel
;          
;       IDL> wzoom,1 
; RESTRICTIONS:
;	Image can only be expanded by an integral factor.
;	The final image will not be larger than the screen available.
;	Requires X windows display
;
; BIG HINT TO USERS:
; 	This function will not "unzoom" your image.  If you want to be able to
;	undo or redo this procedure, I recommend copying your existing image
;	to a separate window. 
;
; MODIFICATION HISTORY:
;	Written by J. D. Offenberg, STX		June, 1991
;	Some key pieces of code "stolen" from: CZOOM, by K. Bhagat, STX 
;       Remove restriction of a square image   W. Landsman   September 1991
;-
;

COMMON TV,	chan,zoom,xroam,yroam	;Only the ZOOM field of this block 
common images,  x00,y00,xsize,ysize     ;change with this function.

On_error,2		;Return to caller if error occurs

if (!D.FLAGS AND 256) NE 256 then $
               message,'Current device '+ !D.NAME + ' does not support windows'

if n_params() LT 1 THEN BEGIN
	print,'Syntax - WZOOM, INC,[/INTERP]'
	return
endIF

device, get_screen_size=MSize	;MSize is a 2D vector, containing the screen 
MaxSize = min(MSize)		;dimensions in pixels.  The largest square that
				;will fit on the screen can be no larger than
				;the smaller of these two numbers.
;
;	SET PARAMETERS FOR INCREMENT
;
inc = inc + 1
if N_elements(xsize) GT 0 then begin
     sizex = xsize(chan)   & sizey = ysize(chan)
endif else begin
     sizex = !D.X_size   &   sizey = !D.Y_size
endelse		
CurrSize = (sizex > sizey) 	; The greater of the two (They 
					; should ususally be equal)
LLL = MaxSize/CurrSize
if (LLL LT 2) then begin
		message,"Image too big...impossible to WZOOM.",/inform
		goto, done
	end
if inc gt LLL then begin	
		message, "Zoom increment too large.  Reset for maximum",/inform
		print,"Maximum zoom increment for this image is: ",fix(LLL)-1
		inc = fix(LLL)
	end

if (n_elements(inc) le 0) then inc = 2

if (inc lt 2) then begin
		message,"Zoom increment too small.  Set at 2",/inform
		inc = 2
	end
;
;	READ ORIGINAL WINDOW
;
print,"Reading original window.  Please wait..." 
AAA = tvrd (0,0, sizex, sizey )
;	
;	RESIZE ORIGINAL WINDOW 
;
NewSizeX = inc * sizex
NewSizeY = inc * sizey
window,chan,xsize=NewSizeX,ysize=NewSizeY
;
;	ZOOM IMAGE AND PLOT
;
tv, rebin (AAA,NewSizeX,NewSizeY,sample=1-keyword_set(interp) )
;
;	UPDATE COMMON BLOCK.  
;
	zoom(chan) = zoom(chan) + inc - 1
;
;	"BEEP"
;
done:   print,string(7B)
end 
