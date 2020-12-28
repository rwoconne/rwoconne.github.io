pro copychan, in, out, PIXMAP=pixmap
;+
; NAME:
;	COPYCHAN
; PURPOSE:
;	To copy an image from one window to another.  X-windows version
;
; CALLING SEQUENCE:
;	copychan, in, out, [ /PIXMAP]
;
; OPTIONAL INPUTS:
;	in   - window to be copied (0-127), this window must already exist
;	out  - window to copy to (0-127).   If the output window does not
;		already exist, it will be created
;	COPYCHAN will prompt for the input and output windows if not supplied
;
; OUTPUTS:
;	none
;
; SIDE EFFECTS:
;	Image on input window is copied to output window.  The active window
;	is not changed.
;
; EXAMPLE:
;	An image in plane 0 will have graphics written over it by VIDEO.   
;	Save the pristine image in a PIXMAP so that it can be quickly 
;	redisplayed.
;
;	IDL> COPYCHAN,0,5,/PIX              ;Save image in pixmap on window 5
;	.........VIDEO graphics overlay commands
;	IDL> COPYCHAN,5,0                   ;Restore the pristine image 
;
; REVISION HISTORY:
;	written by W. Landsman    February, 1991
;-
 On_error,2

 common tv,chan,zoom,xroam,yroam
 common images, x00,y00,xsize,ysize
;
 npar = N_params()

 if (!D.FLAGS AND 256) NE 256 then begin     ;Is current device X windows?
	set_plot,'X'
	message,/INF, 'Output device set to X windows'
 endif

 device,window_state = opnd
 open = where(opnd,nopen)
 if nopen EQ 0 then message,'No windows are available to be copied'

 if npar lt 1 then read,'Enter source window: ',in

; OPND is a 128-element array which is 1 wherever the window is open

 test = where(open EQ in, Nfound)
   if Nfound EQ 0 then message,'Input window number '+strtrim(in,2) + $
                               ' is not currently open'
 sv_window = !D.WINDOW
 wset,in

 if (xsize(in) EQ 0) then begin
     xsize(in) = !D.X_SIZE  & ysize(in) = !D.Y_SIZE
 endif

 xsize_in = !D.X_SIZE   & ysize_in= !D.Y_SIZE       ;Input image sizes

 if npar LT 2 then read,'Enter destination window: ',out

 if not keyword_set(PIXMAP) then begin
        pixmap = 0 & retain = 1
 endif else retain = 0

 newwindow = 1b

 if (pixmap EQ 0) and opnd(out) then begin
    wset,out
    if (!D.X_SIZE EQ xsize_in) and (!D.Y_SIZE EQ ysize_in) then newwindow= 0b 
 endif

 if newwindow then $
   window,out,xsize = xsize_in,ysize = ysize_in,PIXMAP=pixmap,RETAIN=retain

 wset,out
 device,copy = [0,0,xsize_in,ysize_in,0,0,in]

 wset,sv_window

 if N_elements(x00) NE 0 then begin       ;update images common
   x00(out) = x00(in)
   y00(out) = y00(in)
   xsize(out) = xsize(in)
   ysize(out) = ysize(in)
 endif

 return
 end  
