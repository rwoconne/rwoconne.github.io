pro unzoom_xy,xtv,ytv,xim,yim,OFFSET=offset
;+
; NAME:
;      UNZOOM_XY
; PURPOSE:
;      Converts X, Y position on the image display to the the X,Y position 
;      on the corresponding image array.  (These  positions are identical 
;      only for an unroamed, unzoomed image with with pixel (0,0) of the 
;     image placed at position (0,0) on the TV.)
;
; CALLING SEQUENCE:
;      UNZOOM_XY,XTV,YTV,XIM,YIM,[ OFFSET=    
;
; INPUTS:
;      XTV - Scalar or vector giving X position(s) as read on the image
;            display (e.g. with CURSOR,XTV,YTV,/DEVICE)
;      XTV - Scalar or vector giving Y position(s) on the image display.
;      If only 2 parameters are supplied then XTV and YTV will be modfied
;      on output to contain the image array coordinates.
;
; OPTIONAL KEYWORD INPUT:
;      OFFSET - 2 element vector giving the offset between (0,0) on the
;               TV and (0,0) on the image.  If not supplied, then the 
;               offset is obtained from the common block IMAGES.  If the 
;               common block is not defined, then OFFSET = [0,0]
; OUTPUTS:
;      XIM,YIM - X and Y coordinates of the image corresponding to the
;            cursor position on the TV display.
; COMMON BLOCKS:
;       The offset of the image array on the TV display is read from
;       the common block IMAGES.  If this common block is not defined,
;       then position (0,0) on the image is assumed to be at (0,0)
;       on the TV display
; REVISON HISTORY:
;       written by B. Pfarr, STX, 5-29-87                 
;       offset parameter added, W. Landsman, STX 6-30-88
;	converted to use with a workstation.  M. Greason, STX, 31 May 1990
;	Removed DeAnza call    W. Landsman       November 1994
;	Removed IVAS call   W. Landsman       April 1995
;-
common tv,chan,zoom,xroam,yroam
common images,x00,y00,xsize,ysize

 On_error,2

if N_params() LT 2 then begin
        print,'Syntax - UNZOOM_XY, xtv, ytv, xim, yim, [OFFSET= 
        return
endif
    
if N_elements(offset) NE 2 then begin
    if n_elements(x00) eq 0 then offset = [0,0] $  ;Images common block defined
                            else offset = [x00(chan),y00(chan)]
endif

 z = zoom(chan)
 cen =  (z-1)/2.
 xim = xroam(chan) + float((xtv-cen)/z) - offset(0)
 yim = yroam(chan) + float((ytv-cen)/z) - offset(1)

 if N_Params() LT 3 then begin
   xtv = xim & ytv = yim
 endif

return
end                                    
