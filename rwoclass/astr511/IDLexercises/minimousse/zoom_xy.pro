pro zoom_xy,xim,yim,xtv,ytv,OFFSET=offset
;+
; NAME:
;      ZOOM_XY
; PURPOSE:
;      Converts X, Y position on the image array to the the X,Y position 
;      on the current image display.  (These  positions are identical 
;      only for an unroamed, zoomed image with with pixel (0,0) of the 
;     image placed at position (0,0) on the TV.)
;
; CALLING SEQUENCE:
;      ZOOM_XY,XIM,YIM,XTV,YTV,[ OFFSET = ]
;
; INPUTS:
;      XIM - Scalar or vector giving X position(s) as read on the image
;            display (e.g. with CURSOR,XIM,YIM,/DEVICE)
;      YIM - Like XTV but giving Y position(s) as read on the image display.
;
;      If only 2 parameters are supplied then XIM and YIM will be modfied
;      on output to contain the converted coordinates.
;
; OPTIONAL KEYWORD INPUT:
;      OFFSET - 2 element vector giving the offset between (0,0) on the
;               TV and (0,0) on the image.  If not supplied, then the 
;               offset is obtained from the common block IMAGES.  If the 
;               common block is not defined, then OFFSET = [0,0]
;
; OUTPUTS:
;      XTV,YTV - REAL*4 X and Y coordinates of the image corresponding to the
;            cursor position on the TV display.   Same number of elements as
;            XIM, YIM.
;
; COMMON BLOCKS:
;       The offset of the image array on the TV display is read from
;       the common block IMAGES.  If this common block is not defined,
;       then position (0,0) on the image is assumed to be at (0,0)
;       on the TV display
; REVISON HISTORY:
;       written by B. Pfarr, STX, 5-29-87                 
;       offset parameter added, W. Landsman, STX 6-30-88
;	converted to use with a workstation.  M. Greason, STX, 31 May 1990
;	documentation corrected.  M. Greason, Hughes-STX, 31 March 1992
;	Removed DeAnza calls   W. Landsman  HSTX   December 1994
;	Removed IVAS call   W. Landsman   HSTX   April 1995
;-
 On_error,2 
 common tv,chan,zoom,xroam,yroam
 common images,x00,y00,xsize,ysize
;
 if N_params() LT 2 then begin
        print,'Syntax - ZOOM_XY,XTV, YTV, XIM, YIM, [ OFFSET= 
        return
 endif
    
 if N_elements(offset) NE 2 then begin
    if n_elements(x00) eq 0 then offset = [0,0] $  ;Images common block defined
                            else offset = [x00(chan),y00(chan)]
 endif

 z = zoom(chan)
 cen =  (z-1)/2.

 xtv =  cen + z*(xim - xroam(chan) + offset(0) )
 ytv =  cen + z*(yim - yroam(chan) + offset(1) )

 if N_Params() LT 3 then begin
    xim = xtv  & yim = ytv
 endif                  

 return
 end                                    
