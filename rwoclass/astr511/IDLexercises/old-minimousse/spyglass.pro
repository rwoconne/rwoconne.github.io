pro spyglass, litim, lithdr, bigim, bighdr
;+
; NAME:
;	SPYGLASS
; PURPOSE: 
;	To allow user to select a position on a compressed image and display a 
;	portion of a full resolution image, centered on this position.
;	If both headers contain astrometry, then the images do not have
;	to be aligned.  
;
; CALLING SEQUENCE:
;	spyglass, litim, lithdr, bigim, bighdr 
;
; INPUTS: 
;	litim   - little (compressed array), usually 512 x 512
;	lithdr  - FITS header of compressed array
;	bigim   - big (full resolution) image
;	bighdr  - FITS header of full resolution image
; OUTPUTS:
;	None
; USAGE:
;	The compressed image should be DISPLAYED IN THE ACTIVE WINDOW
;	before calling SPYGLASS.  Once the portion of the big image
;	has been displayed, a menu will appear which gives the user
;	such options as loading and adjusting a color table, displaying 
;	coordinates, intensities, and pixel values, and zooming an image.   
;	Other options include returning to the compressed image to  
;	choose another region of the big image to display.
;
; SIDE EFFECTS:
;	A box is drawn on the compressed image which shows the region 
;	of the full resolution image to be displayed. 
; REVISION HISTORY: 
;	Version 1  by B. Boothman 11-19-86
;	Version 2  by W. Landsman  7-28-88
;	Converted for use on workstations, and some menu options added, 
;		K. Rhode, STX, July 1990
;	Updated to use ASTROMETRY structures.  J.D.Offenberg, HSTX, Jan 1993
;	Recognize GSSS header   W. Landsman      June 1994
;	Modified to work with the Astro2 distortion model.  M.R. Greason,
;		HSTX, August 1995.
;-                                    
 common tv,chan,zoom,xroam,yroam
 On_error,2
 if N_params() LT 4 then begin
	  print,'Syntax -  SPYGLASS, Litim, Lithdr, Bigim, Bighdr.'
 	return
 endif                                                                 

 check_fits, litim, lithdr, dimen
 xsizl = dimen(0) & ysizl = dimen(1)

 check_fits, bigim, bighdr, dimen, idltype
 xsizb = dimen(0) & ysizb = dimen(1)

 xratio = float(xsizb)/xsizl     ;Get the ratio of big, little image sizes
 yratio = float(ysizb)/ysizl

 if (min([xratio,yratio]) lt 1.) then begin
  message, $      
   'ERROR - Small image (LITIM) is larger than big image (BIGIM)',/INF
  message, $
   'Be sure to have your calling sequence parameters in the right order',/INF
  return
endif
;
extast,lithdr,lastr,lparams             ;Extract astrometry if it exists
extast,bighdr,bastr,bparams
if (lparams lt 0) or (bparams lt 0) then begin
  print,"WARNING - At least one of the headers doesn't contain astrometry,"
  print,'                so THE IMAGES MUST BE COALIGNED!'
  lparams = -1
endif
;
; For saving & deleting windows:
; SV_BIGW contains the indexes of the big image windows opened by the user.
; SV_WINDOW_FLAG and DEL_WINDOW_FLAG directly correspond to SV_BIGW ---
; SV_WINDOW_FLAG is 1 wherever the user has saved a big image window, and 
; DEL_WINDOW_FLAG is 1 wherever the user has deleted a big image window.     
;
sv_bigw = (sv_window_flag = (del_window_flag = (intarr(40)) ) ) 
n = (bigw = 0)                          ;Initialize variables                   
tvcrs,1                                 ;Make sure cursor is enabled
;
; Compressed image should be displayed before SPYGLASS is called,
; so that the window with the compressed image is the current window
litw = !d.window
START: 
chan,litw
;
print,'Place cursor on the compressed image, on the region you wish to see,'  
print,'then press the LEFT mouse button. (RIGHT mouse button to EXIT SPYGLASS.)'
tvrdc,x,y,3,/device
CASE !err OF 
  4: goto,DONE
ELSE: begin
  tvbox,[xsizl/xratio,ysizl/yratio],x,y   ;Draw box around selected area
  if (lparams ge 0) then begin            

   case idastrom(lithdr) of 
	'UIT':	uit_xy2ad, x, y, lastr, a, d
	'GSS': uit_xy2ad,lastr, x, y, a, d
	else:  xy2ad,x,y,lastr,a,d             
   endcase 

   case idastrom(bighdr) of             ;Get x,y for new image 
	'UIT': uit_ad2xy, a, d, bastr, xnew, ynew 
	'GSS': gsssadxy, bastr, a, d, xnew, ynew  
	else:  ad2xy,bastr, a, d, xnew,ynew 
    endcase

  endif else begin                                      
    xnew = x*xratio
    ynew = y*yratio
  endelse
  xnew = (xnew-!d.x_vsize/2)>1
  ynew = (ynew-!d.y_vsize/2)>1

;Create a big image window, or use an existing one that hasn't been saved
  if (n eq 0) and (sv_window_flag(n) eq 0) then begin
    if (bigw ne 0) then wset,bigw else window,/free,xsize=512,retain=2 
  endif else begin
    if (sv_window_flag(n) eq 1) then begin
      window,/free,xsize=512,retain=2 
      n = n + 1
    endif else if (del_window_flag(n) eq 1) then $
                window,/free,xsize=512,retain=2 else wset,bigw
  endelse
  bigw = !d.window
  chan,bigw
  sv_bigw(n) = bigw     ;Save the window index for later use
;
  print,'Portion of Big Image will be displayed centered at:'
  print,'x=',xnew + !d.x_vsize/2,',      y=',ynew + !d.y_vsize/2
  if (idltype eq 1) then $
   CTV,bigim,-xnew,-ynew,bigw else CTVSCL,bigim,-xnew,-ynew,bigw 
 end
ENDCASE
;
;After displaying the big image window, display a menu of options 
OPT_MENU:
ostr=['SPYGLASS OPTIONS MENU','Select another region on the compressed image',$
   'Display the same region, but change the intensity scaling',$
   'Adjust the intensity table (ITT)',$
   'Load a color table (LOADCT)','Adjust the color table (CONTRAST)',$
   'Display coordinates & intensities (CURVAL)',$
   'Print pixel values surrounding the cursor position (TVLIST)',$
   'Zoom an image (ZOOM)','Delete or save a big image window on the screen',$
   'Exit SPYGLASS']
ispy = wmenu(ostr,init=1,title=0)
chan,litw       ;LITIM window should be the active window for now
tvcrs,x,y,/dev	;Restore the cursor
CASE ispy OF
    1: goto,START 
    2: goto,SCALE
    3: ITT
    4: LOADCT
    5: CONTRAST
    6: goto,CURVAL_MENU                                                 
    7: goto,TVLIST_MENU    
    8: goto,ZOOM_MENU                                                  
    9: goto,WINDOW_MAINT
    10: goto,DONE
ENDCASE
goto,OPT_MENU
;
CURVAL_MENU:
cstr=['Choose which image to run CURVAL with:','Compressed image (LITIM)',$
     'Full resolution image (BIGIM)','Return to main SPYGLASS menu']
icurv = wmenu(cstr,init=1,title=0)
CASE icurv OF
    1: begin 
       chan,litw & tvcrs,x,y,/dev      ;Restore cursor to little window
       CURVAL,lithdr,litim
       end
    2: begin
       chan,bigw & tvcrs,x,y,/dev      ;Restore cursor to big window
       CURVAL,bighdr,bigim             ;Call CURVAL with offset keywords
       end
    3: goto,OPT_MENU
ENDCASE
goto,CURVAL_MENU                       
;      
ZOOM_MENU:
zstr=['Choose which image to run ZOOM with:','Compressed image (LITIM)',$
     'Full resolution image (BIGIM)','Return to main SPYGLASS menu']
iz = wmenu(zstr,init=1,title=0)
CASE iz OF
    1: begin
       chan,litw & tvcrs,x,y,/dev   ;Restore cursor to little window
       ZOOM
       end
    2: begin
       chan,bigw & tvcrs,x,y,/dev   ;Restore cursor to big window
       ZOOM
       end
    3: goto,OPT_MENU
ENDCASE
goto,ZOOM_MENU
;    
TVLIST_MENU:
tvstr=['Choose which image to run TVLIST with:','Compressed image (LITIM)',$
     'Full resolution image (BIGIM)','Return to main SPYGLASS menu']
itv = wmenu(tvstr,init=1,title=0)
CASE itv OF
    1: begin
       chan,litw & tvcrs,x,y,/dev   ;Restore cursor to little window
       TVLIST,litim
       end
    2: begin
       chan,bigw & tvcrs,x,y,/dev   ;Restore cursor to big window
       TVLIST,bigim
       end
    3: goto,OPT_MENU
ENDCASE
goto,TVLIST_MENU
;
WINDOW_MAINT:
svstr=['SPYGLASS WINDOW MAINTENANCE',$
       'SAVE the CURRENT big image window on the screen',$
       'DELETE a PREVIOUSLY SAVED big image window',$
       'Return to main SPYGLASS menu']
isv = wmenu(svstr,init=1,title=0)
ans=''
CASE isv OF
    2: begin
       read,'Type the index of the window you wish to DELETE:',ans
       ans = strtrim(ans,2)
       if (ans eq bigw) then begin
         print,'Sorry, but you may not delete the current big image window.'
         goto,WINDOW_MAINT
       endif else begin
       for i=0,n do begin
         if (ans eq sv_bigw(i)) then begin
           chan,sv_bigw(i)
           wdelete,sv_bigw(i)
           del_window_flag(i)=1 
         endif 
       endfor    
       endelse
       end
    1: sv_window_flag(n)=1 
    3: goto,OPT_MENU
ENDCASE
goto,OPT_MENU
;
SCALE:
print,'Display same region of image, with different intensity scaling:'
min = (max = '')
read,'MINIMUM intensity value of BIGIM to be considered [0]?',min
read,'MAXIMUM intensity value to be considered [300]?',max
if (min eq '') then min=0 else min=fix(min)
if (max eq '') then max=300 else max=fix(max)
xfin = (xnew + 511) < (xsizb - 1)
yfin = (ynew + 511) < (ysizb - 1)
newbigim = bigim(xnew:xfin,ynew:yfin)
chan,bigw
CTV,(BYTSCL(newbigim,MAX=max,MIN=min))   ;Rescale that region of big image
goto,OPT_MENU
;
DONE:
; Delete remaining big window if it has not been saved
if (sv_window_flag(n) eq 0) then begin
  chan,bigw & wdelete,bigw
endif 
; Make the little image window active, as it was when SPYGLASS was called  

chan,litw  

end
