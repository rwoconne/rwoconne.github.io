PRO ICROLL,im,min=min,max=max,ctnum=ctnum,crate=crate,cdir=cdir
;+

; NAME:
;    ICROLL    ; Roll Color Lookup table, 24-bit display version

; PURPOSE:

;   Demonstration program which continuously rolls a color lookup table at a fixed rate.  

; CALLING SEQUENCE:

;    ICROLL,im,min=min,max=max,ctnum=ctnum,crate=crate,cdir=cdir

; INPUT:

;     IM = 2-D image; image will be loaded whether display
;          is 8-bit or 24-bit.

; OPTIONAL KEYWORD INPUTS: 

;    MIN,MAX:  minimum and maximum values for display 
;              as in CTVSCL.  Both must be entered if one is entered.
;      CTNUM:  standard IDL color table number for loading into display
;              before image is loaded.  Default = 0.
;      CRATE:  Rate in units of 0.1 seconds at which the color table
;              is updated.
;       CDIR:  If present and non-zero, then color table rolls to the left.
;              Otherwise color table rolls to the right

; OUTPUTS:   None.  

; SIDE EFFECTS:

;       24-bit displays are reset to "device,decomposed=0" for
;          emulation of indexed-color display

; PROCEDURE:

;    The image is displayed using specified scaling for CTVSCL.
;    Each color vector is shifted incrementally and the
;    image is redisplayed until the user presses a key. 


; REVISION HISTORY:
;    W. B. Landsman, ST Systems   February, 1988  as "CROLL"
;    Modified for workstations.  M. Greason, STX, June 1990.
;    Updated for true-color 24-bit displays; input 
;       sequence modified; program is intended for
;       demo mode only: 8/5/13  RWO
;    Doc update: 8/6/13 RWO
;--------------------------------------------------------------------

if (n_params(0) eq 0) then begin
   print,'Syntax - icroll,min=min,max=max,ctnum=ctnum,crate=crate,cdir=cdir'
   return
endif

;; Note: common blocks from original "croll" routine are removed. 

;   Set defaults.

rate=1
direction=1

if keyword_set(crate) then rate=crate
if keyword_set(cdir) then direction=-1


;; Determine visual class.

   device,get_visual_name=vname,get_visual_depth=vdepth
   mdepth=8    ;;; 8-bit pseudocolor is the default
   if (vdepth eq 24) then mdepth=24

ctab=0    ;; Default is color table 0 

if keyword_set(ctnum) then ctab=ctnum

;;  The program will load image and color table ab-initio.
;;  In the case of a 24-bit display, this will be done
;;  after setting the display to emulate indexed-color.

loadct,ctab

if (mdepth eq 24) then begin

   ;; Note: program changes mode to DECOMPOSED=0 on 24 bit display 

   device,decomposed=0
 
  endif 
 
;; Load the original image 
;; NOTE: program uses CTVSCL here rather than TVSCL so
;;   will have nice window-adjustment features  

if (keyword_set(min) or keyword_set(max)) then begin

     ;; Program assumes that if either "min" or "max"
     ;;   keyword is set, both are set.  

         mind=min
         maxd=max
         ctvscl,im,mindata=mind,maxdata=maxd,/silent

  endif else ctvscl,im,/silent

print,'Press any key to exit ICROLL'
shft = direction

tvlct,r,g,b,/get 

;
;	Loop until a key is struck, rolling the color table
;	continuously.
;

wait,1.0   ;; initial pause 

while get_kbrd(0) eq '' do begin 

   wait,.1*rate			; pause 
   red = shift(r,shft) & blue = shift(b,shft)
   green = shift(g,shft)
   tvlct,red,green,blue
   
   ;; For 8-bit displays, loading the color table completes a step
   ;; For 24-bit displays, we must reload the image. 
   
   if (mdepth eq 24) then begin
        if (keyword_set(min) or keyword_set(max)) then begin
            mind=min
            maxd=max
            ctvscl,im,mindata=mind,maxdata=maxd,/silent
          endif else ctvscl,im,/silent
     endif 

   shft = ((shft + direction) mod 256)    ;; increment the shift 

endwhile

;; Image is left in final rolled state when program exits. 

return
end

