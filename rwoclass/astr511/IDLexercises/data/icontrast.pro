PRO ICONTRAST,im,minimum=minimum,maximum=maximum,ctnum=ctnum,$
               reverse=reverse 
;+
; NAME:
;	ICONTRAST  ;; Interactive contrast enhancement for 8 or 24-bit displays

; PURPOSE:
;	Use the mouse to change the slope and Y intercept of the color
;	lookup table on an X windows display on a 24-bit monitor
;       Intended as a demo program only, for IDL Tutorial.
;
; CALLING SEQUENCE:
;	ICONTRAST,IM,MINIMUM=MINIMUM,MAXIMUM=MAXIMUM,CTNUM=CTNUM,REVERSE=REVERSE
;
; INPUT:  
;        IM = 2-D image ; required in the case of a 24-bit display;
;             but no input is required if display is 8-bit and
;             the current window already contains an image. 
;
; OPTIONAL KEYWORD INPUTS
;
;        MIMIMUM,MAXIMUM: minimum and maximum values for display 
;                         as in CTVSCL.  Both must be entered if one
;                         is entered.
;        CTNUM:  standard IDL color table number for loading into display
;                before image is loaded.  Default = 0.
;        REVERSE:  color table will be reversed before being applied 
;
; OUTPUT:
;	None
;
; SIDE EFFECTS:
;	Mouse button enabled:	LEFT - Reverses color table
;				RIGHT - Exit
;
;       24-bit displays are reset to "device,decomposed=0" for
;          emulation of indexed-color display
;
; PROCEDURE:
;	The y position (0-!D.Y_SIZE) is converted into a slope of range -10 
;	to 10 by using a tangent function.  The x position (0-!D.X_SIZE) is 
;	converted into a Y-intercept range of -128 to 128.
;
;       Program requires access to MOUSSE routine "CTVSCL" 
;
; COMMON BLOCKS:
;	The COLORS common block (see LOADCT) store the modified color table
;	The CONTRAST common block stores the final position of the cursor.
;
; REVISION HISTORY:  
;	Adapted from IVAS version on contrast, B. Pfarr, STX, 1/91
;	Added CONT parameter   W. Landsman       STX    August, 1991
;       First update for true-color, 24-bit display:  4/19/09 RWO
;       Complete update for true-color 24-bit  display; 
;         input sequence modified; 
;         program is intended for demo-mode only:   8/5/13 RWO
;       Keyword REVERSE added: 8/6/13 RWO 
;----------------------------------------------------------------

if (n_params(0) eq 0) then begin
   print,'Syntax - icontrast,im,minimum=minimum,maximum=maximum,'
   print,'         ctnum=ctnum, reverse=reverse'
   return
  endif

On_error,2                            ;Return to caller

;; Note: common blocks from original "contrast" routine are removed. 

;; Determine visual class.

   device,get_visual_name=vname,get_visual_depth=vdepth
   mdepth=8    ;;; 8-bit pseudocolor is the default
   if (vdepth eq 24) then mdepth=24

ctab=0    ;; Default is color table 0 
rev=0

if keyword_set(ctnum) then ctab=ctnum
if keyword_set(reverse) then rev=1

;; If display is 8-bit pseudocolor, then program will execute as
;;   in the 1991 version, using the already-displayed image
;;   and color table.  

;; If display is 24-bit truecolor, then program will load
;;   image and color table ab-initio after setting the
;;   display to emulate indexed-color.

if (mdepth eq 24) then begin

   ;; PROGRAM CHANGES MODE TO DECOMPOSED=0 on 24 bit display 

   ;; NOTE: program uses CTVSCL here rather than TVSCL so
   ;;   will have nice window-adjustment features  

   device,decomposed=0
   loadct,ctab

   if (keyword_set(minimum) or keyword_set(maximum)) then begin

     ;; Program assumes that if either "minimum" or "maximum"
     ;;   keyword is set, both are set.  

         mind=minimum
         maxd=maximum
         ctvscl,im,mindata=mind,maxdata=maxd,/silent

       endif else ctvscl,im,/silent

  endif

;; if (N_elements(r) NE !D.N_COLORS) then tvlct,r,g,b,/get

tvlct,r,g,b,/get

if (rev eq 1) then begin
     r=reverse(r)
     g=reverse(g)
     b=reverse(b)
  endif 

pi_4 = !pi/4 & ramp = findgen(256)        ;Useful constants

if (!D.FLAGS and 256) EQ 256 then wshow,!D.WINDOW     ;Bring up current window

y_mid = !D.Y_VSIZE/2 & xscale = 256./!D.X_VSIZE

tvlct,r,g,b

tvcrs,!D.X_VSIZE/2,y_mid	;Initialize at slope of 1, midpoint of 128

print,'Move cursor UP   to increase contrast
print,'Move cursor LEFT to increase zero level'
print,'Hit LEFT Mouse button to reverse color table'
print,'Press RIGHT Mouse button to exit CONTRAST'
cursor,/device,imp_old,isl_old,0                       ;Current cursor position

while !ERR ne 0 do cursor,/device,imp_old,isl_old,0    ;Empty the mouse buffer  
plus = 1b
Tb:	wait,0.13           ;let others use the CPU
        if ( !err EQ 4) then goto, cleanup
	cursor,/device,imp,isl,2	         ;read cursor position
        IF !err eq 1 THEN BEGIN             ;Reverse color table?
                plus = 1b - plus
                dis = 1
        endif else dis = abs(imp_old - imp) + abs(isl_old - isl)
        if dis GE 1 then begin
            imp_old = imp & isl_old = isl
            slope = tan(isl*pi_4/y_mid)
            y_int = imp*xscale-128
            x = slope*(indgen(256) +y_int) >0 <255
            r_new = r(x)
            g_new = g(x)
            b_new = b(x)
 	;Load linear scale to ITT
	    if not plus then begin
               tvlct,reverse(r_new),reverse(g_new),reverse(b_new)
            endif else tvlct,r_new,g_new,b_new

         ;; On 8-bit display, no further action is needed

         if (mdepth eq 24) then begin

            ;; On 24-bit display, must re-load image after each
            ;;   adjustment of color tables

              if (keyword_set(minimum) or keyword_set(maximum)) then begin
                 mind=minimum
                 maxd=maximum
                 ctvscl,im,mindata=mind,maxdata=maxd,/silent
              endif else ctvscl,im,/silent

           endif
      endif

goto,tb

cleanup:

return
end
