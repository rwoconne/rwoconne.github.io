pro itt,type
;+
; NAME:
;	ITT
; PURPOSE:
;	Load or disable an intensity transformation table (ITT).   It is
;	usually used to either (1) enhance weak features or (2) reverse
;	positive and negative.
;
; CALLING SEQUENCE:
;	ITT, [ type ]
;
; OPTIONAL INPUT PARAMETERS:
;	TYPE - The type of ITT transformation; (0) disable the ITT, (1) linear 
;		scale, (2) square root scale, (3) cube root scale, and 
;		(4) logarithmic scale.
;		A negative number will reverse positive and negative.  If this 
;		parameter is omitted, a menu will appear.
; OUTPUT PARAMETERS:
;	None.
;
; COMMON BLOCKS:
;	The common block ITTCOM stores the 256 element array used to transform
;	the color vectors.
;
; PROCEDURE:
;	The color vectors are multiplied by the intensity transformation table
;	and loaded with TVLCT
;
; MODIFICATION HISTORY:
;      Written, W. Landsman, August 1986
;-
common ittcom,ittval 
common tv,chan,zoom,xroam,yroam
common colors,r,g,b,r_curr,g_curr,b_curr
if N_params() eq 0 then begin	             ;Display menu?

MENU: 
 print,'Choose the type of ITT transformation'
 print,'0: Disable the ITT'
 print,'1: Linear scale'
 print,'2: Square root scale'
 print,'3: Cube root scale'
 print,'4: Logarithmic Scale'
 print,'   Choose a negative number to reverse positive and negative'
 read,type
endif

if abs(type) gt 1 then buffer = findgen(256)/255.

case abs(type) of
0: ittval = replicate(1,256)
1: ittval = replicate(1,256)   
2: ittval = [255,buffer(1:*)^(-0.5)]
3: ittval = [255,buffer(1:*)^(-0.6667)]
4: begin 
   buffer = findgen(256)
   ittval = buffer/255.
   ittval = [ 255, alog10(buffer(1:*) + 1.)/(2.4082*ittval(1:*))]
   end
else: begin
        print,'An ITT of type'+string(type,'(i3)')+' is not an allowed choice'
        goto, menu 
      end
endcase

if N_elements(r) eq 0 then savlut,r,g,b

rr = fix( (r*ittval+0.5) < 255)
gg = fix( (g*ittval+0.5) < 255)
bb = fix( (b*ittval+0.5) < 255)
if type lt 0 then tvlct,255-rr,255-gg,255-bb else tvlct,rr,gg,bb
return
end
