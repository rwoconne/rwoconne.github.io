pro zero,iplane
;+
; NAME:
;	ZERO
; PURPOSE:
;	Erase an image within a window (without deleting the window)
;
; CALLING SEQUENCE:
;	ZERO		;Erase the current window
;	ZERO,IPLANE	;Erase window IPLANE
;
; OPTIONAL INPUT:
;	Iplane - Image plane to be erased.  If omitted, the current window
;		in the common block TV will be erased.
;
; OUTPUTS:
;	None.
;
; SIDE EFFECTS:
;	A window is erased.
; COMMON BLOCKS:
;	If the IMAGES common block had been set, the size and starting
; 	position of the image is set to the default.
; PROCEDURE
;	ERASE is called.
; MODIFICATION HISTORY:
;	Written B. Hill STI Corp. December 1985
;	to workstation use.  M. Greason, STX, May 1990.
;	Removed window_state function  W. Landsman STX, May 1991
;-
On_error,2
common tv,chan,zoom,xroam,yroam
common images,x00,y00,xsize,ysize
;
;			Check parameter.
;
if N_params() EQ 0 then iplane = chan			; Channel not spec.
device,window_state=opnd
n_planes = n_elements(opnd) - 1

if (iplane LT 0) or (iplane GT n_planes) then $   	; Invalid channel.
     message,'ERROR - Window index must be between 0 and ' + strtrim(n_planes,2)

if opnd(iplane) EQ 0 then $			; Window not open.
     message,'ERROR - Window '+ strtrim(iplane,2) + ' not open'
;
;			Clear the window.
;
wi = !d.window						; Save curr. window.
wset, iplane						; Erase window.
erase
if not !quiet then print,'Window',string(iplane,'(i2)'),' erased'
;
;			Reset common block values.
;
if N_elements(x00) GT 0 then begin
    x00(iplane) = 0 & y00(iplane) = 0
    xsize(iplane) = 0 & ysize(iplane) = 0
endif
;
;			Restore the current window.
;
if (wi GE 0) and (wi LE n_planes) then wset, wi
;
return
end
