PRO SAVLUT,RED,GREEN,BLUE  	;Save Current Color LUT 
;+
; NAME:
;	SAVLUT
; PURPOSE:
;	Read current color tables and save them in a file or in arrays.
;
; CALLING SEQUENCE:
;	SAVLUT, [Red, Green, Blue  ]
;
; INPUT PARMETERS:
;	None
;
; OPTIONAL OUTPUT PARAMETERS:
;	Red   - 256 element int array containing the red color table
;	Green - 256 element int array containing the green color table
;	Blue  - 256 element int array containing the blue color table
;		If no parameters are supplied, program will ask for the
;		name of a file to store the color tables.  If a file
;		name is not supplied, then the colors will be stored in the
;		common block COLORS.
;
; COMMON BLOCKS:
;	The user may store the currently displayed color table in the common
;	block COLORS.
;
; PROCEDURE:
;	The IDL procedure TVLCT, with the GET keyword set, is used to read 
;	the color table
;
; MODIFICATION HISTORY:
;	Written, W. Landsman STI Corp. August 1986
;	Converted to IDL Version 2.  M. Greason, STX, June 1990.
;-
COMMON TV,CHAN,ZOOM,XROAM,YROAM
COMMON COLORS,R,G,B,R_CURR,G_CURR,B_CURR
;
red = intarr(256) & blue=red & green=red	;Define color arrays
tvlct,red,green,blue,/get			;Fill color arrays.
red = fix(red)
green = fix(green)
blue = fix(blue)

if N_params() EQ 0 THEN BEGIN
  NAME = ''
  PRINT,'Enter file name without quotes or extensions' 
  READ,'Or hit RETURN to store in common blocks: ',NAME
  IF NAME EQ '' THEN BEGIN              ;Store in common blocks?
    R = RED & B = BLUE & G = GREEN
    r_curr = red & b_curr = blue & g_curr = green
  ENDIF ELSE BEGIN			;Store in a disk file?
    get_lun, unit
    OPENW, unit, NAME
    n = n_elements(red)
    tmp = intarr(3 * n)
    tmp = [red,green,blue]
    writeu, unit, n
    writeu, unit, tmp
    free_lun,unit
    NAME = STRUPCASE(NAME)
    PRINT,'To restore this color table - type GETLUT,red,green,blue,"'+NAME+'"'
  ENDELSE
ENDIF
;
RETURN
END
