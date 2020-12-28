PRO TVSTATUS
;+
; NAME:
;   TVSTATUS
; PURPOSE:
;   Display common blocks and global variables associated with the 
;   image display for each open window.
; CALLING SEQUENCE:
;   tvstatus
; INPUTS:
;   None.
; OUTPUTS:
;   None.
; COMMON BLOCKS:
;   The values of the common block TV are displayed but not changed.
; PROCEDURE:
;   The status of the TV is read from the common block TV.
;   Also, the status of each of the available windows is read from 
;   the WINDOW_STATE array, OPND.
; REVISION HISTORY:
;   Modified to work with a workstation.  M. Greason, STX, June 1990.
;   Common block variable OPND replaced with WINDOW_STATE statement,
;                                             K. Rhode, STX, July 1990.
;   Only display the open windows W. Landsman STX December, 1990
;-
ON_ERROR,2                                     ;Return to caller
COMMON TV,CHAN,ZOOM,XROAM,YROAM
COMMON IMAGES,X00,Y00,XSIZE,YSIZE
IF (!D.FLAGS AND 8) NE 8 THEN $
    MESSAGE,'Current device '+!D.NAME + ' does not support windows' else $
    PRINT,'Current device name: '+ !D.NAME
;
; Print information in common block "TV"
;
; OPND is array which is 1 wherever the window is open
;
device,window_state=opnd
open = where(opnd,nopen)
IF NOPEN EQ 0 THEN MESSAGE,'No '+!D.NAME +' windows are currently open'
PRINT,'Current Active Window: ',STRTRIM(CHAN,2)
PRINT,' '
;
IF (N_ELEMENTS(ZOOM) EQ 0) THEN MESSAGE,'TV Common block not initialized.' 
;
PRINT,'                  Open Windows'
IF (N_ELEMENTS(X00) EQ 0) THEN BEGIN
  PRINT,'Image Plane  Zoom   XRoam  YRoam      '
  PRINT,' '
  FOR j=0,nopen-1 DO begin
        i = open(j)
        PRINT,format='(4I8,7X,A6)', i,zoom(i),xroam(i),yroam(i)
  ENDFOR
 ENDIF ELSE BEGIN
  PRINT,'Frame  Zoom  XRoam  YRoam   X00   Y00   X-Size  Y-Size'
  PRINT,' '
  FOR j=0,nopen-1 DO begin
      i = open(j)
      PRINT,format="(' ',I3,I6,1X,2I7,2I6,2I8,6X,A6)",$
      i,zoom(i),xroam(i),yroam(i),x00(i),y00(i),xsize(i),ysize(i)
  ENDFOR
ENDELSE
RETURN
END
