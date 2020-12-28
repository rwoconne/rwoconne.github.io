FUNCTION window_state, dum
;+
; NAME:
;	WINDOW_STATE
; PURPOSE:
;	Returns an array indicating which workstation windows are open.
;
; CALLING SEQUENCE:
;	state = window_state()
;
; INPUTS:
;    None.
;
; OUTPUTS:
;    state  A 128 element array, one element per window, containing ones
;           and zeros.  A one indicates the window is open while a zero 
;           indicates the window is closed.
;
; PROCEDURE:
;    The device procedure is used.  NOTE:  The window_state keyword of the
;    device procedure will open window zero unless it is already open.
;    Therefore, !D.WINDOW is first checked to see if any windows are open.
;
; REVISION HISTORY:
;    Written by Michael R. Greason, August 1990.
;    Added !D.WINDOW W. Landsman,   October 1990.
;-
;
if !D.WINDOW lt 0 then opnd = intarr(128) else $
    device,window_state=opnd          ;Array that tells which windows are open.
;
RETURN, opnd
END
