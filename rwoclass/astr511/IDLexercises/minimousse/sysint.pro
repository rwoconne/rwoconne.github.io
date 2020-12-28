pro sysint
;+
; NAME
;   SYSINT
; PURPOSE:
;   To initialize the workstation image windows and related IDL common blocks.  
;   All windows are deleted, all zoom factors set to unity, and all roam
;   factors set to zero.   The normal pixel logic (GXcopy) is implemented
;   (device,set_graphics=3).
; CALLING SEQUENCE:
;   SYSINT
; INPUTS:
;   None
; OUTPUTS:
;   None
; REVISION HISTORY:
;   Written for workstations.  M. Greason, STX, June 1990
;   OPND in common block TV replaced with WINDOW_STATE statement, 
;                                             K.Rhode, STX, July 1990.
;-
On_error,2
if !D.WINDOW NE -1 then begin
   device,window_state = opnd,set_graphics=3
   FOR i = 0, N_elements(opnd)-1 DO $
	IF opnd(i) THEN cdel, i
endif
cinit
;
return
end
