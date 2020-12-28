pro square
;+
;  NAME:
;    SQUARE
;  PURPOSE:
;    Modify !P.POSITION, (if necessary) so that the current plotting area
;    is square.   To return to the default plotting window set !P.POSITION=0
;    !P.POSITION must be reset every time the plotting device is changed.
;  CALLING SEQUENCE:
;    SQUARE
;  INPUTS:
;    None
;  OUTPUTS:
;    None
;  REVISION HISTORY:
;    Written   W. Landsman                      May, 1991
;-
xmar = !X.MARGIN*!D.X_CH_size
ymar = !Y.MARGIN*!D.Y_CH_SIZE
xlen = !D.X_VSIZE - xmar(0) - xmar(1)
ylen = !D.Y_VSIZE - ymar(0) - ymar(1)
if xlen GT ylen then begin
      set_SCREEN,xmar(0),xmar(0)+ylen,ymar(0),ymar(0)+ylen
endif else if ylen GT xlen then begin
       set_SCREEN,xmar(0),xmar(0)+xlen,ymar(0)+ylen-xlen,ymar(0)+ylen
endif
return
end
