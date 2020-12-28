  ; STARTUP FILE mousse_startup
  ; Configures an IDL session to use the MOUSSE
  ;   software.
  ; Original name:  idl_startupx.pro
  ; Last Modified: 15 May 1992, 30 Sep 93, 27 Dec 93
  ;   8/94; 9/97
  ; 7/23/01: eliminate PSPRINTER

setplot, 'X'
DEFSYSV,'!DEBUG',0
DEFSYSV,'!TEXTUNIT',0
DEFSYSV,'!TEXTOUT',1
DEFSYSV,'!PRIV',0

cinit
!PROMPT = 'IDL>'
!EDIT_INPUT = 100
on_error,1

print,' *** on_error,1 is default ***
print,' '
print,' *** If terminal is VT100/TEK or equivalent, type SET_PLOT,TEK ***'
print,' '
