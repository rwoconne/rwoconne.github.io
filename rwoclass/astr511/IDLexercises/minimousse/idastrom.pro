;
;+
; NAME:
;	IDASTROM
; PURPOSE:
;	To examine a header and return the type of astrometry stored in it
;	(Guidestar, UIT, tangent, or other).
; CALLING SEQUENCE:
;	id = idastrom(hd)
; INPUTS:
;	hd - The header to examine.
; RETURNED:
;	id - A string indicating the results of the test:
;		GSS   - A Guidestar plate solution,
;		UIT   - A UIT distortion-corrected solution (either model),
;		TAN   - A ['RA---TAN','DEC--TAN'] solution,
;		OTHER - Any other.
; PROCEDURE:
;	The CTYPE keywords are extracted.  If they aren't present then
;	the header is tested for a Guidestar keyword (PPO1).  If that is
;	present, the GSS is returned.  If the CTYPE keywords are present
;	then they are checked for the strings UIT and TAN.
; MODIFICATION HISTORY:
;	Written by Michael R. Greason, Hughes STX, 7 August 1995.
;-
;
FUNCTION idastrom, hd
;
on_error, 2
;
;			Check arguments.
;
ret = 'OTHER'
IF (n_params() LT 1) THEN message, 'Syntax:  id = idastrom(hd)'
;
s = size(hd)
IF ((s(0) NE 1) OR (s(2) NE 7)) THEN message, $
	'FITS Header (first parameter) must be a string array'
;
;			Use the 'CTYPE' keywords to id the astrometry in
;			the header.
;
ctype = strupcase(strtrim(sxpar(hd, 'CTYPE*', COUNT=nc), 2))
IF (nc EQ 0) THEN BEGIN
;
;				Guidestar?
;
	gsss = sxpar(hd, 'PPO1', COUNT=ng)
	IF (ng EQ 1) THEN ret = 'GSS'
ENDIF ELSE BEGIN
;
;				UIT or TAN?
;
	IF (strpos(ctype(0), 'UIT') GE 0) THEN ret = 'UIT' ELSE $
	IF (strpos(ctype(0), 'TAN') GE 0) THEN ret = 'TAN'
ENDELSE
;
RETURN, ret
END
