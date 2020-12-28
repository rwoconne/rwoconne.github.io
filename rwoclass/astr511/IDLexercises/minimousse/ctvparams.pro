pro CTVparams, NP,P,H,HYN,index,TFlag
;+
; NAME:
;	CTVParams
; PURPOSE:
;	Used by CTV and CTVSCL to sort out numerical (integer) parameters
;	and FITS header.
; INPUTS:
;	P - i-th parameter of undetermined type.
;	Index- number i (i.e. ordinal position of P among all parameters {P})
; OUTPUTS
;	NP- INTARR to receive numerical values of P
;	H - FITS header (same value as parameter with type STRARR)
;	HYN - Boolean, takes value of 1 if H is filled.
; 	TFlag- Boolean, takes value of 1 if routine runs into trouble.  Messages
;		indicating type of trouble will be printed to screen.
;	Index- incremented if an element of NP is filled.
; RESTRICTIONS:
;	P must be a string array or a scalar.
;	NP must be of type INTARR, FLTARR, DBLARR
; COMMENT:
; 	Routine was created to work with CTV and CTVSCL program and thus is
;	fairly eccentric for general use.  IF THIS ROUTINE IS MODIFIED, MAKE
;	SURE CTV and CTVSCL ARE ALSO MODIFIED TO ACCOMODATE THE CHANGES.
; Modifcation history:
;	Written 	J.D. Offenberg	Oct, 1991
;-

IF Datatype(P) eq 'STR' AND N_elements(P) GT 1 THEN begin	;FITS header
	IF HYN then begin
		message,'MULTIPLE FITS HEADERS',/inf 
		TFlag = 1
	endIF else begin
		H = P
		HYN = 1
		return
	endELSE
endIF ELSE $
   IF Datatype(P) NE 'STR' and N_elements(P) EQ 1 THEN begin	;Not FITS header
	NP(index) = Fix(P)
	index = index + 1
   endIF $
ELSE Begin
	message,'WRONG DATA TYPE:  '+datatype(P)+'  '+string(n_elements(P)),/inf
	TFlag = 1
	return
   endELSE
end
