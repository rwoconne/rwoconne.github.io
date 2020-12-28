PRO astromit, x, y, a, d, hdr, fl, UNDISTORT=undist, DMPTYPE=dmptype, MODEL=mdl
;+
; NAME:
;	ASTROMIT
; PURPOSE:
;	Uses the procedure ASTROM to derive an astrometry solution from a 
;	set of reference star positions and coordinates.  The reference stars 
;	may be culled interactively to obtain the best astrometric solution.     
;	The procedure writes to a file ASTROM.DMP during each iteration.
;
; CALLING SEQUENCE:
;	ASTROMIT, X, Y, A, D [, HDR, FL, /UNDISTORT, MODEL=mdl]
;
; INPUTS:
;	X - Vector (at least 3 elements) containing the X positions of a set
;		of astrometric reference stars
;	Y - Vector containing Y positions of reference stars
;	A - Vector (same # of elements as X) containing the Right Ascension 
;		(decimal degrees) of the astrometric reference stars
;	D - Vector containing declinations (decimal degrees) of reference stars
;
; OPTIONAL INPUTS:
;	HDR - Image header which may be updated with the astrometric solution.
;	FL  - Camera focal length.  If not supplied, a focal length appropriate
;		to UIT is assumed
; KEYWORDS:
;	UNDISTORT - If present and non-zero, the reference star (x,y) positions
;		are assumed to have been corrected for image distortion
;		(transformed into the undistorted frame of reference).  THIS
;		KEYWORD SHOULD ONLY BE USED FOR UIT IMAGES.  NOTE:  THIS
;	        PROCEDURE DOES NOT TRANSLATE POSITIONS; IF DISTORTION IS AN
;	        ISSUE THEN THE COORDINATES MUST BE TRANSFORMED BEFORE CALLING
;	        THIS PROCEDURE!
;	MODEL-  Forces the selection of the Astro1 (if set to 1) or Astro2
;	        (if set to 2) distortion model.  This is only applicable if
;	        UNDISTORT has been specified.  If MODEL is not supplied then
;	        the ASTRO header keyword is used to determine the model.
;	DMPTYPE-  Default = 0
;               If 0, printed information will NOT be dumped.  If 1,
;		printed information will be dumped to ASTROM.DMP in the
;		same format as the screen.  If 2, a higher precision and
;		sexigesimal format will be dumped to file ASTROM.DMP.
;		DmpType=2 is noticeably slower...
; METHOD:
;	The procedure ASTROM is called (twice) to compute an initial astrometry
;	solution.  The errors in the initial solution are then displayed for
;	each reference star.  The user then has the option of culling the
;	the reference star list, either by setting a maximum tolerable error
;	or by removing individual stars.
; NOTES:
;	ASTROMIT will write the results of each iteration to a file ASTROM.DMP.
;	However, under Unix only the final iteration is saved.
;
; PROCEDURES USED:
;	ZPARCHECK,ASTROM,PUTAST,STRN,STRNUMBER,SXADDHIST
; REVISION HISTORY:
;	Written R. Cornett, W. Landsman               January 1988
;	Fixed to allow header updates if npar GE 5, B Pfarr, June 1989
;	20-AUG-91 Fixed so that it correctly puts residual in header.  
;	   (E. Deutsch)
;	24-AUG-91 Modified to work with latest mods to ASTROM.   (E. Deutsch)
;	Corrected ASTROM bug, requiring a change in how the residual is put
;	   into the header.  M. R. Greason, Hughes STX, 10 December 1992.
;	Converted to the new, image-distortion-related, astrometry scheme.
;	   Reformatted the procedure and beefed up the internal documentation.
;	   M. R. Greason, Hughes STX, 6 January 1993.
;	Fixed misnomers in calling sequence, bug in ctype handling, a couple
;	   of instances of not including the structure name astr.  
;	   RSH, HSTX, 25-Mar-93
;	Fixed a loop init. and processing.  MRG, HSTX, 8-Apr-1993.
;	Individual star deletion fixed (`where' added).  RSH, HSTX, 18 June 93
;	Individual star deletion fixed again.  RSH, HSTX, 20 Apr 95.
;       Added dmptype keyword, NRC, HITC, 26 Jul, 1995
;	Corrected a subtle bug in distortion application.  Added the Astro2
;	   distortion model.  MRG, HSTX, 4 Aug 1995.
;-
On_error,2
;
;			Check parameters.
;
npar = N_params()
error = string(7b) + 'ASTROMIT: ERROR - '
IF (npar LT 4) THEN BEGIN
	print, 'Syntax - ASTROMIT, X, Y, A, D, [ HDR, FL ,', $
		'/UNDISTORT,/DMPTYPE,MODEL=mdl]'
	print, '  X,Y - X,Y positions of reference pixels
	print, '  A,D - Right Ascension and Declination vectors in DEGREES'
	RETURN
ENDIF
;
 zparcheck, 'ASTROMIT', x, 1, [1,2,3,4,5], 1, 'X coordinate vector'
 zparcheck, 'ASTROMIT', y, 2, [1,2,3,4,5], 1, 'Y coordinate vector'
 zparcheck, 'ASTROMIT', a, 3, [1,2,3,4,5], 1, 'Right Ascension (Degrees)'
 zparcheck, 'ASTROMIT', d, 4, [1,2,3,4,5], 1, 'Declination (Degrees)'
;
;				Initial keyword processing.
;
IF (n_elements(dmptype) EQ 0) THEN dmptype = 1
;
undist = keyword_set(undist)
telescop = strupcase(strtrim(sxpar(hdr, 'TELESCOP'), 2))
IF (telescop NE 'UIT') THEN undist = 0
;
astromiss = 0				; ASTRO mission.
IF (undist) THEN BEGIN
	IF (NOT keyword_set(mdl)) THEN astromiss = sxpar(hdr, 'ASTRO') $
	                          ELSE astromiss = mdl
ENDIF
;
n = n_elements(x)			; Sufficient # of reference objects?
IF (n LT 3) THEN $
     message,'ERROR - At least 3 stars are required for an astrometric solution'
;
IF (npar LT 6) THEN BEGIN		; Focal Length Supplied?
	print, 'Assumed Focal Length (UIT): 170000 Pixel Units'
	fl = 1.7D5
ENDIF
;
;			If the FITS header was supplied, extract useful info.
;			If it wasn't, ask for the info.
;
cam = ''
IF (npar GE 5) THEN BEGIN
	naxis1 = sxpar(hdr, 'NAXIS1')
	naxis2 = sxpar(hdr, 'NAXIS2')
	IF telescop EQ 'UIT' THEN BEGIN
                filter = sxpar(hdr, 'FILTER')
		cam = strmid(strupcase(strtrim(filter, 2)), 0, 1)
	ENDIF
	eqnx   = sxpar(hdr, 'EQUINOX')
	IF (!err LT 0) THEN BEGIN
		eqnx = sxpar(hdr, 'EPOCH')
		IF (!err LT 0) THEN eqnx = 2000.
	ENDIF
ENDIF ELSE BEGIN
	read, 'Enter X and Y dimensions of image: ', naxis1, naxis2
	read, '      If UIT, enter the camera id: ', cam
	cam = strmid(strupcase(strtrim(cam, 2)), 0, 1)
	read, '    Equinox of the plate solution: ', eqnx
ENDELSE
;
;			Define the initial guess at the plate solution.  Zero
;			the CD's out.  Use the mean RA and DEC for CRVAL,
;			assuming the reference pixel to be at the image center.
;			Create the plate solution structure.
;
flag = 'F'				; Don't correct for distortion yet!
ctype = ['RA---TAN', 'DEC--TAN']
IF (((cam EQ 'A') OR (cam EQ 'B')) AND (undist)) THEN BEGIN
	IF (astromiss EQ 1) THEN ctype = ['RA---UIT', 'DEC--UIT'] ELSE $
	IF (astromiss EQ 2) THEN ctype = ['RA--UIT2', 'DEC-UIT2']
ENDIF
crpix = ([naxis1, naxis2] - 1.) / 2. + 1.
crval = [total(a), total(d)] / float(n)
idgs = indgen(n)			; idgs contains the id #'s of the stars
;
if strpos(ctype(0),'UIT') GT 0 then $
	astr = buildast( [[0., 0.], [0., 0.]], crpix, crval, ctype, $
						flag, cam, eqnx ) else $
	make_astr,astr,CRPIX=crpix,CRVAL=crval,CTYPE=ctype
;
; 			Perform the preliminary ASTROM runs.  The first run
;			essentially computes a CRVAL for the plate center.  
;			The second computes a plate solution using that CRVAL.
;
astrom, a, d, idgs, x, y, astr, err, fl, /NOQUES
astrom, a, d, idgs, x, y, astr, err, fl, /NOQUES, DUMP=dmptype
;
;			Interactive section.  This consists of a command loop
;			that allows the user to specify a maximum residual
;			followed by a loop that allows the removal of specific
;			stars.  The resultant list, with those objects dropped,
;			is used to update the solution.
;
;				Start by asking if the preliminary solution
;				needs to be improved.
;
ans = ''
read, 'Do you want to improve the solution [N]? ', ans
yn = strmid(strupcase(ans), 0, 1)
;
WHILE (yn EQ 'Y') DO BEGIN
;
;				Remove stars with residuals above a given
;				value.
;
	REPEAT BEGIN
		good = 0
		ans = ''
		read, 'Maximum tolerable error in arcsec ' + $
			'([RETURN] to continue): ', ans
		IF (ans NE '') THEN BEGIN
			good = strnumber(ans, errlim)
			IF (good EQ 0) THEN BEGIN
				print,error,'A numeric value must be supplied.'
			ENDIF ELSE BEGIN
				remove = where (err GT errlim, nbad)
				IF (nbad GT 0) THEN idgs(remove) = -1
			ENDELSE
		ENDIF
	ENDREP UNTIL ((ans EQ '') OR (good NE 0))
;
;				Now remove stars individually at the user's
;				whim. 
;
	REPEAT BEGIN
		ans = ''
		read, 'Index number of a star to hand-remove ' + $
			'([RETURN] to continue): ', ans
		IF (ans NE '') THEN BEGIN
			good = strnumber(ans, iremove)
			IF (good EQ 0) THEN BEGIN
				print,error,'A numeric value must be supplied.'
			ENDIF ELSE BEGIN
				remove = where(idgs EQ iremove, nbad)
				IF (nbad GT 0) THEN idgs(remove) = -1
			ENDELSE
		ENDIF
	ENDREP UNTIL (ans EQ '')
;
;				Now redo the solution with the restricted star
;				list.
;
	idgs = idgs( where(idgs GE 0, n) ) 	; Make the new ID list.
	IF (n LT 3) THEN BEGIN
		print, error, 'At least 3 stars are required for an ' + $
			'astrometric solution'
		RETURN
	ENDIF
	print, ' ******* SOLUTION USING THE CULLED LIST OF STARS ****** '
	astrom, a, d, idgs, x, y, astr, err, fl, /NOQUES, DUMP=dmptype
;
;				Ask if the user wishes to continue improving
;				the solution.
;
	ans = ''
	read, 'Do you want to improve the solution [N]? ', ans
	yn = strmid(strupcase(ans), 0, 1)
;
ENDWHILE
;
;			Calculate and display the rotation angle and plate
;			center info.
;
print, FORMAT='(/,A)', 'Final Astrometric Solution'
getrot, astr
print, 'Plate center: degrees: ', astr.crval
print, '               pixels: ', astr.crpix
print, FORMAT="('    The CDs (deg/pix): ',E13.6,'  ',E13.6)", $
	astr.cd(*,0)
print, FORMAT="('                       ',E13.6,'  ',E13.6)", $
	astr.cd(*,1)
print, ' '
;
;			Update the header?
;
IF (npar GE 5) THEN BEGIN
	ans = ''
	read, 'Do you want to add these astrometry parameters ' + $
		'to the image header [N]? ', ans
	IF (strmid(strupcase(ans),0,1) EQ 'Y') THEN BEGIN
		IF (((cam EQ 'A') OR (cam EQ 'B')) AND (undist)) THEN $
			astr.flag = 'T'
		putast, hdr, astr, cd_type=2
		sxdelpar, hdr,['CDELT1','CDELT2']
		label = 'ASTROM:' + strmid(!stime, 0, 17) + ' '
		nmatch = n_elements(idgs)
		hst = label + 'ASTROMETRIC SOLUTION USED n=' + strn(nmatch) + $
			' STARS'
		sxaddhist, hst, hdr
		resid = sqrt(total(err ^ 2) / nmatch)
		hst = label + 'RMS RESIDUAL: ' +	$
			strn(resid, FORMAT='(F5.2)') +	$
			'" RESIDUAL/SQRT(n): ' + 	$
			strn(resid / sqrt((nmatch-3) > 1), FORMAT='(F6.3)')+'"'
		sxaddhist, hst, hdr
	ENDIF ELSE print,'Image header not modified
ENDIF
;
RETURN
END
