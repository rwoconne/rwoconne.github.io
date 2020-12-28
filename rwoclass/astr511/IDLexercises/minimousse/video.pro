PRO video, hdr, ra, dec, x, y, boxsz, BOXSIZE=boxsize, COLOR=color, $
	MODEL=mdl, UNDIST=undist
;+
; NAME:
;	VIDEO
; PURPOSE:
;	Overlays the positions of a set of reference stars derived from
;	a rough astrometry solution.  User can interactively adjust
;	astrometry parameters to better match the overlay with stars
;	on an image on the window display.
;
; CALLING SEQUENCE:
;	VIDEO, hdr, ra, dec, [ x, y, boxsz, BOXSIZE =, COLOR =, /UNDIST ]
;
; INPUTS:
;	RA    - vector of Right Ascension values (Decimal degrees)
;		At least 3 reference stars should be supplied
;	DEC   - vector of Declination values (Decimal degrees)
;
; OPTIONAL INPUTS:
;	BOXSZ - size of box to be drawn around reference stars (default = 7)
;
; INPUTS-OUTPUT:
;	HDR   - FITS Image header corresponding to the image on TV.  May
;		contain the initial astrometry solution.  Upon output
;		user can optionally add the improved astrometry to HDR.
;
; OPTIONAL OUTPUTS:
;	X     - vector containing X position of reference stars
;		Same number of elements as RA
;	Y     - vector containing Y position of reference stars
;
; OPTIONAL INPUT KEYWORD:
;	BOXSIZE - Scalar that can be used instead of the BOXSZ parameter
;		  to specify the width of the overlay boxes.
;	COLOR   - Scalar specifying the overlay intensity.  Default is !P.COLOR
;	MODEL   - Used with the UNDIST keyword to specify the UIT distortion 
;		  model.  This keyword indicates which distortion model to
;		  use (either 1 or 2).  If not set or if not 1 or 2, then the
;		  ASTRO keyword is used to select the model.  If the header
;		  does not describe an UIT then this keyword is ignored.  If
;		  UNDIST is not specified then this keyword is ignored.
;	UNDIST  - If present and non-zero, and if the hdr is a UIT header,
;		  image distortion is forced into the plate solution IF the
;		  header describes an UIT image.  The presense of this keyword 
;		  will override image distortion status initially contained in 
;		  the header; however, the absense of this keyword will not 
;		  force distortion to be ignored.
; DISCUSSION:
;	VIDEO requires a initial rough astrometry solution.  This
;	solution can be input to VIDEO in 3 possible ways:
;	(1) Astrometry can be included in the input image header.
;	    (PUTAST,HDR can be used to manually insert astrometry)
;	(2) In response to prompt, user may manually input astrometry
;           parameters. (Image center, plate scale, rotation)
;       (3) User can identify 2 stars on image display and enter their
;           RA and Dec.
;
; NOTES:
;	The image to examine must be the image displayed in the current window.
; PROCEDURES USED:
;	SCR_MOVE,EXTAST,PUTAST,STARAST,SXADDHIST,ZPARCHECK,TVBOX,AD2XY,
;	DIST_COORD_CONV
; REVISON HISTORY:
;	written by B. Pfarr, STX,  5/6/87 
;	converted to work on a workstation.  M. Greason, STX, 6 June 1990
;	Added BOXSIZE, COLOR keywords  W. Landsman   Hughes STX   December 1991
;	Added Extra Fine scale.  CRPIX inconsistency fixed.  
;		R. S. Hill, Hughes STX, 27-May-1992
;	Converted to the new, image-distortion-related, astrometry scheme.
;	Reformatted the procedure and beefed up the internal documentation.
;		M. Greason, Hughes STX, 7 January 1993.
;	Converted to new astrometry structure   WBL, Jan 1994
;	Work for non-UIT images WBL  Mar 1994
;	Translation moves ('R','L','U','D') now fixed in pixel space WBL 06/94
;	Astro2 distortion correction added.  MRG, HSTX, August 1995.
;-         
On_error, 2				; Return to caller on error.
;
;			Check parameters.
;
npar = n_params()
IF (npar LT 3) THEN BEGIN
	print, "Syntax - VIDEO, h, ra, dec,[ x, y, BOXSIZE=, COLOR=, /UNDIST ]
	print, '    h - FITS header (with or without astrometry)
	print, '    ra,dec - Input RA and Dec in DEGREES
	print, '    x,y - Ouput vectors containing positions of input stars
	RETURN
ENDIF
;
; 			Initialize some text-writing variables.
;
cr = string("15b)
err = string(7b)+'VIDEO: ERROR - '
inp = ''
nstars = min([ n_elements(ra), n_elements(dec) ])       
IF (nstars LT 2) THEN BEGIN
	print, err, 'The RA and Dec of at least 2 stars is required input'
	print, '               VIDEO,HDR,RA,DEC,[X,Y,BOXSZ]'
	RETURN
ENDIF ELSE zparcheck, 'VIDEO', hdr, 1, 7, 1, 'FITS image header'
;
IF (NOT keyword_set(color)) THEN color = !p.color
IF (keyword_set(BOXSIZE)) THEN boxsz = boxsize
undist = keyword_set(undist)
IF (n_elements(mdl) LE 0) THEN mdl = 0
IF ((mdl LT 1) OR (mdl GT 2)) THEN mdl = 0
;
; 			Bring the active window to the foreground.
;
wi = !d.window				; Save the window id.
wshow, !d.window
;
; 			Stuff the displayed image into a pixmap.
;
xsz = !d.x_vsize
ysz = !d.y_vsize
window, /FREE, XSIZE=xsz, YSIZE=ysz, /PIXMAP 
wp = !d.window				; Index of the pixmap window
device, COPY=[0,0,xsz,ysz,0,0,wi]	; Copy image into pixmap
wset, wi				; Reactivate the window.
;
; 			More initialization.
;
scr_curmov, 1, 1			; Get SCR_CURMOV compiled.
ra_rad = ra / !radeg			; Convert RA and DEC to radians.
dec_rad = dec / !radeg
telescop = sxpar(hdr, 'TELESCOP')	; Is it a UIT image?
astromis = 0				; UIT distortion model selection.
IF (telescop NE 'UIT') THEN undist = 0 ELSE BEGIN
	IF (mdl EQ 0) THEN astromis = sxpar(hdr, 'ASTRO') $
	              ELSE astromis = mdl
ENDELSE
;
;			If not specified, get the box size.
;
IF (n_elements(boxsz) NE 1) THEN BEGIN
	inp = ''
	read, 'Enter width of overlay boxes in pixels [7]: ', inp
	IF (inp EQ '') THEN boxsz = 7 ELSE boxsz = fix(inp)
ENDIF
inp = ''
;
; 			Try to extract the initial plate solution from the 
;			header.  If there's no solution in the header get 
;			one from the user.
;
sz = size(hdr)
extast, hdr, astr, noparams
IF (noparams LT 0) THEN BEGIN
;
;				No astrometry was available.
;
	read, 'Do you have an initial plate solution [N]? ', inp
	inp = strmid(strtrim(strupcase(inp), 2), 0, 1)
	IF (inp EQ 'Y') THEN BEGIN
;
;				Prompt for it.
;
		putast, hdr, cd_type=2 
		extast, hdr, astr, noparams
;
	ENDIF ELSE BEGIN
;
;				Estimate it.  Two stars are required for this.
;
;					Display the list of stars (if desired).
;
		print, 'You will asked to identify two stars on the ' + $
			'image display by ID'
		read, "Do you want a list of stellar ID's and " + $
			"coordinates [N]? ", inp
		inp = strmid(strtrim(strupcase(inp), 2), 0, 1)
		IF strupcase(inp) EQ 'Y' THEN BEGIN
			print,'ID   RA    DEC           RA             DEC'
			FOR i = 0, nstars-1 DO print, FORMAT='(I4,2F7.2,A)', $ 
				i, ra(i), dec(i), adstring([ra(i),dec(i)], 1)
		ENDIF
;
;					Get the id and position of the
;					first star.
;
		id1 = 0
		REPEAT BEGIN
			read, 'Enter star number for initial star: ', id1
		ENDREP UNTIL (id1 LE nstars)
		print, 'Position cursor on star, then hit a mouse button:'
		cursor, xcen1, ycen1, /WAIT, /DEVICE
		tvbox, boxsz, xcen1, ycen1   	; Draw box around first star.
;
;					Get the id and position of the
;					second star.
;
		id2 = 0
		REPEAT BEGIN
			read, 'Enter star number for second star: ', id2
		ENDREP UNTIL ((id2 LE nstars) AND (id2 NE id1))
		done = 0
		REPEAT BEGIN
			print, 'Position cursor on star, then hit a ' + $
				'mouse button:'
			cursor, xcen2, ycen2, /WAIT, /DEVICE
			IF (xcen2 EQ xcen1) AND (ycen2 EQ ycen1) THEN BEGIN
				print, err, "Star 2 can't be in the " + $
					"same place as Star 1!"
				device, COPY=[0,0,xsz,ysz,0,0,wp]
				tvbox, boxsz, xcen1, ycen1
			ENDIF ELSE done = 1
		ENDREP UNTIL (done NE 0)
;
;					Estimate the plate solution from
;					the two stars.
;
		device, COPY=[0,0,xsz,ysz,0,0,wp]
		xref = [xcen1, xcen2]   & yref = [ycen1, ycen2]
		a = [ra(id1),ra(id2)]   & d = [dec(id1),dec(id2)]
		starast, a, d, xref, yref, cd
		crval1 = [ ra(id1), dec(id1) ]
		crpix = [!d.x_vsize, !d.y_vsize] / 2. + 1.
;
		eqnx = sxpar(hdr, 'EQUINOX')
		IF (!err LT 0) THEN BEGIN
			eqnx = sxpar(hdr, 'EPOCH')
			IF (!err LT 0) THEN eqnx = 2000.
		ENDIF
		IF (telescop NE 'UIT') THEN cam = '' ELSE $
			cam = strupcase(strmid(strtrim(sxpar(hdr, 'FILTER'), $
				2), 0, 1))
		IF (((cam EQ 'A') OR (cam EQ 'B')) AND (undist)) THEN BEGIN
		   IF (astromis EQ 1) THEN ctype = ['RA---UIT', 'DEC--UIT'] $
		                      ELSE ctype = ['RA--UIT2', 'DEC-UIT2']
		   flag = 'T'
		   astr = buildast(cd, [xcen1,ycen1]+1, crval1, ctype, $
					flag, cam, eqnx)
		   uit_xy2ad, crpix(0), crpix(1), astr, racen, deccen
		   crval = [racen,deccen]	; Coordinates of center pixel
		   astr = buildast(cd, crpix, crval, ctype, flag, cam, eqnx)
		ENDIF ELSE BEGIN
                   make_astr,astr,CD = cd, CRPIX = [xcen1,ycen1]+1., $
				CRVAL=crval1,DELT = [1.0,1.0]
		   xy2ad, crpix(0), crpix(1), astr, racen, deccen
                   crval = [racen, deccen]
		   make_astr,astr,CD = cd, CRPIX = crpix, CRVAL = crval, $
			DELT = [1.0d,1.0D]
;
		ENDELSE
	ENDELSE
ENDIF
;
;			If UNDIST has been specified and the header describes
;			an UIT image, force distortion into the astrometry 
;			structure.
;
proj = idastrom(hdr)			; Astrometric projection type
IF (proj EQ 'GSS') then $ 
	message,'ERROR - Cannot improve upon ST guide star solution'
;
IF (undist) THEN BEGIN
	IF (astromis EQ 1) THEN astr.ctype = ['RA---UIT', 'DEC--UIT'] $
	                   ELSE astr.ctype = ['RA--UIT2', 'DEC-UIT2']
	astr.flag = 'T'
ENDIF
;
;			Determine the rotation angle.
;
getrot, astr, rotat_deg, cdelt
cdelt = abs(cdelt(0)) 
rotat = rotat_deg / !radeg
;
;			Write info describing the initial plate solution
;			to the screen.
;
print, 'Initial plate scale (Arcseconds/pixel): ', 3600 * cdelt
print, '                  Initial plate center: ', adstring(astr.crval)
print, '      Initial rotation angle (degrees): ', rotat_deg
;    
;			Use initial astrometry solution to obtain X,Y positions
;			of target stars.  Convert the positions into screen
;			positions.  Flag those that do not lie too close to
;			the edge of the image.
;
IF (proj EQ 'UIT') THEN uit_ad2xy, ra,dec, astr,x,y $
                   ELSE ad2xy, ra, dec, astr, x, y
zoom_xy,x,y
;                             
good = where( ((x-boxsz GE 0) AND (x+boxsz LT !d.x_vsize)) AND $
	((y-boxsz GE 0) AND (y+boxsz LT !d.y_vsize)), ngood)  
IF (ngood GT 0) THEN BEGIN
	tvbox, boxsz, x(good), y(good), color
	nbad = nstars - ngood
ENDIF ELSE nbad = nstars
;
;			Enter the command loop.  The user issues commands,
;			adjusting star positions, until the solution looks
;			right.  Two command loops are maintained.  The inner
;			one actually processed commands.  The outer one 
;			exists to give the user the chance to change his/her
;			mind about exiting.
;
;				First define the menu.
; 
menu = $
	['To move the boxes:     press  ', 	 $
	 '                              ', 	 $
	 '    to the Right         R    ', 	 $
	 '    to the Left          L    ', 	 $
	 '    Up                   U    ', 	 $
	 '    Down                 D    ', 	 $
	 '    Spread  apart        S    ', 	 $
	 '    closer Together      T    ', 	 $
	 '    Clockwise            C    ', 	 $
	 '    Anti-clockwise       A    ', 	 $
	 '                              ', 	 $
	 'To make Gross changes,press G ', 	 $
	 'To make Fine changes, press F ', 	 $
	 'To make Extra Fine changes, press X ', $
	 '                              ', 	 $ 
	 'To get this Help menu, press H', 	 $
	 '                              ', 	 $
	 'To Quit, press Q              ', 	 $
	 '                              ']
nmenu = n_elements(menu) - 1
blank = string(replicate(32b,55))
;
;				Start the outer command loop and display the
;				menu.  Display the status line.
;
REPEAT BEGIN
;
	print, 'TWEAKING...'
	FOR i = 0, nmenu DO print, menu(i)
	adjust = 1.
	scale = 'Gross scale'
	IF (nbad NE 0) THEN print, FORMAT='($,A,A,10x,A,15x,A,I3,A,A)', $
	       cr,inp,scale,'WARNING:',nbad,' stars outside screen display' $
	ELSE print, FORMAT='($,A,A,10x,A,A)',cr, inp, scale, blank
;
;				Start the inner command loop.
;
	REPEAT BEGIN
;
;					Loop initialization.
;
		redraw = 1
		crpix0 = astr.crpix
;
;					Get the command.
;
		inp = strupcase( get_kbrd(1) )
		print, format='($,A,A)', cr,'Working....'
		scr_curmov, 2, 12
;
;					Decode and implement the command.
;
		CASE (inp) OF

;						Shift the stars up.

			'U': astr.crpix(1) = astr.crpix(1) + 5*adjust

;						Shift the stars down.

			'D': astr.crpix(1) = astr.crpix(1) - 5*adjust

;						Shift the stars to the left.

			'L': astr.crpix(0) = astr.crpix(0) - 5*adjust

;						Shift the stars to the right.

			'R': astr.crpix(0) = astr.crpix(0) + 5*adjust

;						Shift stars closer together.

			'T': cdelt = cdelt + 0.1 * adjust * cdelt

;						Spread stars further apart.

			'S': cdelt = cdelt - 0.1 * adjust * cdelt

;						Rotate stars counterclockwise.

			'A': rotat = rotat + adjust * 5. / !radeg

;						Rotate stars clockwise.

			'C': rotat = rotat - adjust * 5. / !radeg

;						Change scale to gross.

			'G': BEGIN
				adjust = 1.
				redraw = 0
		                scale = 'Gross scale'
			END
;
;						Change scale to fine.
;
			'F': BEGIN
				adjust = 0.1
				redraw = 0
				scale = 'Fine scale '
			END
;
;						Change scale to extra fine.
;
			'X': BEGIN
				adjust = 0.01
				redraw = 0
				scale = 'Extra Fine '
			END
;
;						Help.
;
			'H': BEGIN
				FOR i = 0, 17 DO print, menu(i)
				redraw = 0
			END
;
			ELSE: redraw = 0
		ENDCASE
;
;					Recompute solution, then display boxes
;					around the stars.  This only need be
;					done if the "redraw" variable is set.
;
		IF (redraw) THEN BEGIN
			astr.cd = CDELT * [ [-cos(rotat),-sin(rotat)], $
				[-sin(rotat),cos(rotat)] ]
;
			if proj EQ 'UIT' then $
				uit_ad2xy, ra, dec, astr, x, y else $
				ad2xy, ra, dec, astr, x, y
			zoom_xy, x, y
			good = where( (x-boxsz GE 0) AND (x+boxsz LT $
				!d.x_vsize) AND (y-boxsz GE 0) and $
				(y+boxsz LT !d.y_vsize), ngood)  
			IF (ngood NE 0) THEN BEGIN
				device, COPY=[0,0,xsz,ysz,0,0,wp]
		        	tvbox, boxsz, x(good), y(good), color
				nbad = nstars - ngood
			ENDIF ELSE nbad = nstars
		ENDIF
;
;					Update the status line.
;
		IF (nbad NE 0) THEN print, FORMAT='($,A,A,10x,A,15x,A,I3,A)', $
		 cr,inp,scale,'WARNING:',nbad,' stars outside screen display' $
		ELSE print, FORMAT='($,A,A,10x,A,A)', cr,inp,scale, blank

;
	ENDREP UNTIL ( inp EQ 'Q' ) 
;
;				Display the current solution.
;
	print, 'New plate scale (Arcseconds/pixel)', 3600 * cdelt
	print, 'New plate center: ', adstring(astr.crval)
	print, 'New rotation angle (degrees)', rotat * !radeg
;
;				Is the user sure about quitting?
;
	read, 'Do you want to improve the solution [N]? ', inp
	inp = strmid(strtrim(strupcase(inp), 2), 0, 1)
ENDREP UNTIL (inp NE 'Y')
;
;			Update the header?
;
read, 'Do you want to add update astrometry to image header [N]? ', inp
inp = strmid(strtrim(strupcase(inp), 2), 0, 1)
IF (inp EQ 'Y') THEN BEGIN
	sxaddhist, 'VIDEO: ' + strmid(!STIME,0,11) + ' ' + strtrim(ngood,2) + $
		' STARS MANUALLY ALIGNED', hdr 
	 if proj EQ 'UIT' then $
		uit_xy2ad, crpix0(0), crpix0(1), astr, crval1, crval2 else $
		    xy2ad, crpix0(0), crpix0(1), astr, crval1, crval2
	astr.crpix = crpix0
	astr.crval = [ crval1, crval2 ]
        putast, hdr, astr
ENDIF
;
;			Delete the pixmap.
;
wdelete, wp
;
RETURN
END
