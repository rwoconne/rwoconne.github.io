pro tvdbase,hdr,catalogue,list,BOXWIDTH = boxwidth,SUBLIST=sublist, $
                  _EXTRA = _extra
;+
; NAME
;	TVDBASE
; PURPOSE:
;	Draw boxes in the around the sources found in specified database on 
;	the image in the active window.
;
; CALLING SEQUENCE:
;	tvdbase, hdr,[ catalogue, list, BOXWITH =, SUBLIST = , /NOMATCH]
;
; INPUTS:
;	hdr - FITS image header corresponding to the currently displayed image
;	catalogue - string giving name of catalogue in database
;		Database must contain the (preferably indexed)
;		fields RA (in hours) and DEC.   If not supplied then the
;		currently opened database is used.
;
; OPTIONAL OUTPUT:
;	list -  vector containing the entry numbers of the sources found
;		in the image display
;
; OPTIONAL INPUT KEYWORDS:
;	BOXWIDTH - set equal to the width of the overlay boxes.   If not
;		supplied, then the default box width is 9.
;	SUBLIST -  vector containing entry numbers of the database to be
;		considered.   If not supplied, or set equal to -1, then all 
;		entries are considered.
;
;		Any keywords accepted by PLOTS (e.g. COLOR, THICK, LINESTYLE)
;		are accepted by TVDBASE through the _EXTRA facility.   These
;		keywords describe the drawn boxes 
; COMMON BLOCKS:
;	The common block IMAGES must be previously defined (by CTV or CTVSCL).
;
; RESTRICTIONS:
;	Because X windows does not have a graphics overlay, the drawn boxes will
;	erase the portion of the image beneath them.   
;
; EXAMPLE:
;	Overlay the positions of all ST guidestars brighter that V = 12 on the 
;	current image.  The image is assumed to have an associated header, H.
;
;	SUBLIST = DBFIND('VMAG < 12')		;Restrict list to stars 
;						;brighter than V=12
;	TVDBASE,H,'GUIDESTAR',SUB=SUBLIST	;Overlay positions of the 
;						;guidestars
;
; PROCEDURES CALLED
;	ZPARCHECK,UNZOOM_XY,ZOOM_XY,IMDBASE, TVBOX
; REVISION HISTORY:
;	Written W. Landsman            September, 1988
;	Adapted for IDL Version 2 & IVAS, J. Isensee, June, 1990
;	Added SUBLIST keyword     W. Landsman      September, 1990
;	Added NOMATCH keyword.    M. Greason       August 1992.
;	Add COLOR, THICK keywords    W. Landsman    August 1995
;	Use _EXTRA facility    W. Landsman          November 1995
;-
On_error,2                         ;Return to caller

 if N_params() EQ 0 then begin      ;Sufficient parameters?
     print,'Syntax - tvdbase,hdr,[catalogue, SUBLIST=, BOXWIDTH=, _EXTRA = ]'
     return
 endif

IF (keyword_set(nomt)) THEN nomt = 0 ELSE nomt = 1

 common tv,chan,zoom,xroam,yroam
 common images,x00,y00,xsize,ysize

zparcheck,'TVDBASE',hdr,1,7,1,'FITS image header'

if !D.WINDOW EQ -1 then message,        $
      'ERROR - No ' +!D.NAME + ' image windows are currently open'

 if not keyword_set(BOXWIDTH) then boxwidth = 9
 if N_elements(catalogue) EQ 0 then begin
        if db_info('open') EQ 0 then message, $
	'ERROR - No database has been opened'
	catalogue = db_info('NAME',0)
 endif

 if N_elements(x00) EQ 0 then begin
     message,'WARNING - No previous image loaded with CTV',/CONTINUE
     x00 = intarr(128) & y00 = x00 
     xsize = x00 & ysize = y00
     xsize(!D.WINDOW) = !D.X_VSIZE & ysize(!D.WINDOW) = !D.Y_VSIZE
 endif
   
 if xsize(chan) EQ 0 then xsize(chan) = !D.X_VSIZE
 if ysize(chan) EQ 0 then ysize(chan) = !D.Y_VSIZE

 if N_elements(sublist) GE 1 then $
        message,'Restricting source search to user input sublist',/INF $
        else sublist = -1

; Get RA and Dec of 4 corners of the TV

 x = [x00(chan) > 0,xsize(chan)-1 < (!D.X_VSIZE-1)]
 y = [y00(chan) > 0,ysize(chan)-1 < (!D.Y_VSIZE-1)]
 unzoom_xy,x,y

 imdbase,hdr,catalogue,list,XRANGE = x, YRANGE = y, XPOS = xpos, YPOS = ypos, $
                           SUBlIST = sublist

 nfound = N_elements(xpos)
   if nfound GT 0 then begin                   ;Draw boxes around stars
              zoom_xy,xpos,ypos                ;Convert to zoomed coords
              tvbox, boxwidth, xpos, ypos, _EXTRA = _extra
   endif else message,'No '+strtrim(strupcase(catalogue),2) + $
              ' sources found in currently displayed image', /CON
 return
 end
