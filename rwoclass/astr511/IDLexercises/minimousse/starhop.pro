pro starhop,im,hdr,ra,dec,x,y,r2,d2,fwhm,field1=fld1,field2=fld2, $
                                    mindata = mindata,maxdata = maxdata, $
                                    log = log, sigrange=sigrange
;+
; NAME:
;  STARHOP
; PURPOSE:
;  Interactively compute centroided x and y values for stars 
;  with known right ascension and declination.   A compressed
;  version of images larger than the screen size should be loaded
;  in image plane 0, before calling STARHOP.  
;
; CALLING SEQUENCE:
;  starhop, im, hdr, ra, dec, x, y, r2, d2, [ fwhm, FIELD1= ,FIELD2=   
;                       MINDATA = , MAXDATA = , /LOG ]
;
; INPUTS:
;  im   -   full data array
;  hdr   -  FITS header for full data array.  If header does not already
;     contain aproximate astrometry parameters, then user
;     will be prompted for plate center, scale and rotation.
;  ra     - array of right ascensions, in decimal degrees
;  dec    - array of declinations, in decimal degrees
;
; OPTIONAL INPUTS:
;  fwhm   - size in pixels of area for centroiding
;  field1 - keyword containing the window index containing the compressed 
;     image.  Defaults to 0.
;  field2 - keyword containing the window index to use for subimage 
;  display.   Defaults to 1.  May NOT be zero -- if set to zero, it will 
;     be reset to 1.
;
; OUTPUTS:
;  x      - x value on total image
;  y      - y value on total image
;  r2     - ra in decimal degrees of id'd stars
;  d2     - dec in decimal degrees of id'd stars
;
; OPTIONAL KEYWORD INPUTS:
;  LOG     - pass on to CTVSCL
;  MINDATA - scalar giving minimum pixel value to use when displaying a
;     subimage
;  MAXDATA - scalar giving maximum pixel value to use to display subimage
;  SIGRANGE - pass on to CTVSCL
;
; RESTRICTIONS:
;  The cursor type and graphics color should be set before calling
;  STARHOP.  A programmable cursor type is recommended.
;
; REVISON HISTORY:
;  written by B. Pfarr, STX,  5/6/87
;  star-finding loop slightly mod'd B. Cornett, W. Landsman STX, 1/28/88  
;  updated to IDL V2 and workstations.  M. Greason, STX, 5 June 1990.
;  allow user to center cursor if not centroided, J. Isensee, Oct.,1990
;  Added MAXDATA,MINDATA keywords, fixed some alignments W. Landsman Aug 1991
;  Fixed full image screen coordinates.  M. Greason, Hughes STX, Mar 1992
;  LOG keyword added.  M. Greason, HSTX, 14 July 1992.
;  Updated to use ASTROMETRY structures.  J. Offenberg, HSTX, Jan 1993
;  Fixed $ formats.  RSH, HSTX, 14 Apr 95
;  More improvements in $ formats.  RSH, HSTX, 20 Apr 95.
;  Modified to work with the Astro2 distortion model.  MRG, HSTX, August 1995.
;  Sigrange added, /log is same as with ctvscl.  RSH, HSTX, 19 Apr 1996
;-
 On_error,2                          ;Return to caller
 common tv,chan,zoom,xroam,yroam
 common images,x00,y00,xsize,ysize
;
;        Check parameters.
;
 npar = N_params()
 IF (npar LT 8) THEN BEGIN ; Check calling sequence
   print,'Syntax- starhop, im, hdr, ra, dec, x, y, r2, d2, [fwhm]'
   print,'               [MinData = , MaxData = , /log, Field1 = , Field2 = ] 
   RETURN
 ENDIF
 cr = string("15b)
 check_fits, im, hdr, dimen
 if N_elements(dimen) LT 2 then message, $
            'ERROR - Input image must be 2-dimensional'
 naxis1 = fix( dimen(0) )
 naxis2 = fix( dimen(1) )
 IF (npar LT 9) THEN BEGIN ; Check Full-width Half-max.
   inp = ''
   read,'Enter approximate value for FWHM in pixels [10]: ',inp
   IF inp EQ '' THEN fwhm = 10.0 ELSE fwhm = float(inp)
 ENDIF

 IF NOT keyword_set(fld1) THEN ch1 = 0 ELSE ch1 = fld1   ; Check window indices
 IF NOT keyword_set(fld2) THEN ch2 = 1 ELSE ch2 = fld2
 log = keyword_set(log)
;
 wshow,!D.window              ;Bring the active window to the foreground
;
;        Convert the FWHM into sizes.
;
len = fix(0.637*FWHM) > 2
boxsz = 2*len +1
;
;        Save image display info. for later restoration.
;
 sv_window = chan
;
;        Get astrometry and convert the input RA's and DEC's
;        into pixel positions.
;
 extast, hdr, astr, noparams
 IF noparams LT 0 THEN mkhdr,hdr,im
 if idastrom(hdr) EQ 'UIT' then $
 UIT_ad2xy, ra, dec, astr, xi, yi else $
     ad2xy, ra, dec, astr, xi, yi
;
;        Determine those stars which lie fully within the image.
;
 id = where( (xi GE len) AND (xi LT naxis1-len) AND $  ;Select those stars
              (yi GE len) AND (yi LT naxis2-len),ngood)     ;within image field
 if ngood eq 0 then begin 
     message, 'ERROR - No star positions are within the current image',/CON
     message,'Be sure RA vector is in DEGREES, and in the header equinox',/INF
     return
 endif    
 xi = xi(id) & yi = yi(id)
 index = intarr(ngood)
 x = fltarr(ngood) & y = x
;
;
;        Menu / input cases
;
 scr_erase, 5           ; Erase screen.
 print,'You will be asked to centroid or delete '+strtrim(ngood,2)+ ' stars'
 menu = $
 ['        To:                       Press', $
 '                                       ', $     
 '    Adjust contrast                 A  ', $     
 '    Load a color table              T  ', $
 '    Centroid this star              C  ', $   
 '    Drop this star from solution    D  ', $     
 '    List pixels in the subimage     L  ', $
 '    Look at the Whole image         W  ', $
 '    Look at the Subimage            S  ', $
 '                                       ', $     
 '    Specify a image radius          R  ', $     
 '                                       ', $     
 '    Quit STARHOP, (keeping stars)   Q  ', $     
 '                                       ', $     
 '    Get this Help menu              H  ', $
 '                                       ']
 nmu = n_elements(menu) - 1
 FOR j = 0, nmu DO print, menu(j)
;
;        Process stars.
;
;        Initialize.
;
 halfsub = 60           ; Subimage size over 2 (before scaling)
 zscl = 2            ; Zoom scale factor
 rad = 8192          ; Initial image radius (effect. inf.)
 xhalf = float(naxis1 / 2) & yhalf = float(naxis2 / 2)
 sv_quiet = !quiet & !quiet = 1 
 nided = 0           ; Number of centroided stars
 chan, ch1
 xsz = !d.x_size & ysz = !d.y_size
 window, 9, /free, /pixmap, retain=2, xsize=xsz, ysize=ysz
 device, copy=[0, 0, xsz, ysz, 0, 0, ch1]
 wp = !d.window
 chan, ch1
 tvcrs,1
 IF (log) THEN q = alog10(indgen(32767) > 1)
;
;              Compute the window coordinate for the stars, with
;        respect to the low-resolution image.
;
 xtv = float(xi) * float(!d.x_vsize) / float(naxis1)  ; Display compression.
 ytv = float(yi) * float(!d.y_vsize) / float(naxis2)
 zoom_xy, xtv, ytv                                 ; Zoom and roam.
 xtv = nint(xtv)                    ; Convert to integer.
 ytv = nint(ytv)
;
;        Loop through the stars.
;
 chan, ch2
 FOR i = 0, ngood-1 DO BEGIN
;
;           Check position against the image radius.
;
   r = fix(sqrt(((float(xi(i)) - xhalf) ^ 2) + $
      ((float(yi(i)) - yhalf) ^ 2)))
   IF r GT rad THEN goto, radjmp
;
;           Redisplay the large image
;
   print, format='($,A)',cr+'Thinking...     '
   chan, ch1
   device, copy=[0, 0, xsz, ysz, 0, 0, wp]
;
;           Display portion of large image
;
   chan, ch2
   x1 = fix( (xi(i) - halfsub) > 0)
   x2 = fix( (xi(i) + halfsub - 1) < (naxis1 - 1) )
   y1 = fix( (yi(i) - halfsub) > 0 )
   y2 = fix( (yi(i) + halfsub - 1) < (naxis2 - 1) )
        im_max = max( im(x1:x2,y1:y2), min = im_min)
        minset = 0 & maxset = 0
        if n_elements(mindata) gt 0 then begin
           minset = 1
           im_min = im_min > mindata
        endif
        if n_elements(maxdata) gt 0 then begin
           maxset = 1
           im_max = im_max < maxdata
        endif
   subim = rebin(im(x1:x2,y1:y2),(x2-x1+1)*zscl,(y2-y1+1)*zscl )
   IF (log) THEN BEGIN
      ctvscl, subim, /log
   ENDIF ELSE IF (n_elements(sigrange) eq 2) THEN BEGIN
      ctvscl, subim, sigrange=sigrange
   ENDIF ELSE IF minset OR maxset THEN BEGIN 
      ctvscl, subim, max = im_max, min = im_min
   ENDIF ELSE BEGIN
      ctvscl, subim
   ENDELSE
        zoom(ch2) = zscl  & x00(ch2) = -x1 & y00(ch2) = -y1
;
;           Draw box around star
;
        xsub = 0.5 + (xi(i)-x1)*zscl    
        ysub = 0.5 + (yi(i)-y1)*zscl
        tvbox,(len*boxsz),xsub,ysub,180
   xyouts, /device, xsub, ysub + boxsz+ 4, $
      strtrim(id(i),2), size=2, color=180
   chan, ch1
   tvbox, boxsz, xtv(i), ytv(i)
   print,form='($,20x,A)',adstring([ra(id(i)),dec(id(i))])+cr+' '
;
;           Get and implement command.
;
   REPEAT BEGIN 
      print,form='($,A)',cr+'<<Your choice>>'
      inp = strupcase( get_kbrd(1) )
      print, form='($,A)',cr+inp
      CASE inp OF
;
         'A': contrast   ;Contrast image
;
         'T': loadct     ;Load a color table
;
         'C': BEGIN      ;Centroid star image
;
;                 Get the position.
;
                          if !D.WINDOW NE ch2 then chan,ch2
           print,form = '($,A,A25)', cr $
               + 'Put cursor on star in box; ' $
               + 'then press any mouse button', ' '
           cursor, xdum, ydum, 1, /device
                          unzoom_xy,xdum,ydum,xnew,ynew
;
;                 Centroid using big image.
;
            cntrd, im, xnew, ynew, xnew2, ynew2, fwhm
;
;                 If centroid is no good, use
;                 the cursor position.
;
          IF xnew2 eq -1 THEN BEGIN
                          print,form= '($,49X,A)',$
                             string(7B)+'CNTRD: No convergence  '
                          print,' '
                          print,'Please move cursor to center of star'
                          print,'When centered push any mouse button.'
                          print,' '
                          print,'Cursor coordinates will be used'
                          print,' '
                          cursor,xdum,ydum,1,/device
                          xnew2 = (xdum/zscl) + x1
                          ynew2 = (ydum/zscl) + y1
          ENDIF
;
;                 Add star to 'GOOD' list.
;
            index(nided)= id(i)
            x(nided) = xnew2
            y(nided) = ynew2
            nided = nided + 1
              END     
;
;              Drop star.
;
         'D': print,form='($,A)',cr+'Dropped'
;
;              Print Help menu in scroll region.
;
         'H': for j = 0, nmu do print, menu(j)
;
         'L': tvlist, subim    ;List pixels in the subimage
;
;              Quit.
;
         'Q': BEGIN
            print,format='($,39X,A30)', cr+' '
            read, cr+'Are you sure you want to quit now [Y]? ',inp
            IF (strmid(strupcase(inp),0,1) NE 'N') THEN goto,done
              END
;
;              Look at the subimage.
;
         'S': BEGIN
                chan, ch2
                tvcrs,xsub,ysub
              END
;
;              Look at the whole image.
;
         'W': BEGIN
            chan, ch1
            tvcrs, xtv(i), ytv(i)
              END
;
;              Change the image radius.
;
         'R': BEGIN
            print, "    "
            print, "The image radius currently = ", rad
            read, "Enter the new one: ", rad
            print, "    "
              END
         ELSE: print,form='($,55x,A)',$
            string(7b)+'INVALID OPTION: '+strtrim(inp,2)
      ENDCASE     
   ENDREP UNTIL ( (inp EQ 'D') OR (inp EQ 'C'))
   RADJMP:
ENDFOR
;
;        Restore saved session parameters and the screen, and
;        pretty up the output arrays.
;
 done:              
 !quiet = sv_quiet
 chan, sv_window
 cdel, ch2
 wdelete, wp
 IF NOT !quiet THEN print,'STARHOP: ',strtrim(nided,2),' stars centroided'
 IF nided GT 0 THEN BEGIN
   index = index(0:nided-1)
   r2 = ra(index) & d2 = dec(index)
   x = x(0:nided-1) & y = y(0:nided-1)
 ENDIF
;
 RETURN
 END
