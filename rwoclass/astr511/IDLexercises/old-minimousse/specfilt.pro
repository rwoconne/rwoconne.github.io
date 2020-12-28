function specfilt,wave,flux,filter,weff, SDASfile = sdasfile, FITSfile= fitsfile
;+
; NAME
;	SPECFILT
; PURPOSE:
;	Convolve a spectrum through a filter to obtain a flux.  Optionally
;	return the effective wavelength of the flux distribution.
;
; CALLING SEQUENCE:
;	result = SPECFILT( wave, flux, [ filter, weff ])  
;
; INPUTS:
;	WAVE - wavelength vector
;	FLUX - flux vector, same number of elements as W. 
;
; OPTIONAL INPUT:
;	FILTER - 2 character string giving the name of filter to be used.
;		 'JU','JB','JV','JR'  -- Johnson U,B,V,R filters
;		 'KB','KV','KR','KI' -- Kitt Peak U,B,V,R,I filters
;		 'KH', 'KK', 'KL', $
;
;               SPECFILT will prompt for this parameter if not supplied. 
;
; OUTPUT:
;	RESULT - scalar giving intensity in same units as FLUX convolved
;		through specified filter  
;
; OPTIONAL OUTPUT:
;	WEFF - scalar giving the effective wavelength of the flux distribution
;		through the specified filter
;
; OPTIONAL INPUTS:
;	SDASFILE - scalar string give the name of the STSDAS table with a 
;		filter response curve.    The first column in the table must
;		be a wavelength scale, and the second column must be the
;		relative filter response.
;	FITSFILE - scalar string giving the name of the FITS ASCII table with
;		with a filter response curve.   The first column in the table
;		must be a wavelength scale, and the second column must be the
;		relative filter response.
;
;       If either FITSFILE or SDASFILE is specified, then the FILTER parameter
;	is ignored.
;
; FILES USED:
;	SPECFILT reads the files CSTECAL.DAT and CSICAL.DAT on the logical
;	directory UIT_DATA: or FITS ASCII tables storing additional filter info.
;
; EXAMPLE:
;	Find the ratio of the UV flux of a 20000K star in the UIT broadband
;	filters (A1 and B1) and Johnson V
;
;	KURUCZ,W,F,20000,4.0,0	;Get W and F vectors for a Kurucz model of a 
;					;20000K, log(g)=4.0 star
;	NUV = SPECFILT(W,F,'A1') ;Relative intensity in the near UV
;	FUV = SPECFILT(W,F,'B1') ;Relative intensity in the far UV.
;         V = SPECFILT(W,F,'JV') ;Relative intensity in Johnson V band.
;           to find that    NUV/FUV  =  1.084
;               and that    NUV/V    =  8.167
;
; PROCEDURES CALLED:
;	FIND_WITH_DEF(), FTGET(), READFITS(), REMCHAR, TABLE_EXT, TSUM()
; REVISION HISTORY:
;	Written    W. Landsman    October 1988
;              added additional filters February 1996 E. P. Smith NASA/GSFC
;	KPNO filter tracings from KPNO library of Harris broad band filters
;	Use FIND_WITH_DEF rather than OSFCNVRT()    November 1997
;	Use a finer wavelength grid, W. Landsman  November 1997
;-
; On_error,2

 if N_params() LT 2 then begin
      print,'Syntax - result = SPECFILT( wave, flux, [ filter, weff ])'
      return, -1
 endif

 name = ['A1', 'A2', 'A3', 'A4', 'A5', 'B1', 'B2', 'B3', 'B4', 'B5', 'B6',$
	'Johnson U (JU)', 'JB', 'JV', 'JR', 'JI', $
	'KPNO U (KU)', 'KB', 'KV', 'KR', 'KI', $
	'KPNO IR (KJ)', 'KH', 'KK', 'KL', $
	'Stromgren u (Su)', 'Sb', 'Sv', 'Sy']
 peakwl = [ 2763, 1853, 1899, 2184, 2508, 1443, 1266, 1388, 1523, 1518, 1477,$
	3750, 4275, 5300, 7000, 8600, $
	3750, 4331, 5397, 6424, 8244, $
	12670, 16720, 22240, 38010, $
	3500, 4700, 4100, 5500 ]
 bandps = [ 1147, 412, 173, 244, 456, 354, 160, 256, 129, 225, 404, $
	 550, 1000,  850, 2100, 2200, $
	1045, 1045, 1059, 1540, 1954, $
	2710, 2740, 3940, 6020, $
	340, 160, 200, 240]


 if N_params() LT 3 then readfilter = 1b else readfilter = 0b
 if keyword_set(FITSfile) or keyword_set(SDASfile) then readfilter = 0b

 GETFILTER: if readfilter then begin
    filter = ''
    print,'The following UIT filters are available'
    print,'   Filter            Peak WL (A)   Bandpass (A)'
    for i = 0,n_elements(name)-1 do print,$
		 f ='(A16,6X,I6,8X,I6)',name(i),peakwl(i),bandps(i)
    print,' '        
    read,'Enter 2 character ID of desired of desired filter: ',filter
 endif

 if keyword_set(FITSfile) then begin
	 table = readfits(FITSfile, hdr, /EXTEN)
	 w = ftget(hdr, table, 1)
	 frel = ftget(hdr, table, 2)

 endif else if keyword_set(SDASfile) then begin

	table_ext,sdasfile,[1,2],w,frel

 endif else begin

 remchar,filter,' '
 filt_type = strupcase( strmid( filter,0,1 ) )
 UIT = (filt_type EQ 'A') or (filt_type EQ 'B')
 filt = strupcase( strmid(filter,1,1) )
 if UIT then begin 
   filnum = fix(filt)
   if ( filnum GT 6) or ( filnum LT 1 ) then begin
        message,'ERROR - Unknown UIT filter '+strupcase(filter), /CON
        readfilter = 1b
        goto, GETFILTER 
   endif
 endif

 case filt_type of 
 'A':  begin                      ;Cesium Teluride Filter (A)
      openr, lun, find_with_def( 'cstecal.dat','UIT_DATA' ), /GET_LUN   
      a = assoc( lun, fltarr(3676))
      k = 2450.                      ;Normalization (Wavelength range)
      end

'B':  begin                      ;Cesium Iodide Filter (B)
      openr, lun, find_with_def( 'csical.dat','UIT_DATA' ),/GET_LUN
      a = assoc( lun, fltarr(2400) )
      k = 1599.33                    ;Normalization (Wavelength range)
      end
;
;   Johnson filter selected
;
'J':  begin
	  case filt of
	  'U': fname = 'johnsonU'
          'B': fname = 'johnsonB'
          'V': fname = 'johnsonV'
          'R': fname = 'johnsonR'
          'I': fname = 'johnsonI'
       ELSE: begin
		    message,'ERROR - Unknown Johnson Filter '+ filt,/CON
			readfilter = 1b
			goto, GETFILTER
			end
       endcase
	   end
;
; KPNO filter selected  
;   Note: divide all frel by 100 because the kpno filters are 
;         stored as percentages and not fractions.
;
 'K':  begin
	   case filt of
	   'U': fname = 'kpnoU'
           'B': fname = 'kpnoB'
	   'V': fname = 'kpnoV'
	   'R': fname = 'kpnoR'
	   'I': fname = 'kpnoI'
	   'J': fname = 'kpnoJ'
	   'H': fname = 'kpnoH'
	   'K': fname = 'kpnoK'
	   'L': fname = 'kpnoL'
	   ELSE: begin
			 message,'ERROR - Unknown KPNO Filter '+ filt,/CON
			 readfilter = 1b
			 goto, GETFILTER
			 end
       endcase
	   end
;
;  Stromgren filters selected
;	   
'S':  begin
	   case filt of
	   'U': fname = 'stromgrenu'
           'V': fname = 'stromgrenv'
	   'B': fname = 'stromgrenb'
	   'Y': fname = 'stromgreny'
	   ELSE: begin
	           message,'ERROR - Unknown Stromgren Filter '+ filt,/CON
	           readfilter = 1b
		   goto, GETFILTER
		 end
       endcase
	   end

ELSE: begin
      message,'ERROR - Unknown Filter ' + filter,/CON
      readfilter = 1b
      goto, GETFILTER   
      end

 endcase
  

 if UIT then begin
    w = a(0)                   ;Read in filter wavelength grid
    frel = a( filnum )         ;Get relative intensity for specified filter
    free_lun, lun              ;Done with file
 endif else begin 
	fitsname = FIND_WITH_DEF(fname + '.fits','UIT_DATA')
	 table = READFITS( fitsname, hdr, /EXTEN,/Silent)
	 w = FTGET(hdr, table, 1)
	 frel = FTGET(hdr, table, 2)
	 if filt_type EQ 'K'  then frel = frel/100.
 endelse
 endelse

; Make a wavelength grid that (1) has the same range as the filter response 
; curve, and (2) within that range includes all wavelength values of both the 
; filter response curve and input spectrum.    That way we ensure that we take
; into account any narrow features in either the input spectrum or the filter
; response curve

 wmin = min(w, max = wmax)
 g = where((wave GE wmin) and (wave LE wmax), Ng)
 if Ng EQ 0 then begin
	message,/CON, $
	'ERROR - Supplied wavelengths outside of filter response curve'
	return,-1
 endif

 wgrid = [w,wave(g)]          

 wgrid = wgrid(sort(wgrid))
 wgrid = wgrid(uniq(wgrid))

 linterp,wave,flux,wgrid,fluxg
 linterp,w,frel,wgrid,frelg
  feff = fluxg*frelg              ;Effective flux through filter

 if ( N_params() EQ 4 ) then $     ;Get effective wavelength if requested
       weff = tsum(wgrid*feff)/tsum(feff)

 return,tsum(wgrid,feff)/tsum(wgrid,frelg) ;Integrate flux*(relative intensity)

 end
