PRO CTVSCL,IMAGE,P1, P2,P3,P4,PIXMAP=PX,MINDATA=mind, MAXDATA = maxd,TOP=top,$
    TITLE=title, log = log, Field = fld, Sigrange = Sigrange,SILENT = Silent, $
    WINSIZE = winsize
;+
; NAME:
;       CTVSCL
; PURPOSE:
;       Load an array to the image display after byte scaling.  It differs from 
;       the IDL procedure TVSCL in the following ways:
;
;       (1)  The default MIN and MAX for the scaling is computed by calculating
;               the sky value and variance of the image
;       (2)  The size and position of the image is stored in the common block
;               IMAGES.
;       (3)  The user is warned if the image array is larger than the screen 
;               size
;       (4)  The window is resized to the size of the image, unless the position
;               parameter is given.
;       (5)  Negative numbers may be used for the X and Y parameters to extract 
;               a subarray of a large image.
;       (6)  The MAXD and MIND keywords can be used to specify the range of the
;               image to be scaled.
;       (7)  An image title can be displayed by either supplying a FITS header
;               or by use of the TITLE keyword.
;       (8)  The /LOG keyword lets one take the LOG of the image before scaling
;
; CALLING SEQUENCE:
;       CTVSCL,IM,PIXMAP=1          ;Scale & display image in center of current channel
;       CTVSCL,IM,X,Y,HEADER
;       CTVSCL,IM,HEADER[,X,Y,FIELD]
;       CTVSCL,IM,HEADER[,POS]
;       CTVSCL,IM,POS,HEADER
;       CTVSCL,IM,X,Y,FIELD, $
;            [/PIXMAP,MAXD =, MIND = , TOP = top, TITLE = ,/LOG, WINSIZE =
;               SIGRANGE =[s1,s2],/SILENT ]
;
;       CTVSCL will accept up to 4 parameters following the IM parameter- up 
;       to 3 numbers and up to one string array.  The string array is assumed 
;       to be a FITS header and can be placed arbitrarily among the number 
;       parameters.  The definition of the numerical parameters is dependent 
;       upon the number of them: 
;
;       1 number        POSITION 
;       2 numbers       X, Y    (in order of appearance in procedure call)
;       3 numbers       X, Y, FIELD. (in order of appearance in procedure call)
;       FIELD may also be specified as a keyword.       
;
; INPUTS:
;       IM - 2-d image array to be loaded to the image window
;
; OPTIONAL INPUT PARAMETERS:
;       X - X position of display, starting from the lower left hand corner.
;       Y - Y position of display, starting from the lower left hand corner.
;               If X or Y are not specified, the current window position is 
;               used.
;               If X and Y are negative, then a subimage is extracted from IM 
;               beginning at position X,Y.    The size of the subimage is 
;               either taken from the WINSIZE keyword, or the maximum size 
;               that can fit on the screen. 
;               Note that the maximum and minimum values of the subimage (not 
;               the original image) are used to scale into a byte array
;       POSITION - Scalar giving position on screen where image will be
;               displayed.  Position 0 is in the upper left had corner and
;               positions run from left to right, and from top to bottom.
;               See documentation for TV for information on POSITION.
;       FIELD - Image channel to be loaded.  If omitted, the current image
;               channel (from the common block TV) is used.
;       HEADER = FITS header for image, from which a summary is extracted and
;               written to the window's title bar.
;
; OPTIONAL KEYWORD INPUTS:
;       PIXMAP - A keyword parameter indicating whether the image should be
;               written to a pixmap instead of a visible window.  If set and 
;               non-zero, the image is written to a pixmap.  The PIXMAP 
;               keyword may be used with any of the four calling sequences.
;       MAXDATA - Maximum value of IM to consider before scaling, scalar
;       MINDATA - Minimum value of IM to consider before scaling, scalar
;       SILENT - If present and non-zero, all print-to-screen commands are 
;               suppressed.
;       SIGRANGE - Two-element vector containing range of scaling (*sigma, with
;               respect to average.) Default is [-0.5,12].  USE OF SIGRANGE 
;               OVER-RIDES MINDATA, MAXDATA.
;       TOP - Maximum value of the scaled result, scalar, default =
;             !D.TABLE_SIZE.   The MAXDATA, MINDATA, and TOP keyword values are
;             passed  directly to the BYTSCL procedure -- see documentation 
;            for BYTSCL
;       TITLE - A scalar string to be displayed as the title to the image
;               window.
;       LOG - If present and non-zero, LOG10 of IMAGE is used.
;       FIELD - acts same as FIELD parameter
;       WINSIZE - 2 element vector specifying the size of the window to contain
;               the image.    Only useful if X,Y are specified as negative
;
; OUTPUTS:
;       None.
;
; EXAMPLE:
;       One has a 2048 x 2048 image array BIGIM, and associated FITS header, H.
;       Display a 512 x 512 subimage starting at pixel (800,800) and include 
;       a title extracted from the header
;
;            IDL> ctvscl,IM,-800,-800,H
;
; COMMON BLOCKS:
;       The current image plane is read from the common block TV.  The lower
;       left hand corner and the image size are stored in the common block
;       IMAGES.
;
; RESTRICTIONS:
;       Program can only store the size and starting position of a single
;       image on a given image frame.
;
; MODIFICATION HISTORY:
;       Adapted to workstations.  M. Greason, May 1990.
;       Added TITLE keyword, J. Isensee, September 9, 1991.
;       Added new default scaling, HEADER parameter, LOG, SIGRANGE keywords  
;                               J. Offenberg Oct, 1991
;       Made more robust scaling, fix position parameter problems WBL May 1992
;       Set default value of TOP to !D.N_COLORS, never scale outside the
;       min and max of the image     WBL     Aug 96
;       New algorithm for taking logarithm   WBL   Aug 96
;       Make sure a window open before using !D.N_TABLE_SIZE   WBL  October 1999
;       For integer images, make sure max > min +1  WBL  January 2000
;       Two element WINSIZE vector, convert to square brackets  WBL January 2001
;-
 On_error,2
 COMMON TV, chan, zoom, xroam, yroam
 COMMON IMAGES, x00, y00, xsize, ysize
;
;                       Check the image array for validity.
;
 npar = N_params()

 IF npar EQ 0 THEN BEGIN                ;Image array supplied?
        print, 'Syntax - CTVSCL, image, [ x, y, field, TITLE =, /PIXMAP' + $
               ', /LOG, FIELD = ]'
        print,'                           MAXDATA =, MINDATA =, TOP = '
        RETURN
 ENDIF

 sz = size(image)               ;Warn user if image isn't two dimensional.
 IF (sz[0] NE 2) THEN $
    message,'Image array (first parameter) is not 2 dimensional'
 dtype = sz[3]
 integer = (dtype LT 4) or (dtype GE 12)
;
;       Check parameters 2,3,4,5 for existance, type and number.  
;       Separate numerical parameters from FITS header.
;

HDR_YN = 0      ;All variables ending in _YN are booleans describing the 
Y_YN = 0        ;types of input parameters given.
FIELD_YN = 0
POS_YN = 0
TRBL_YN = 0
NP = Intarr(4)
Index =0 

IF npar GE 2 then CTVParams, NP,P1, Header,HDR_YN,Index,trbl_yn
IF npar GE 3 then CTVParams, NP,P2, Header,HDR_YN,Index,trbl_yn
IF npar GE 4 then CTVParams, NP,P3, Header,HDR_YN,Index,trbl_yn
IF npar GE 5 then CTVParams, NP,P4, Header,HDR_YN,Index,trbl_yn

CASE Index OF
0:      Begin           ;No plotting paramaters given
        POS_YN = 0 & X_YN = 0 & Y_YN = 0 & FIELD_YN = 0
        X = 0 & Y = 0
        end
1:      Begin           ;POSITION parameter
        POS_YN = 1 & X_YN = 0 & Y_YN = 0 & FIELD_YN = 0
        X = NP[0] & Y = 0
        END
2:      Begin           ;X & Y parameters
        POS_YN = 0 & X_YN = 1 & Y_YN = 1 & FIELD_YN = 0
        X = NP[0] & Y = NP[1]
        END
3:      Begin           ;X, Y & FIELD parameters
        POS_YN = 0 & X_YN = 1 & Y_YN = 1 & FIELD_YN = 1
        X = NP[0] & Y = NP[1] & FIELD = NP[2]
        END
4:      Begin           ;Uh, oh.
        message,"TOO MANY NUMERICAL PARAMETERS: 3 AT MOST!",/inf
        return
        end
else:   begin           ;For debugging purposes.  Should never come up.
        message,"Encountered a problem.  Program quitting.",/inf
        return
        end
endCASE
;       
;                       Check FIELD keyword
IF keyword_set(Fld) then begin
        Field_yn = 1
        field = fld
    endIF
;
 setrange = 0             ;Bit 1 Min Value set,  Bit 2 Max value set
 if N_elements(mind) eq 1 then setrange = setrange + 1
 if N_elements(maxd) eq 1 then setrange = setrange + 2
;
;                       Check the PIXMAP keyword.
;
p = keyword_set(px)
;
;                       If a window index has been supplied, make the
;                       corresponding window the active window.
;
IF not(FIELD_YN) THEN field = chan
chan, field
;
 if not keyword_set(TOP) then top = !D.TABLE_SIZE - 1
;                       Set window title
;
wtitle = 'IDL '+ strtrim(field,2)

IF keyword_set(TITLE) then wtitle = wtitle + '   ' + title $
ELSE IF HDR_YN then wtitle = wtitle + '    ' + Headerstring(Header)

;
;                       Initialize window variables.
;
device, GET_SCREEN_SIZE = screen        ;Get maximum screen size
wi = !d.window
xwsz = !d.x_vsize
ywsz = !d.y_vsize
if not keyword_set(WINSIZE) then $
           winsize = [sz[1]< screen[0],sz[2]<screen[1]] else $
           if N_elements(winsize) EQ 1 then winsize = [winsize ,winsize]
;
;                       Check for a small window / screen.
;
rszflg = 0
IF (sz[1] GT xwsz) OR (sz[2] GT ywsz) THEN BEGIN
    IF ((sz[1] GT screen[0]) OR (sz[2] GT screen[1] )) AND $
        ((x GE 0) AND (y GE 0)) THEN $
           message,'Image array too large for the display screen'
    xwsz = sz[1]
    ywsz = sz[2]
    rszflg = 1
ENDIF
;
;                       If a negative position was specified, extract subimage.
;
    IF (x LT 0) OR (y LT 0) THEN BEGIN
        largeim = 1  & npar = -1
        xst = abs(x) & xfn = (xst + winsize[0]-1) < (sz[1] - 1)
        yst = abs(y) & yfn = (yst + winsize[1]-1) < (sz[2] - 1)
        xwsz = xfn - xst + 1
        ywsz = yfn - yst + 1
        x_yn = 0 & y_yn = 0
        subim = image[xst:xfn,yst:yfn]
    ENDIF ELSE BEGIN
        largeim = 0
        xwsz = xwsz < sz[1]
        ywsz = ywsz < sz[2]
    ENDELSE

;                       Open window, display image.

 device, WINDOW_STATE=opnd, GET_WINDOW_POSITION = Gwpos, GET_SCREEN_SIZE=screen

if not pos_yn then begin
   if (not opnd[chan]) or (!D.X_VSIZE NE xwsz) or (!D.Y_VSIZE NE ywsz) $
       or keyword_set(TITLE) or HDR_YN  then begin

                IF not(X_YN and Y_YN) then BEGIN
                        XZ = Gwpos[0]
                        YZ = Gwpos[1]
                endIF ELSE BEGIN
                        XZ = X
                        YZ = Y  
                endELSE

                IF (XZ + xwsz) GT screen[0] then xz = screen[0] - xwsz
                IF (YZ + ywsz) GT screen[1] then yz = screen[1] - ywsz
                xz = xz > 0
                YZ = yz > 0

        window, chan, xsize=xwsz, ysize=ywsz, xpos=xz, ypos=yz, $
                pixmap=p, title = wtitle
    endif
endif
CASE setrange OF        ;Fill in missing scaling parameters
    3: begin    ;Do nothing.  Everything has been chosen
        end 
    2: mind = min(image)
    1: maxd = max(image)
    0: begin
            iF LARGEIM then $
                    maxd = max(subim,MIN=mind) else $
                    maxd = max(image,MIN=mind) 
            if not keyword_set(log) then begin
            if not keyword_set(SIGRANGE) then sigrange = [-.25,12]
            if LARGEIM then sky, subim, avg, sigma, /SILENT else $
                            SKY, Image, Avg, Sigma,/Silent
            if sigma GT 0. then begin
                mind = (avg + Sigrange[0]*sigma) > mind
                maxd = (avg + Sigrange[1]*sigma) < maxd
            endif else begin 
               iF LARGEIM then $
                    maxd = max(subim,MIN=mind) else $
                    maxd = max(image,MIN=mind)
             endelse 
        endif
        if integer then maxd = maxd > (fix(mind) +1)
      end
    ENDCASE


IF NOT(Keyword_set(SILENT)) and not keyword_set(LOG) THEN $     
 message,"Image scaled between "+strn(mind)+" and "+strn(maxd),/INF

IF keyword_set(LOG) then BEGIN
        if not keyword_set(SILENT) then message,"Taking LOG[10] of IMAGE",/inf
if LARGEIM then $ 
        q = alog10( image[xst:xfn,yst:yfn] - (mind-1)*(mind LE 0) ) else $
        q = alog10(image - (mind-1)*(mind LE 0) ) 
        mind = min(q,max=maxd)
   endIF
;

if not pos_yn then begin
iF LARGEIM then begin

        if keyword_set(LOG) then $
           tv,bytscl(q,MIN = mind, MAX=maxd,TOP = top) $
        else $
           tv,bytscl(image[xst:xfn,yst:yfn],min = mind,max=maxd,top=top)
       
endif else begin 

        if keyword_set(LOG) then $
           tv, bytscl(q,MIN = mind, MAX = maxd, TOP = top)      $
        else $
           tv, bytscl(image,MIN = mind, MAX = maxd, TOP =top)    

endelse
endif else begin 
iF LARGEIM then begin

        if keyword_set(LOG) then $
         tv,bytscl(q,MIN = mind, MAX=maxd,TOP = top),x $
        else $
           tv,bytscl(image[xst:xfn,yst:yfn],min = mind,max=maxd,top=top),x
       
endif else begin 

        if keyword_set(LOG) then $
           tv, bytscl(q, MIN = mind, MAX = maxd, TOP = top),x   $
        else $
           tv, bytscl(image,MIN = mind, MAX = maxd, TOP =top),x
endelse
endelse
;                       Fill common block variables.
;
    x00[chan] = x < 0
    y00[chan] = y < 0
    xsize[chan] = sz[1]
    ysize[chan] = sz[2]
    zoom[chan] = 1
;
return
end
