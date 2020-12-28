Pro  bio,r,p0,pop

;+ ---------------------------------------------------------------
;
;  BIO is an IDL demonstration program which calculates the
;     population of a sample "DETERMINISTIC, NONPERIODIC" CHAOTIC
;     BIOLOGICAL SYSTEM.
;
;  The user is invited to run BIO for various values of the 
;     input parameters r and p0, looking for those which yield
;     chaotic behavior.
;
;  The function used here is p(n+1) = r*p(n)*(1-p(n)).  p(n) is the
;  population of the nth generation in the system.  This is a famous
;  example of a function which, for the correct selection of r and p(0),
;  can produce "deterministic, nonperiodic" behavior.  
;
;  INPUT:
;
;  To execute the routine for parameters r and p(0) = p0, type:
;
;                        bio,r,p0,pop
;
;  RECOMMENDED INPUT PARAMETERS:  
;
;  Try various r,p0 combinations, but keep p0 positive and less than 1.0.
;  A good starting point is r =2.9, p0 = 0.05.  
;  A combination yielding chaotic behavior is r = 4, p0 = 0.05.  
;
;     Explore the rapid changes in the function near these values by
;     using the recall buffer ("up" arrow) and editing the command line.
;
;  [The program terminates the computation if the pop vector goes negative.]
;
;  OUTPUT:
;
;  BIO automatically produces two plots: the population versus generation
;     number, and the power spectrum of the population
;
;  The [optional] output vector POP, which shows the population of the
;  biological system, has 500 elements.  It is plotted on the
;  terminal along with its power spectrum calculated with a fast
;  Fourier transform function. A sharp spike in the power spectrum
;  indicates that the POP function has strongly periodic behavior,
;  whereas a broad, irregular power spectrum is found for non-periodic,
;  chaotic behavior.
;
;  You can experiment with various IDL plotting modes using the POP
;  output vector of BIO.  If you wish to see only part of
;  the pop function, type "plot,pop(0:50)", for example, to plot only
;  the first 51 elements.  You can store the result of the calculation
;  in any variable you wish: e.g. "bio,r,p0,snarf" creates the output
;  array "snarf".  It is therefore easy to compare results for different
;  input parameters.
;
;  A vector containing the power spectrum can be obtained by typing
;  "f = abs(fft(pop,-1))".  
;
;  If you wish to see the coding of the subroutine,  type ".run -t bio"
;
;  HISTORY:  Created 28 FEB 90, RWO
;            5/31/00:  Text consolidated; doc revised
;            6/26/00:  Call sequence change, other small mods. RWO
;            7/12/00:  Stop computation when pop goes negative
;             8/1/01:  Doc update
;             8/8/01:  Doc update
;-------------------------------------------------------------


if (n_params(0) eq 0) then begin
     print,'   TO RUN BIO, type: bio,r,p0,pop'
     print,'   where r is ~ 1-4 and 0 < p0 < 1.0'
     return
  endif
	
pop=fltarr(500)  ; Create a 500-element array to hold result
                 ; Initial value is zero

pop(0)=p0        ; First element of result is parameter p0

for i = 1,499 do begin   ; Fill remainder of answer array

    s = pop(i-1)

    if ((1-s) le 0) then goto,next

    pop(i) = r*s*(1-s)

  endfor

next: 

; Set up general plotting parameters

!fancy=1
!ignore=1
!type=16

; Plot pop versus generation in upper window

set_viewport,0.15,0.9,0.59,0.95  ; Define upper window

!mtitle='POPULATION HISTORY'
!xtitle='Generation'
!ytitle='Population Size'

plot,pop    ; Plot the result

; Plot power spectrum of population history in lower window

!noeras=1

set_viewport,0.15,0.9,0.1,0.46  ; Define lower window

!mtitle='POWER SPECTRUM'
!xtitle='Frequency'
!ytitle='Amplitude'

plot_io,abs(fft(pop,-1))  ; Plot power spectrum in semilog form

; Restore plotting parameters

set_viewport,0.1,0.95,0.1,0.95 

!noeras=0
!mtitle=' '
!xtitle=' '
!ytitle=' '

 return

end

