Function xpwr,x,pwr=pwr

;+ INTEGRAND ROUTINE for simple power law
;
;  Intended for use in conjunction with ASTUSELIB
;    version of the QSIMP integration routine.
;    That routine provides for keyword "inheritance" 
;    so that additional keywords used in call of QSIMP 
;    are passed through to the integrand function to
;    provide a larger range of integrand options.  
;
;  KEYWORD PWR: the power law index; default = 1
;
;  INPUT: x can be a scalar or vector
;
;  OUTPUT: x^pwr
;
;  History:  9/21/03: created RWO
;----------------------------------------------

if  (n_elements(pwr) ne 1) then pwr = 1.0

x=float(x)   ;; Ensures floating point output

return,x^pwr

end
