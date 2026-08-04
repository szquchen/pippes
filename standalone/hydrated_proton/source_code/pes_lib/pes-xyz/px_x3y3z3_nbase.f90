PURE FUNCTION px_x3y3z3_nbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cx_nbase(pes_x3y3z3_nki,pes_x3y3z3_sysnew,px_pcv)
return
END FUNCTION px_x3y3z3_nbase
