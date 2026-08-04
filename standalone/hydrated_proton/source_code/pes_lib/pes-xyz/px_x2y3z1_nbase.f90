PURE FUNCTION px_x2y3z1_nbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cx_nbase(pes_x2y3z1_nki,pes_x2y3z1_sysnew,px_pcv)
return
END FUNCTION px_x2y3z1_nbase
