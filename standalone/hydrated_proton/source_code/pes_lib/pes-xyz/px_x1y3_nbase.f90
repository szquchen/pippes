PURE FUNCTION px_x1y3_nbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cx_nbase(pes_x1y3_nki,pes_x1y3_sysnew,px_pcv)
return
END FUNCTION px_x1y3_nbase
