PURE FUNCTION px_x4y3_nbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cx_nbase(pes_x4y3_nki,pes_x4y3_sysnew,px_pcv)
return
END FUNCTION px_x4y3_nbase
