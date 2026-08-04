PURE FUNCTION px_x3_nbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cx_nbase(pes_x3_nki,pes_x3_sysnew,px_pcv)
return
END FUNCTION px_x3_nbase
