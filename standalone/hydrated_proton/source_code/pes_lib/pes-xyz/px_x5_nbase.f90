PURE FUNCTION px_x5_nbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cx_nbase(pes_x5_nki,pes_x5_sysnew,px_pcv)
return
END FUNCTION px_x5_nbase
