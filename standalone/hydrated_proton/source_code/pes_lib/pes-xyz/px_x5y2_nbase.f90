PURE FUNCTION px_x5y2_nbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cx_nbase(pes_x5y2_nki,pes_x5y2_sysnew,px_pcv)
return
END FUNCTION px_x5y2_nbase
