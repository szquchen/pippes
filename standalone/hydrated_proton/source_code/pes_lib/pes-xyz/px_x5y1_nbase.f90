PURE FUNCTION px_x5y1_nbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cx_nbase(pes_x5y1_nki,pes_x5y1_sysnew,px_pcv)
return
END FUNCTION px_x5y1_nbase
