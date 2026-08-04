PURE FUNCTION px_x5y4_nbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cx_nbase(pes_x5y4_nki,pes_x5y4_sysnew,px_pcv)
return
END FUNCTION px_x5y4_nbase
