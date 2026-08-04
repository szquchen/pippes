PURE FUNCTION px_x5y1z1_nbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cx_nbase(pes_x5y1z1_nki,pes_x5y1z1_sysnew,px_pcv)
return
END FUNCTION px_x5y1z1_nbase
