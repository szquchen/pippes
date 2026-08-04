PURE FUNCTION px_x2y2z2_nbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cx_nbase(pes_x2y2z2_nki,pes_x2y2z2_sysnew,px_pcv)
return
END FUNCTION px_x2y2z2_nbase
