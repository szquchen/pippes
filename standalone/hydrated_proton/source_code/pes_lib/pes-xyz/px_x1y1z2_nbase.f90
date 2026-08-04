PURE FUNCTION px_x1y1z2_nbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cx_nbase(pes_x1y1z2_nki,pes_x1y1z2_sysnew,px_pcv)
return
END FUNCTION px_x1y1z2_nbase
