PURE FUNCTION px_x3y1z2_nbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cx_nbase(pes_x3y1z2_nki,pes_x3y1z2_sysnew,px_pcv)
return
END FUNCTION px_x3y1z2_nbase
