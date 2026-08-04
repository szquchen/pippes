PURE FUNCTION px_x4y4z2_nbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cx_nbase(pes_x4y4z2_nki,pes_x4y4z2_sysnew,px_pcv)
return
END FUNCTION px_x4y4z2_nbase
