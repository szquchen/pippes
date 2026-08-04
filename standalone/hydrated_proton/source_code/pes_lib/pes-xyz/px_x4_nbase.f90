PURE FUNCTION px_x4_nbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cx_nbase(pes_x4_nki,pes_x4_sysnew,px_pcv)
return
END FUNCTION px_x4_nbase
