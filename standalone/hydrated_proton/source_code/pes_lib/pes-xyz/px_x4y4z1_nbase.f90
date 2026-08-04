PURE FUNCTION px_x4y4z1_nbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cx_nbase(pes_x4y4z1_nki,pes_x4y4z1_sysnew,px_pcv)
return
END FUNCTION px_x4y4z1_nbase
