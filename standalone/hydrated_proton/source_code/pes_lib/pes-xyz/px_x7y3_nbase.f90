PURE FUNCTION px_x7y3_nbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cx_nbase(pes_x7y3_nki,pes_x7y3_sysnew,px_pcv)
return
END FUNCTION px_x7y3_nbase
