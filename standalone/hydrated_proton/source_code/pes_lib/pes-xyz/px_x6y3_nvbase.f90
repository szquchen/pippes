PURE FUNCTION px_x6y3_nvbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cxv_nbase(pes_x6y3_nki,pes_x6y3_sysnew,px_vpcv)
return
END FUNCTION px_x6y3_nvbase
