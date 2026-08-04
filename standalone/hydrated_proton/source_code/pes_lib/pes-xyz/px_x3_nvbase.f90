PURE FUNCTION px_x3_nvbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cxv_nbase(pes_x3_nki,pes_x3_sysnew,px_vpcv)
return
END FUNCTION px_x3_nvbase
