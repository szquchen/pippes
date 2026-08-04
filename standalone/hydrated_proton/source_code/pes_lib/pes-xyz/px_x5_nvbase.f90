PURE FUNCTION px_x5_nvbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cxv_nbase(pes_x5_nki,pes_x5_sysnew,px_vpcv)
return
END FUNCTION px_x5_nvbase
