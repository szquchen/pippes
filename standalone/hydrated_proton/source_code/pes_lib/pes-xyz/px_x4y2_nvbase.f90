PURE FUNCTION px_x4y2_nvbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cxv_nbase(pes_x4y2_nki,pes_x4y2_sysnew,px_vpcv)
return
END FUNCTION px_x4y2_nvbase
