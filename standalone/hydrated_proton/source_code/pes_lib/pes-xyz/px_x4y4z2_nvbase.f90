PURE FUNCTION px_x4y4z2_nvbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cxv_nbase(pes_x4y4z2_nki,pes_x4y4z2_sysnew,px_vpcv)
return
END FUNCTION px_x4y4z2_nvbase
