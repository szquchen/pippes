PURE FUNCTION px_x4y4z1_nvbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cxv_nbase(pes_x4y4z1_nki,pes_x4y4z1_sysnew,px_vpcv)
return
END FUNCTION px_x4y4z1_nvbase
