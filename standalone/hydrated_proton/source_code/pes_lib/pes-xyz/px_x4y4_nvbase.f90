PURE FUNCTION px_x4y4_nvbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cxv_nbase(pes_x4y4_nki,pes_x4y4_sysnew,px_vpcv)
return
END FUNCTION px_x4y4_nvbase
