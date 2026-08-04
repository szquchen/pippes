PURE FUNCTION px_x1y1z2_nvbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cxv_nbase(pes_x1y1z2_nki,pes_x1y1z2_sysnew,px_vpcv)
return
END FUNCTION px_x1y1z2_nvbase
