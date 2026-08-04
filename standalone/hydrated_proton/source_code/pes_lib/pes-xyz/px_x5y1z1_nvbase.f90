PURE FUNCTION px_x5y1z1_nvbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cxv_nbase(pes_x5y1z1_nki,pes_x5y1z1_sysnew,px_vpcv)
return
END FUNCTION px_x5y1z1_nvbase
