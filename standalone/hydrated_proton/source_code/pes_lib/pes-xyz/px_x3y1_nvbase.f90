PURE FUNCTION px_x3y1_nvbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cxv_nbase(pes_x3y1_nki,pes_x3y1_sysnew,px_vpcv)
return
END FUNCTION px_x3y1_nvbase
