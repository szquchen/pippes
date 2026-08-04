PURE FUNCTION px_x3y3z2_nvbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cxv_nbase(pes_x3y3z2_nki,pes_x3y3z2_sysnew,px_vpcv)
return
END FUNCTION px_x3y3z2_nvbase
