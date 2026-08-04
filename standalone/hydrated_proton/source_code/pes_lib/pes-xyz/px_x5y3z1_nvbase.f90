PURE FUNCTION px_x5y3z1_nvbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cxv_nbase(pes_x5y3z1_nki,pes_x5y3z1_sysnew,px_vpcv)
return
END FUNCTION px_x5y3z1_nvbase
