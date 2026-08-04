PURE FUNCTION px_x2y2z1u1_nvbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cxv_nbase(pes_x2y2z1u1_nki,pes_x2y2z1u1_sysnew,px_vpcv)
return
END FUNCTION px_x2y2z1u1_nvbase
