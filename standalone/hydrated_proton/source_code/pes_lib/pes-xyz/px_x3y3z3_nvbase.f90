PURE FUNCTION px_x3y3z3_nvbase () RESULT (nb)
integer :: nb
!-----------------------------------------------------------------------
nb = cxv_nbase(pes_x3y3z3_nki,pes_x3y3z3_sysnew,px_vpcv)
return
END FUNCTION px_x3y3z3_nvbase
