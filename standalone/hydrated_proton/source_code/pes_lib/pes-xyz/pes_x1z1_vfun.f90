FUNCTION pes_x1z1_vfun (xn) RESULT (f)
! Vector function for generic X1Z1
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f(0:size(xn,2)-1)
!-----------------------------------------------------------------------
integer, parameter :: nki(0:1)=pes_x1z1_nki
real (kind=wp) :: r(0:pes_x1z1_nk-1,0:pes_x1z1_nk-1)
call pes_dists (xn, r)
! Dissociates into X1 + Z1
f = cxv_f1(nki,pes_x1_vcf)+ &
  cxv_f01(nki,pes_z1_vcf)+ &
  cxv_f11(nki,r,pes_x1z1_vpc,pes_x1z1_vcf)
return
END FUNCTION pes_x1z1_vfun
