FUNCTION pes_x1z2_vfun (xn) RESULT (f)
! Vector function for generic X1Z2
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f(0:size(xn,2)-1)
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x1z2_nk-1,0:pes_x1z2_nk-1)
integer, parameter :: nki(0:1)=pes_x1z2_nki
call pes_dists (xn, r)
! Dissociates into X1+Z2 and X1Z1+Z1
f = cxv_f1(nki,pes_x1_vcf)+ &
  cxv_f01(nki,pes_z1_vcf)+ &
  cxv_f11(nki,r,pes_x1z1_vpc,pes_x1z1_vcf)+ &
  cxv_f02(nki,r,pes_z2_vpc,pes_z2_vcf)+ &
  cxv_f12(nki,r,pes_x1z2_vpc,pes_x1z2_vcf)
return
END FUNCTION pes_x1z2_vfun
