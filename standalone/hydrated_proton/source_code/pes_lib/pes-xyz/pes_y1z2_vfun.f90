FUNCTION pes_y1z2_vfun (xn) RESULT (f)
! Vector function for generic Y1Z2
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f(0:size(xn,2)-1)
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_y1z2_nk-1,0:pes_y1z2_nk-1)
integer, parameter :: nki(0:1)=pes_y1z2_nki
call pes_dists (xn, r)
! Dissociates into Y1+Z2 and Y1Z1+Z1
f = cxv_f1(nki,pes_y1_vcf)+ &
  cxv_f01(nki,pes_z1_vcf)+ &
  cxv_f11(nki,r,pes_y1z1_vpc,pes_y1z1_vcf)+ &
  cxv_f02(nki,r,pes_z2_vpc,pes_z2_vcf)+ &
  cxv_f12(nki,r,pes_y1z2_vpc,pes_y1z2_vcf)
return
END FUNCTION pes_y1z2_vfun
