FUNCTION pes_x7_vfun (xn) RESULT (f)
! Vector function for generic X7
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f(0:size(xn,2)-1)
!-----------------------------------------------------------------------
integer, parameter :: nki(0:0)=pes_x7_nki
real (kind=wp) :: r(0:pes_x7_nk-1,0:pes_x7_nk-1)
call pes_dists (xn, r)
f = cxv_f1(nki,pes_x1_vcf)+ &
  cxv_f2(nki,r,pes_x2_vpc,pes_x2_vcf)+ &
  cxv_f3(nki,r,pes_x3_vpc,pes_x3_vcf)+ &
  cxv_f4(nki,r,pes_x4_vpc,pes_x4_vcf)+ &
  cxv_f5(nki,r,pes_x5_vpc,pes_x5_vcf)+ &
  cxv_f6(nki,r,pes_x6_vpc,pes_x6_vcf)+ &
  cxv_f7(nki,r,pes_x7_vpc,pes_x7_vcf)
return
END FUNCTION pes_x7_vfun
