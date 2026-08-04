FUNCTION pes_x4y3_vfun (xn) RESULT (f)
! Vector function for generic X4Y3
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f(0:size(xn,2)-1)
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x4y3_nk-1,0:pes_x4y3_nk-1)
integer, parameter :: nki(0:1)=pes_x4y3_nki
call pes_dists (xn, r)
f = cxv_f1(nki,pes_x1_vcf)+ &
  cxv_f01(nki,pes_y1_vcf)+ &
  cxv_f2(nki,r,pes_x2_vpc,pes_x2_vcf)+ &
  cxv_f11(nki,r,pes_x1y1_vpc,pes_x1y1_vcf)+ &
  cxv_f02(nki,r,pes_y2_vpc,pes_y2_vcf)+ &
  cxv_f3(nki,r,pes_x3_vpc,pes_x3_vcf)+ &
  cxv_f21(nki,r,pes_x2y1_vpc,pes_x2y1_vcf)+ &
  cxv_f12(nki,r,pes_x1y2_vpc,pes_x1y2_vcf)+ &
  cxv_f03(nki,r,pes_y3_vpc,pes_y3_vcf)+ &
  cxv_f4(nki,r,pes_x4_vpc,pes_x4_vcf)+ &
  cxv_f31(nki,r,pes_x3y1_vpc,pes_x3y1_vcf)+ &
  cxv_f22(nki,r,pes_x2y2_vpc,pes_x2y2_vcf)+ &
  cxv_f13(nki,r,pes_x1y3_vpc,pes_x1y3_vcf)+ &
  cxv_f41(nki,r,pes_x4y1_vpc,pes_x4y1_vcf)+ &
  cxv_f32(nki,r,pes_x3y2_vpc,pes_x3y2_vcf)+ &
  cxv_f23(nki,r,pes_x2y3_vpc,pes_x2y3_vcf)+ &
  cxv_f42(nki,r,pes_x4y2_vpc,pes_x4y2_vcf)+ &
  cxv_f33(nki,r,pes_x3y3_vpc,pes_x3y3_vcf)+ &
  cxv_f43(nki,r,pes_x4y3_vpc,pes_x4y3_vcf)
return
END FUNCTION pes_x4y3_vfun
