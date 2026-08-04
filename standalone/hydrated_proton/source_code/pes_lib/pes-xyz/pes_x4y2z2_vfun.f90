FUNCTION pes_x4y2z2_vfun (xn) RESULT (f)
! Vector function for generic X4Y2Z2
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f(0:size(xn,2)-1)
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x4y2z2_nk-1,0:pes_x4y2z2_nk-1)
integer, parameter :: nki(0:2)=pes_x4y2z2_nki
call pes_dists (xn, r)
f = cxv_f1(nki,pes_x1_vcf)+ &
  cxv_f01(nki,pes_y1_vcf)+ &
  cxv_f001(nki,pes_z1_vcf)+ &
  cxv_f2(nki,r,pes_x2_vpc,pes_x2_vcf)+ &
  cxv_f11(nki,r,pes_x1y1_vpc,pes_x1y1_vcf)+ &
  cxv_f02(nki,r,pes_y2_vpc,pes_y2_vcf)+ &
  cxv_f101(nki,r,pes_x1z1_vpc,pes_x1z1_vcf)+ &
  cxv_f011(nki,r,pes_y1z1_vpc,pes_y1z1_vcf)+ &
  cxv_f002(nki,r,pes_z2_vpc,pes_z2_vcf)+ &
  cxv_f3(nki,r,pes_x3_vpc,pes_x3_vcf)+ &
  cxv_f21(nki,r,pes_x2y1_vpc,pes_x2y1_vcf)+ &
  cxv_f12(nki,r,pes_x1y2_vpc,pes_x1y2_vcf)+ &
  cxv_f201(nki,r,pes_x2z1_vpc,pes_x2z1_vcf)+ &
  cxv_f111(nki,r,pes_x1y1z1_vpc,pes_x1y1z1_vcf)+ &
  cxv_f021(nki,r,pes_y2z1_vpc,pes_y2z1_vcf)+ &
  cxv_f102(nki,r,pes_x1z2_vpc,pes_x1z2_vcf)+ &
  cxv_f012(nki,r,pes_y1z2_vpc,pes_y1z2_vcf)+ &
  cxv_f4(nki,r,pes_x4_vpc,pes_x4_vcf)+ &
  cxv_f31(nki,r,pes_x3y1_vpc,pes_x3y1_vcf)+ &
  cxv_f22(nki,r,pes_x2y2_vpc,pes_x2y2_vcf)+ &
  cxv_f301(nki,r,pes_x3z1_vpc,pes_x3z1_vcf)+ &
  cxv_f211(nki,r,pes_x2y1z1_vpc,pes_x2y1z1_vcf)+ &
  cxv_f121(nki,r,pes_x1y2z1_vpc,pes_x1y2z1_vcf)+ &
  cxv_f202(nki,r,pes_x2z2_vpc,pes_x2z2_vcf)+ &
  cxv_f112(nki,r,pes_x1y1z2_vpc,pes_x1y1z2_vcf)+ &
  cxv_f022(nki,r,pes_y2z2_vpc,pes_y2z2_vcf)+ &
  cxv_f41(nki,r,pes_x4y1_vpc,pes_x4y1_vcf)+ &
  cxv_f32(nki,r,pes_x3y2_vpc,pes_x3y2_vcf)+ &
  cxv_f401(nki,r,pes_x4z1_vpc,pes_x4z1_vcf)+ &
  cxv_f311(nki,r,pes_x3y1z1_vpc,pes_x3y1z1_vcf)+ &
  cxv_f221(nki,r,pes_x2y2z1_vpc,pes_x2y2z1_vcf)+ &
  cxv_f302(nki,r,pes_x3z2_vpc,pes_x3z2_vcf)+ &
  cxv_f212(nki,r,pes_x2y1z2_vpc,pes_x2y1z2_vcf)+ &
  cxv_f122(nki,r,pes_x1y2z2_vpc,pes_x1y2z2_vcf)+ &
  cxv_f42(nki,r,pes_x4y2_vpc,pes_x4y2_vcf)+ &
  cxv_f411(nki,r,pes_x4y1z1_vpc,pes_x4y1z1_vcf)+ &
  cxv_f321(nki,r,pes_x3y2z1_vpc,pes_x3y2z1_vcf)+ &
  cxv_f402(nki,r,pes_x4z2_vpc,pes_x4z2_vcf)+ &
  cxv_f312(nki,r,pes_x3y1z2_vpc,pes_x3y1z2_vcf)+ &
  cxv_f222(nki,r,pes_x2y2z2_vpc,pes_x2y2z2_vcf)+ &
  cxv_f421(nki,r,pes_x4y2z1_vpc,pes_x4y2z1_vcf)+ &
  cxv_f412(nki,r,pes_x4y1z2_vpc,pes_x4y1z2_vcf)+ &
  cxv_f322(nki,r,pes_x3y2z2_vpc,pes_x3y2z2_vcf)+ &
  cxv_f422(nki,r,pes_x4y2z2_vpc,pes_x4y2z2_vcf)
return
END FUNCTION pes_x4y2z2_vfun
