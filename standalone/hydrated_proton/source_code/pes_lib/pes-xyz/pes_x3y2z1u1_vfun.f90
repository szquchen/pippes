FUNCTION pes_x3y2z1u1_vfun (xn) RESULT (f)
! Vector function for generic X3Y2Z1U1
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f(0:size(xn,2)-1)
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x3y2z1u1_nk-1,0:pes_x3y2z1u1_nk-1)
integer, parameter :: nki(0:3)=pes_x3y2z1u1_nki
call pes_dists (xn, r)
f = cxv_f1(nki,pes_x1_vcf)+ &
  cxv_f01(nki,pes_y1_vcf)+ &
  cxv_f001(nki,pes_z1_vcf)+ &
  cxv_f0001(nki,pes_u1_vcf)+ &
  cxv_f2(nki,r,pes_x2_vpc,pes_x2_vcf)+ &
  cxv_f11(nki,r,pes_x1y1_vpc,pes_x1y1_vcf)+ &
  cxv_f02(nki,r,pes_y2_vpc,pes_y2_vcf)+ &
  cxv_f101(nki,r,pes_x1z1_vpc,pes_x1z1_vcf)+ &
  cxv_f011(nki,r,pes_y1z1_vpc,pes_y1z1_vcf)+ &
  cxv_f1001(nki,r,pes_x1u1_vpc,pes_x1u1_vcf)+ &
  cxv_f0101(nki,r,pes_y1u1_vpc,pes_y1u1_vcf)+ &
  cxv_f0011(nki,r,pes_z1u1_vpc,pes_z1u1_vcf)+ &
  cxv_f3(nki,r,pes_x3_vpc,pes_x3_vcf)+ &
  cxv_f21(nki,r,pes_x2y1_vpc,pes_x2y1_vcf)+ &
  cxv_f12(nki,r,pes_x1y2_vpc,pes_x1y2_vcf)+ &
  cxv_f201(nki,r,pes_x2z1_vpc,pes_x2z1_vcf)+ &
  cxv_f111(nki,r,pes_x1y1z1_vpc,pes_x1y1z1_vcf)+ &
  cxv_f021(nki,r,pes_y2z1_vpc,pes_y2z1_vcf)+ &
  cxv_f2001(nki,r,pes_x2u1_vpc,pes_x2u1_vcf)+ &
  cxv_f1101(nki,r,pes_x1y1u1_vpc,pes_x1y1u1_vcf)+ &
  cxv_f0201(nki,r,pes_y2u1_vpc,pes_y2u1_vcf)+ &
  cxv_f1011(nki,r,pes_x1z1u1_vpc,pes_x1z1u1_vcf)+ &
  cxv_f0111(nki,r,pes_y1z1u1_vpc,pes_y1z1u1_vcf)+ &
  cxv_f31(nki,r,pes_x3y1_vpc,pes_x3y1_vcf)+ &
  cxv_f22(nki,r,pes_x2y2_vpc,pes_x2y2_vcf)+ &
  cxv_f301(nki,r,pes_x3z1_vpc,pes_x3z1_vcf)+ &
  cxv_f211(nki,r,pes_x2y1z1_vpc,pes_x2y1z1_vcf)+ &
  cxv_f121(nki,r,pes_x1y2z1_vpc,pes_x1y2z1_vcf)+ &
  cxv_f3001(nki,r,pes_x3u1_vpc,pes_x3u1_vcf)+ &
  cxv_f2101(nki,r,pes_x2y1u1_vpc,pes_x2y1u1_vcf)+ &
  cxv_f1201(nki,r,pes_x1y2u1_vpc,pes_x1y2u1_vcf)+ &
  cxv_f2011(nki,r,pes_x2z1u1_vpc,pes_x2z1u1_vcf)+ &
  cxv_f1111(nki,r,pes_x1y1z1u1_vpc,pes_x1y1z1u1_vcf)+ &
  cxv_f0211(nki,r,pes_y2z1u1_vpc,pes_y2z1u1_vcf)+ &
  cxv_f32(nki,r,pes_x3y2_vpc,pes_x3y2_vcf)+ &
  cxv_f311(nki,r,pes_x3y1z1_vpc,pes_x3y1z1_vcf)+ &
  cxv_f221(nki,r,pes_x2y2z1_vpc,pes_x2y2z1_vcf)+ &
  cxv_f3101(nki,r,pes_x3y1u1_vpc,pes_x3y1u1_vcf)+ &
  cxv_f2201(nki,r,pes_x2y2u1_vpc,pes_x2y2u1_vcf)+ &
  cxv_f3011(nki,r,pes_x3z1u1_vpc,pes_x3z1u1_vcf)+ &
  cxv_f2111(nki,r,pes_x2y1z1u1_vpc,pes_x2y1z1u1_vcf)+ &
  cxv_f1211(nki,r,pes_x1y2z1u1_vpc,pes_x1y2z1u1_vcf)+ &
  cxv_f321(nki,r,pes_x3y2z1_vpc,pes_x3y2z1_vcf)+ &
  cxv_f3201(nki,r,pes_x3y2u1_vpc,pes_x3y2u1_vcf)+ &
  cxv_f3111(nki,r,pes_x3y1z1u1_vpc,pes_x3y1z1u1_vcf)+ &
  cxv_f2211(nki,r,pes_x2y2z1u1_vpc,pes_x2y2z1u1_vcf)+ &
  cxv_f3211(nki,r,pes_x3y2z1u1_vpc,pes_x3y2z1u1_vcf)
return
END FUNCTION pes_x3y2z1u1_vfun
