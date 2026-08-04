FUNCTION pes_x3y3z3_vfun (xn) RESULT (f)
! Vector function for generic X3Y3Z3
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f(0:size(xn,2)-1)
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x3y3z3_nk-1,0:pes_x3y3z3_nk-1)
integer, parameter :: nki(0:2)=pes_x3y3z3_nki
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
  cxv_f03(nki,r,pes_y3_vpc,pes_y3_vcf)+ &
  cxv_f201(nki,r,pes_x2z1_vpc,pes_x2z1_vcf)+ &
  cxv_f111(nki,r,pes_x1y1z1_vpc,pes_x1y1z1_vcf)+ &
  cxv_f021(nki,r,pes_y2z1_vpc,pes_y2z1_vcf)+ &
  cxv_f102(nki,r,pes_x1z2_vpc,pes_x1z2_vcf)+ &
  cxv_f012(nki,r,pes_y1z2_vpc,pes_y1z2_vcf)+ &
  cxv_f003(nki,r,pes_z3_vpc,pes_z3_vcf)+ &
  cxv_f31(nki,r,pes_x3y1_vpc,pes_x3y1_vcf)+ &
  cxv_f22(nki,r,pes_x2y2_vpc,pes_x2y2_vcf)+ &
  cxv_f13(nki,r,pes_x1y3_vpc,pes_x1y3_vcf)+ &
  cxv_f301(nki,r,pes_x3z1_vpc,pes_x3z1_vcf)+ &
  cxv_f211(nki,r,pes_x2y1z1_vpc,pes_x2y1z1_vcf)+ &
  cxv_f121(nki,r,pes_x1y2z1_vpc,pes_x1y2z1_vcf)+ &
  cxv_f031(nki,r,pes_y3z1_vpc,pes_y3z1_vcf)+ &
  cxv_f202(nki,r,pes_x2z2_vpc,pes_x2z2_vcf)+ &
  cxv_f112(nki,r,pes_x1y1z2_vpc,pes_x1y1z2_vcf)+ &
  cxv_f022(nki,r,pes_y2z2_vpc,pes_y2z2_vcf)+ &
  cxv_f103(nki,r,pes_x1z3_vpc,pes_x1z3_vcf)+ &
  cxv_f013(nki,r,pes_y1z3_vpc,pes_y1z3_vcf)+ &
  cxv_f32(nki,r,pes_x3y2_vpc,pes_x3y2_vcf)+ &
  cxv_f23(nki,r,pes_x2y3_vpc,pes_x2y3_vcf)+ &
  cxv_f311(nki,r,pes_x3y1z1_vpc,pes_x3y1z1_vcf)+ &
  cxv_f221(nki,r,pes_x2y2z1_vpc,pes_x2y2z1_vcf)+ &
  cxv_f131(nki,r,pes_x1y3z1_vpc,pes_x1y3z1_vcf)+ &
  cxv_f302(nki,r,pes_x3z2_vpc,pes_x3z2_vcf)+ &
  cxv_f212(nki,r,pes_x2y1z2_vpc,pes_x2y1z2_vcf)+ &
  cxv_f122(nki,r,pes_x1y2z2_vpc,pes_x1y2z2_vcf)+ &
  cxv_f032(nki,r,pes_y3z2_vpc,pes_y3z2_vcf)+ &
  cxv_f203(nki,r,pes_x2z3_vpc,pes_x2z3_vcf)+ &
  cxv_f113(nki,r,pes_x1y1z3_vpc,pes_x1y1z3_vcf)+ &
  cxv_f023(nki,r,pes_y2z3_vpc,pes_y2z3_vcf)+ &
  cxv_f33(nki,r,pes_x3y3_vpc,pes_x3y3_vcf)+ &
  cxv_f321(nki,r,pes_x3y2z1_vpc,pes_x3y2z1_vcf)+ &
  cxv_f231(nki,r,pes_x2y3z1_vpc,pes_x2y3z1_vcf)+ &
  cxv_f312(nki,r,pes_x3y1z2_vpc,pes_x3y1z2_vcf)+ &
  cxv_f222(nki,r,pes_x2y2z2_vpc,pes_x2y2z2_vcf)+ &
  cxv_f132(nki,r,pes_x1y3z2_vpc,pes_x1y3z2_vcf)+ &
  cxv_f303(nki,r,pes_x3z3_vpc,pes_x3z3_vcf)+ &
  cxv_f213(nki,r,pes_x2y1z3_vpc,pes_x2y1z3_vcf)+ &
  cxv_f123(nki,r,pes_x1y2z3_vpc,pes_x1y2z3_vcf)+ &
  cxv_f033(nki,r,pes_y3z3_vpc,pes_y3z3_vcf)+ &
  cxv_f331(nki,r,pes_x3y3z1_vpc,pes_x3y3z1_vcf)+ &
  cxv_f322(nki,r,pes_x3y2z2_vpc,pes_x3y2z2_vcf)+ &
  cxv_f232(nki,r,pes_x2y3z2_vpc,pes_x2y3z2_vcf)+ &
  cxv_f313(nki,r,pes_x3y1z3_vpc,pes_x3y1z3_vcf)+ &
  cxv_f223(nki,r,pes_x2y2z3_vpc,pes_x2y2z3_vcf)+ &
  cxv_f133(nki,r,pes_x1y3z3_vpc,pes_x1y3z3_vcf)+ &
  cxv_f332(nki,r,pes_x3y3z2_vpc,pes_x3y3z2_vcf)+ &
  cxv_f323(nki,r,pes_x3y2z3_vpc,pes_x3y2z3_vcf)+ &
  cxv_f233(nki,r,pes_x2y3z3_vpc,pes_x2y3z3_vcf)+ &
  cxv_f333(nki,r,pes_x3y3z3_vpc,pes_x3y3z3_vcf)
return
END FUNCTION pes_x3y3z3_vfun
