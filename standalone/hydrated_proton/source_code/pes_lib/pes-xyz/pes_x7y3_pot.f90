FUNCTION pes_x7y3_pot (xn) RESULT (f)
! Potential for generic X7Y3
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x7y3_nk-1,0:pes_x7y3_nk-1)
integer, parameter :: nki(0:1)=pes_x7y3_nki
call pes_dists (xn, r)
f = pes_x1_cf*nki(0)+pes_y1_cf*nki(1)+ &
  cx_f2(nki,r,pes_x2_pc,pes_x2_cf)+ &
  cx_f11(nki,r,pes_x1y1_pc,pes_x1y1_cf)+ &
  cx_f02(nki,r,pes_y2_pc,pes_y2_cf)+ &
  cx_f3(nki,r,pes_x3_pc,pes_x3_cf)+ &
  cx_f21(nki,r,pes_x2y1_pc,pes_x2y1_cf)+ &
  cx_f12(nki,r,pes_x1y2_pc,pes_x1y2_cf)+ &
  cx_f03(nki,r,pes_y3_pc,pes_y3_cf)+ &
  cx_f4(nki,r,pes_x4_pc,pes_x4_cf)+ &
  cx_f31(nki,r,pes_x3y1_pc,pes_x3y1_cf)+ &
  cx_f22(nki,r,pes_x2y2_pc,pes_x2y2_cf)+ &
  cx_f13(nki,r,pes_x1y3_pc,pes_x1y3_cf)+ &
  cx_f5(nki,r,pes_x5_pc,pes_x5_cf)+ &
  cx_f41(nki,r,pes_x4y1_pc,pes_x4y1_cf)+ &
  cx_f32(nki,r,pes_x3y2_pc,pes_x3y2_cf)+ &
  cx_f23(nki,r,pes_x2y3_pc,pes_x2y3_cf)+ &
  cx_f6(nki,r,pes_x6_pc,pes_x6_cf)+ &
  cx_f51(nki,r,pes_x5y1_pc,pes_x5y1_cf)+ &
  cx_f42(nki,r,pes_x4y2_pc,pes_x4y2_cf)+ &
  cx_f33(nki,r,pes_x3y3_pc,pes_x3y3_cf)+ &
  cx_f7(nki,r,pes_x7_pc,pes_x7_cf)+ &
  cx_f61(nki,r,pes_x6y1_pc,pes_x6y1_cf)+ &
  cx_f52(nki,r,pes_x5y2_pc,pes_x5y2_cf)+ &
  cx_f43(nki,r,pes_x4y3_pc,pes_x4y3_cf)+ &
  cx_f71(nki,r,pes_x7y1_pc,pes_x7y1_cf)+ &
  cx_f62(nki,r,pes_x6y2_pc,pes_x6y2_cf)+ &
  cx_f53(nki,r,pes_x5y3_pc,pes_x5y3_cf)+ &
  cx_f72(nki,r,pes_x7y2_pc,pes_x7y2_cf)+ &
  cx_f63(nki,r,pes_x6y3_pc,pes_x6y3_cf)+ &
  cx_f73(nki,r,pes_x7y3_pc,pes_x7y3_cf)
return
END FUNCTION pes_x7y3_pot
