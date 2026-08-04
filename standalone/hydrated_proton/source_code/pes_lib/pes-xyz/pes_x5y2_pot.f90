FUNCTION pes_x5y2_pot (xn) RESULT (f)
! Potential for generic X5Y2
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x5y2_nk-1,0:pes_x5y2_nk-1)
integer, parameter :: nki(0:1)=pes_x5y2_nki
call pes_dists (xn, r)
f = pes_x1_cf*nki(0)+pes_y1_cf*nki(1)+ &
  cx_f2(nki,r,pes_x2_pc,pes_x2_cf)+ &
  cx_f11(nki,r,pes_x1y1_pc,pes_x1y1_cf)+ &
  cx_f02(nki,r,pes_y2_pc,pes_y2_cf)+ &
  cx_f3(nki,r,pes_x3_pc,pes_x3_cf)+ &
  cx_f21(nki,r,pes_x2y1_pc,pes_x2y1_cf)+ &
  cx_f12(nki,r,pes_x1y2_pc,pes_x1y2_cf)+ &
  cx_f4(nki,r,pes_x4_pc,pes_x4_cf)+ &
  cx_f31(nki,r,pes_x3y1_pc,pes_x3y1_cf)+ &
  cx_f22(nki,r,pes_x2y2_pc,pes_x2y2_cf)+ &
  cx_f5(nki,r,pes_x5_pc,pes_x5_cf)+ &
  cx_f41(nki,r,pes_x4y1_pc,pes_x4y1_cf)+ &
  cx_f32(nki,r,pes_x3y2_pc,pes_x3y2_cf)+ &
  cx_f51(nki,r,pes_x5y1_pc,pes_x5y1_cf)+ &
  cx_f42(nki,r,pes_x4y2_pc,pes_x4y2_cf)+ &
  cx_f52(nki,r,pes_x5y2_pc,pes_x5y2_cf)
return
END FUNCTION pes_x5y2_pot
