FUNCTION pes_x1y3z1_pot (xn) RESULT (f)
! Potential for generic X1Y3Z1
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x1y3z1_nk-1,0:pes_x1y3z1_nk-1)
integer, parameter :: nki(0:2)=pes_x1y3z1_nki
call pes_dists (xn, r)
f = pes_x1_cf*nki(0)+pes_y1_cf*nki(1)+pes_z1_cf*nki(2)+ &
  cx_f11(nki,r,pes_x1y1_pc,pes_x1y1_cf)+ &
  cx_f02(nki,r,pes_y2_pc,pes_y2_cf)+ &
  cx_f101(nki,r,pes_x1z1_pc,pes_x1z1_cf)+ &
  cx_f011(nki,r,pes_y1z1_pc,pes_y1z1_cf)+ &
  cx_f12(nki,r,pes_x1y2_pc,pes_x1y2_cf)+ &
  cx_f03(nki,r,pes_y3_pc,pes_y3_cf)+ &
  cx_f111(nki,r,pes_x1y1z1_pc,pes_x1y1z1_cf)+ &
  cx_f021(nki,r,pes_y2z1_pc,pes_y2z1_cf)+ &
  cx_f13(nki,r,pes_x1y3_pc,pes_x1y3_cf)+ &
  cx_f121(nki,r,pes_x1y2z1_pc,pes_x1y2z1_cf)+ &
  cx_f031(nki,r,pes_y3z1_pc,pes_y3z1_cf)+ &
  cx_f131(nki,r,pes_x1y3z1_pc,pes_x1y3z1_cf)
return
END FUNCTION pes_x1y3z1_pot
