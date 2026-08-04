FUNCTION pes_x6y3z1_pot (xn) RESULT (f)
! Potential for generic X6Y3Z1
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x6y3z1_nk-1,0:pes_x6y3z1_nk-1)
integer, parameter :: nki(0:2)=pes_x6y3z1_nki
call pes_dists (xn, r)
f = pes_x1_cf*nki(0)+pes_y1_cf*nki(1)+pes_z1_cf*nki(2)+ &
  cx_f2(nki,r,pes_x2_pc,pes_x2_cf)+ &
  cx_f11(nki,r,pes_x1y1_pc,pes_x1y1_cf)+ &
  cx_f02(nki,r,pes_y2_pc,pes_y2_cf)+ &
  cx_f101(nki,r,pes_x1z1_pc,pes_x1z1_cf)+ &
  cx_f011(nki,r,pes_y1z1_pc,pes_y1z1_cf)+ &
  cx_f631(nki,r,pes_x6y3z1_pc,pes_x6y3z1_cf)
return
END FUNCTION pes_x6y3z1_pot
