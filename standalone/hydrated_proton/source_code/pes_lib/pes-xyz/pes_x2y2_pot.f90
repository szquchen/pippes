FUNCTION pes_x2y2_pot (xn) RESULT (f)
! Potential for generic X2Y2
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x2y2_nk-1,0:pes_x2y2_nk-1)
integer, parameter :: nki(0:1)=pes_x2y2_nki
call pes_dists (xn, r)
! Dissociates into X1+X1Y2, X2+Y2, X1Y1+X1Y1, X1Y2+Y1
f = pes_x1_cf*nki(0)+pes_y1_cf*nki(1)+ &
  cx_f2(nki,r,pes_x2_pc,pes_x2_cf)+ &
  cx_f11(nki,r,pes_x1y1_pc,pes_x1y1_cf)+ &
  cx_f02(nki,r,pes_y2_pc,pes_y2_cf)+ &
  cx_f21(nki,r,pes_x2y1_pc,pes_x2y1_cf)+ &
  cx_f12(nki,r,pes_x1y2_pc,pes_x1y2_cf)+ &
  cx_f22(nki,r,pes_x2y2_pc,pes_x2y2_cf)
return
END FUNCTION pes_x2y2_pot
