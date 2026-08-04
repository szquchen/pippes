FUNCTION pes_x1y1_pot (xn) RESULT (f)
! Potential for generic X1Y1
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
integer, parameter :: nki(0:1)=pes_x1y1_nki
real (kind=wp) :: r(0:pes_x1y1_nk-1,0:pes_x1y1_nk-1)
call pes_dists (xn, r)
! Dissociates into X1 + Y1
f = pes_x1_cf*nki(0)+pes_y1_cf*nki(1)+ &
  cx_f11(nki,r,pes_x1y1_pc,pes_x1y1_cf)
return
END FUNCTION pes_x1y1_pot
