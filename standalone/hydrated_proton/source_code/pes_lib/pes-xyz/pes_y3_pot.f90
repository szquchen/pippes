FUNCTION pes_y3_pot (xn) RESULT (f)
! Potential for generic Y3
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
integer, parameter :: nki(0:0)=pes_y3_nki
real (kind=wp) :: r(0:pes_y3_nk-1,0:pes_y3_nk-1)
call pes_dists (xn, r)
f = pes_y1_cf*nki(0)+ &
  cx_f2(nki,r,pes_y2_pc,pes_y2_cf)+ &
  cx_f3(nki,r,pes_y3_pc,pes_y3_cf)
return
END FUNCTION pes_y3_pot
