FUNCTION pes_x3_pot (xn) RESULT (f)
! Potential for generic X3
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
integer, parameter :: nki(0:0)=pes_x3_nki
real (kind=wp) :: r(0:pes_x3_nk-1,0:pes_x3_nk-1)
call pes_dists (xn, r)
! Dissociates into X1 + X2
f = pes_x1_cf*nki(0)+ &
  cx_f2(nki,r,pes_x2_pc,pes_x2_cf)+ &
  cx_f3(nki,r,pes_x3_pc,pes_x3_cf)
return
END FUNCTION pes_x3_pot
