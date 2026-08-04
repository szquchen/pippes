FUNCTION pes_x4_pot (xn) RESULT (f)
! Potential for generic X4
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
integer, parameter :: nki(0:0)=pes_x4_nki
real (kind=wp) :: r(0:pes_x4_nk-1,0:pes_x4_nk-1)
call pes_dists (xn, r)
! Dissociates into X1+X3 or X2+X2
f = pes_x1_cf*nki(0)+ &
  cx_f2(nki,r,pes_x2_pc,pes_x2_cf)+ &
  cx_f3(nki,r,pes_x3_pc,pes_x3_cf)+ &
  cx_f4(nki,r,pes_x4_pc,pes_x4_cf)
return
END FUNCTION pes_x4_pot
