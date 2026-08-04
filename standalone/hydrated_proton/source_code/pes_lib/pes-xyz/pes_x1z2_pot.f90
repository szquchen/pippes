FUNCTION pes_x1z2_pot (xn) RESULT (f)
! Potential for generic X1Z2
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x1z2_nk-1,0:pes_x1z2_nk-1)
integer, parameter :: nki(0:1)=pes_x1z2_nki
call pes_dists (xn, r)
! Dissociates into X1+Z2 and X1Z1+Z1
f = pes_x1_cf*nki(0)+pes_z1_cf*nki(1)+ &
  cx_f11(nki,r,pes_x1z1_pc,pes_x1z1_cf)+ &
  cx_f02(nki,r,pes_z2_pc,pes_z2_cf)+ &
  cx_f12(nki,r,pes_x1z2_pc,pes_x1z2_cf)
return
END FUNCTION pes_x1z2_pot
