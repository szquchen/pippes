FUNCTION pes_z1u1_pot (xn) RESULT (f)
! Potential for generic Z1U1
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
integer, parameter :: nki(0:1)=pes_z1u1_nki
real (kind=wp) :: r(0:pes_z1u1_nk-1,0:pes_z1u1_nk-1)
call pes_dists (xn, r)
f = pes_z1_cf*nki(0)+pes_u1_cf*nki(1)+ &
  cx_f11(nki,r,pes_z1u1_pc,pes_z1u1_cf)
return
END FUNCTION pes_z1u1_pot
