FUNCTION pes_z1_pot (xn) RESULT (f)
! Potential for generic Z1
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
! Trivial code, offered for consistency with other pes routines.
f = pes_z1_cf*pes_z1_nki(0)
return
END FUNCTION pes_z1_pot
