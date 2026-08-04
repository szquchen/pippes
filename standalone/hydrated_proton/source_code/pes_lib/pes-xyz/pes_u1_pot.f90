FUNCTION pes_u1_pot (xn) RESULT (f)
! Potential for generic U1
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
! Trivial code, offered for consistency with other pes routines.
f = pes_u1_cf*pes_u1_nki(0)
return
END FUNCTION pes_u1_pot
