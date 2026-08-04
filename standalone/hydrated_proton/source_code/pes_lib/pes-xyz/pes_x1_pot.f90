FUNCTION pes_x1_pot (xn) RESULT (f)
! Potential for generic X1
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
! Trivial code, offered for consistency with other pes routines.
f = pes_x1_cf*pes_x1_nki(0)
return
END FUNCTION pes_x1_pot
