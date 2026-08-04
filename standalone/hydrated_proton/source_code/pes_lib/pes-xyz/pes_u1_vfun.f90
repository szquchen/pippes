FUNCTION pes_u1_vfun (xn) RESULT (f)
! Vector function for generic U1
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f(0:size(xn,2)-1)
!-----------------------------------------------------------------------
! Trivial code, offered for consistency with other pes routines.
f = pes_u1_vcf*pes_u1_nki(0)
return
END FUNCTION pes_u1_vfun
