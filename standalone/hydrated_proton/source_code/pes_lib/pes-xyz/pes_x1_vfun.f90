FUNCTION pes_x1_vfun (xn) RESULT (f)
! Vector function for generic X1
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f(0:size(xn,2)-1)
!-----------------------------------------------------------------------
! Trivial code, offered for consistency with other pes routines.
f = pes_x1_vcf*pes_x1_nki(0)
return
END FUNCTION pes_x1_vfun
