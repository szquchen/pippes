FUNCTION pes_y1_vfun (xn) RESULT (f)
! Vector function for generic Y1
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f(0:size(xn,2)-1)
!-----------------------------------------------------------------------
! Trivial code, offered for consistency with other pes routines.
f = pes_y1_vcf*pes_y1_nki(0)
return
END FUNCTION pes_y1_vfun
