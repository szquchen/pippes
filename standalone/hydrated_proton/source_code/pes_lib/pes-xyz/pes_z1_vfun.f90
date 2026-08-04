FUNCTION pes_z1_vfun (xn) RESULT (f)
! Vector function for generic Z1
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f(0:size(xn,2)-1)
!-----------------------------------------------------------------------
! Trivial code, offered for consistency with other pes routines.
f = pes_z1_vcf*pes_z1_nki(0)
return
END FUNCTION pes_z1_vfun
