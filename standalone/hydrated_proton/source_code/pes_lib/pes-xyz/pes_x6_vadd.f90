SUBROUTINE pes_x6_vadd (pc, cf)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: nb
if (allocated(pes_x6_vcf).and.pes_x6_vpc.eq.pc) then
 pes_x6_vcf = pes_x6_vcf+cf
else
! Require that pes_x6_vcf is null
 if (allocated(pes_x6_vcf)) then
  if (any(pes_x6_vcf.ne.0.0_wp)) then
   stop 'pes_x6_vadd: mismatch'
  endif
  deallocate (pes_x6_vcf)
 endif
 nb = pes_x6_nvb(pc%dg)
 allocate (pes_x6_vcf(0:nb-1))
 pes_x6_vpc = pc
 pes_x6_vcf = cf
endif
return
END SUBROUTINE pes_x6_vadd
