SUBROUTINE pes_x2_vadd (pc, cf)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: nb
if (allocated(pes_x2_vcf).and.pes_x2_vpc.eq.pc) then
 pes_x2_vcf = pes_x2_vcf+cf
else
! Require that pes_x2_vcf is null
 if (allocated(pes_x2_vcf)) then
  if (any(pes_x2_vcf.ne.0.0_wp)) then
   stop 'pes_x2_vadd: mismatch'
  endif
  deallocate (pes_x2_vcf)
 endif
 nb = pes_x2_nvb(pc%dg)
 allocate (pes_x2_vcf(0:nb-1))
 pes_x2_vpc = pc
 pes_x2_vcf = cf
endif
return
END SUBROUTINE pes_x2_vadd
