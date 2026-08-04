SUBROUTINE pes_x8_vadd (pc, cf)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: nb
if (allocated(pes_x8_vcf).and.pes_x8_vpc.eq.pc) then
 pes_x8_vcf = pes_x8_vcf+cf
else
! Require that pes_x8_vcf is null
 if (allocated(pes_x8_vcf)) then
  if (any(pes_x8_vcf.ne.0.0_wp)) then
   stop 'pes_x8_vadd: mismatch'
  endif
  deallocate (pes_x8_vcf)
 endif
 nb = pes_x8_nvb(pc%dg)
 allocate (pes_x8_vcf(0:nb-1))
 pes_x8_vpc = pc
 pes_x8_vcf = cf
endif
return
END SUBROUTINE pes_x8_vadd
