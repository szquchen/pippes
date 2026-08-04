SUBROUTINE pes_z2_vadd (pc, cf)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: nb
if (allocated(pes_z2_vcf).and.pes_z2_vpc.eq.pc) then
 pes_z2_vcf = pes_z2_vcf+cf
else
! Require that pes_z2_vcf is null
 if (allocated(pes_z2_vcf)) then
  if (any(pes_z2_vcf.ne.0.0_wp)) then
   stop 'pes_z2_vadd: mismatch'
  endif
  deallocate (pes_z2_vcf)
 endif
 nb = pes_z2_nvb(pc%dg)
 allocate (pes_z2_vcf(0:nb-1))
 pes_z2_vpc = pc
 pes_z2_vcf = cf
endif
return
END SUBROUTINE pes_z2_vadd
