SUBROUTINE pes_z3_vadd (pc, cf)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: nb
if (allocated(pes_z3_vcf).and.pes_z3_vpc.eq.pc) then
 pes_z3_vcf = pes_z3_vcf+cf
else
! Require that pes_z3_vcf is null
 if (allocated(pes_z3_vcf)) then
  if (any(pes_z3_vcf.ne.0.0_wp)) then
   stop 'pes_z3_vadd: mismatch'
  endif
  deallocate (pes_z3_vcf)
 endif
 nb = pes_z3_nvb(pc%dg)
 allocate (pes_z3_vcf(0:nb-1))
 pes_z3_vpc = pc
 pes_z3_vcf = cf
endif
return
END SUBROUTINE pes_z3_vadd
