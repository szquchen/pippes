SUBROUTINE pes_x7_vadd (pc, cf)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: nb
if (allocated(pes_x7_vcf).and.pes_x7_vpc.eq.pc) then
 pes_x7_vcf = pes_x7_vcf+cf
else
! Require that pes_x7_vcf is null
 if (allocated(pes_x7_vcf)) then
  if (any(pes_x7_vcf.ne.0.0_wp)) then
   stop 'pes_x7_vadd: mismatch'
  endif
  deallocate (pes_x7_vcf)
 endif
 nb = pes_x7_nvb(pc%dg)
 allocate (pes_x7_vcf(0:nb-1))
 pes_x7_vpc = pc
 pes_x7_vcf = cf
endif
return
END SUBROUTINE pes_x7_vadd
