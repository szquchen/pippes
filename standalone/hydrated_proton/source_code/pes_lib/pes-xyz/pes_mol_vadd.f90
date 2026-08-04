SUBROUTINE pes_mol_vadd (pc, cf)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: nb
if (allocated(mol_vcf).and.mol_vpc.eq.pc) then
 mol_vcf = mol_vcf+cf
else
! Require that mol_vcf is null
 if (allocated(mol_vcf)) then
  if (any(mol_vcf.ne.0.0_wp)) then
   stop 'pes_mol_vadd: mismatch'
  endif
  deallocate (mol_vcf)
 endif
 nb = mol_nvb(pc%dg)
 allocate (mol_vcf(0:nb-1))
 mol_vpc = pc
 mol_vcf = cf
endif
return
END SUBROUTINE pes_mol_vadd
