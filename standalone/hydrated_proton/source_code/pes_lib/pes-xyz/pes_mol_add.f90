SUBROUTINE pes_mol_add (pc, cf)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: nb
if (allocated(mol_cf).and.mol_pc.eq.pc) then
 mol_cf = mol_cf+cf
else
! Require that mol_cf is null
 if (allocated(mol_cf)) then
  if (any(mol_cf.ne.0.0_wp)) then
   stop 'pes_mol_add: mismatch'
  endif
  deallocate (mol_cf)
 endif
 nb = mol_nb(pc%dg)
 allocate (mol_cf(0:nb-1))
 mol_pc = pc
 mol_cf = cf
endif
return
END SUBROUTINE pes_mol_add
