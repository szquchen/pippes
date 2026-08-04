SUBROUTINE pes_z1u1_add (pc, cf)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: nb
if (allocated(pes_z1u1_cf).and.pes_z1u1_pc.eq.pc) then
 pes_z1u1_cf = pes_z1u1_cf+cf
else
! Require that pes_z1u1_cf is null
 if (allocated(pes_z1u1_cf)) then
  if (any(pes_z1u1_cf.ne.0.0_wp)) then
   stop 'pes_z1u1_add: mismatch'
  endif
  deallocate (pes_z1u1_cf)
 endif
 nb = pes_z1u1_nb(pc%dg)
 allocate (pes_z1u1_cf(0:nb-1))
 pes_z1u1_pc = pc
 pes_z1u1_cf = cf
endif
return
END SUBROUTINE pes_z1u1_add
