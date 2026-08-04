SUBROUTINE pes_z2_add (pc, cf)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: nb
if (allocated(pes_z2_cf).and.pes_z2_pc.eq.pc) then
 pes_z2_cf = pes_z2_cf+cf
else
! Require that pes_z2_cf is null
 if (allocated(pes_z2_cf)) then
  if (any(pes_z2_cf.ne.0.0_wp)) then
   stop 'pes_z2_add: mismatch'
  endif
  deallocate (pes_z2_cf)
 endif
 nb = pes_z2_nb(pc%dg)
 allocate (pes_z2_cf(0:nb-1))
 pes_z2_pc = pc
 pes_z2_cf = cf
endif
return
END SUBROUTINE pes_z2_add
