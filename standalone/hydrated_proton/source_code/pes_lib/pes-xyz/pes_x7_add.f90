SUBROUTINE pes_x7_add (pc, cf)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: nb
if (allocated(pes_x7_cf).and.pes_x7_pc.eq.pc) then
 pes_x7_cf = pes_x7_cf+cf
else
! Require that pes_x7_cf is null
 if (allocated(pes_x7_cf)) then
  if (any(pes_x7_cf.ne.0.0_wp)) then
   stop 'pes_x7_add: mismatch'
  endif
  deallocate (pes_x7_cf)
 endif
 nb = pes_x7_nb(pc%dg)
 allocate (pes_x7_cf(0:nb-1))
 pes_x7_pc = pc
 pes_x7_cf = cf
endif
return
END SUBROUTINE pes_x7_add
