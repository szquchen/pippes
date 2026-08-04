SUBROUTINE pes_y2z3_add (pc, cf)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: nb
if (allocated(pes_y2z3_cf).and.pes_y2z3_pc.eq.pc) then
 pes_y2z3_cf = pes_y2z3_cf+cf
else
! Require that pes_y2z3_cf is null
 if (allocated(pes_y2z3_cf)) then
  if (any(pes_y2z3_cf.ne.0.0_wp)) then
   stop 'pes_y2z3_add: mismatch'
  endif
  deallocate (pes_y2z3_cf)
 endif
 nb = pes_y2z3_nb(pc%dg)
 allocate (pes_y2z3_cf(0:nb-1))
 pes_y2z3_pc = pc
 pes_y2z3_cf = cf
endif
return
END SUBROUTINE pes_y2z3_add
