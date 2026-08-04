SUBROUTINE pes_x6y3_add (pc, cf)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: nb
if (allocated(pes_x6y3_cf).and.pes_x6y3_pc.eq.pc) then
 pes_x6y3_cf = pes_x6y3_cf+cf
else
! Require that pes_x6y3_cf is null
 if (allocated(pes_x6y3_cf)) then
  if (any(pes_x6y3_cf.ne.0.0_wp)) then
   stop 'pes_x6y3_add: mismatch'
  endif
  deallocate (pes_x6y3_cf)
 endif
 nb = pes_x6y3_nb(pc%dg)
 allocate (pes_x6y3_cf(0:nb-1))
 pes_x6y3_pc = pc
 pes_x6y3_cf = cf
endif
return
END SUBROUTINE pes_x6y3_add
