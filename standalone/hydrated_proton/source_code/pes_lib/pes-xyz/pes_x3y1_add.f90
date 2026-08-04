SUBROUTINE pes_x3y1_add (pc, cf)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: nb
if (allocated(pes_x3y1_cf).and.pes_x3y1_pc.eq.pc) then
 pes_x3y1_cf = pes_x3y1_cf+cf
else
! Require that pes_x3y1_cf is null
 if (allocated(pes_x3y1_cf)) then
  if (any(pes_x3y1_cf.ne.0.0_wp)) then
   stop 'pes_x3y1_add: mismatch'
  endif
  deallocate (pes_x3y1_cf)
 endif
 nb = pes_x3y1_nb(pc%dg)
 allocate (pes_x3y1_cf(0:nb-1))
 pes_x3y1_pc = pc
 pes_x3y1_cf = cf
endif
return
END SUBROUTINE pes_x3y1_add
