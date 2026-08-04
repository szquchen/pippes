SUBROUTINE pes_x5y4_add (pc, cf)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: nb
if (allocated(pes_x5y4_cf).and.pes_x5y4_pc.eq.pc) then
 pes_x5y4_cf = pes_x5y4_cf+cf
else
! Require that pes_x5y4_cf is null
 if (allocated(pes_x5y4_cf)) then
  if (any(pes_x5y4_cf.ne.0.0_wp)) then
   stop 'pes_x5y4_add: mismatch'
  endif
  deallocate (pes_x5y4_cf)
 endif
 nb = pes_x5y4_nb(pc%dg)
 allocate (pes_x5y4_cf(0:nb-1))
 pes_x5y4_pc = pc
 pes_x5y4_cf = cf
endif
return
END SUBROUTINE pes_x5y4_add
