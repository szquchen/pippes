SUBROUTINE pes_x1_add (cf)
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
if (size(cf).eq.1) then
 pes_x1_cf = pes_x1_cf+cf(0)
else
 stop 'pes_x1_add: size mismatch'
endif
return
END SUBROUTINE pes_x1_add
