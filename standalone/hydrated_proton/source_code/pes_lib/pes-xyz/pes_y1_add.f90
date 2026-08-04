SUBROUTINE pes_y1_add (cf)
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
if (size(cf).eq.1) then
 pes_y1_cf = pes_y1_cf+cf(0)
else
 stop 'pes_y1_add: size mismatch'
endif
return
END SUBROUTINE pes_y1_add
