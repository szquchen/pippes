SUBROUTINE cx_write (iun, pc)
! write an element of type cx_t to unit iun
integer, intent (in) :: iun
type (cx_t), intent (in) :: pc
write (iun,'(3(1x,i4),2x,2es20.12,2x,a)') pc, 'type(cx_t)'
return
END SUBROUTINE cx_write
