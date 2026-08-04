SUBROUTINE pes1_init (sys)
! Read pcf data files.
character (len=*), intent (in) :: sys
!------------------------------------------------------------------------
! Announce the intent
write (*,*) 'Expected data: ', sys(1:len_trim(sys))
! Read pcf data for generic xyz systems
call pes1_init_xyz (sys)
return
END SUBROUTINE pes1_init
