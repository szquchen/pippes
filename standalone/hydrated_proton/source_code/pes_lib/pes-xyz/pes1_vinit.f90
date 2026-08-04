SUBROUTINE pes1_vinit (sys)
! Read vpcf data files.
character (len=*), intent (in) :: sys
!------------------------------------------------------------------------
! Announce the intent
write (*,*) 'Expected data: ', sys(1:len_trim(sys))
! Read vpcf data for generic xyz systems
call pes1_vinit_xyz (sys)
return
END SUBROUTINE pes1_vinit
