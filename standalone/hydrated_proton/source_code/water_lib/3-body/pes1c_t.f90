MODULE pes1c_t
!..use and access
use inv_t
use pes0_t
use pes_x6y3_t
!use pes1c_xyz
implicit none
private
public :: pes1_init
!..procedures
CONTAINS
SUBROUTINE pes1_init (sys)
! Read pcf data files.
character (len=*), intent (in) :: sys
!------------------------------------------------------------------------
! Announce the intent
!write (*,*) 'Expected data: ', sys(1:len_trim(sys))
write(*,*) "Initializing H2O-H2O-H2O potential!"
! Read pcf data for generic xyz systems
call pes1_init_xyz (sys)
return
END SUBROUTINE pes1_init

include 'pes1_init_xyz.f90'

END MODULE pes1c_t
