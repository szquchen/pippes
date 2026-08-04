MODULE inv_cx211111111
!..use and access
use inv_core
use inv_mg211111111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b211111111, cx_f211111111
!..data
integer, parameter, public :: &
  cx_nb211111111(-1:ubound(mg211111111_nb,dim=1))=(/0,mg211111111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b211111111.f90'
include 'cx_f211111111.f90'
END MODULE inv_cx211111111
