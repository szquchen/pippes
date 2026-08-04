MODULE inv_cx211111
!..use and access
use inv_core
use inv_mg211111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b211111, cx_f211111
!..data
integer, parameter, public :: &
  cx_nb211111(-1:ubound(mg211111_nb,dim=1))=(/0,mg211111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b211111.f90'
include 'cx_f211111.f90'
END MODULE inv_cx211111
