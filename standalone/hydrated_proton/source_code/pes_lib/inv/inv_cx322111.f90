MODULE inv_cx322111
!..use and access
use inv_core
use inv_mg322111
use inv_cxx
implicit none
private
!..procedures
public :: cx_b322111, cx_f322111
!..data
integer, parameter, public :: &
  cx_nb322111(-1:ubound(mg322111_nb,dim=1))=(/0,mg322111_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b322111.f90'
include 'cx_f322111.f90'
END MODULE inv_cx322111
