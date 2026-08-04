MODULE inv_cx532
!..use and access
use inv_core
use inv_mg532
use inv_cxx
implicit none
private
!..procedures
public :: cx_b532, cx_f532
!..data
integer, parameter, public :: &
  cx_nb532(-1:ubound(mg532_nb,dim=1))=(/0,mg532_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b532.f90'
include 'cx_f532.f90'
END MODULE inv_cx532
