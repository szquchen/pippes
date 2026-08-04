MODULE inv_cx431
!..use and access
use inv_core
use inv_mg431
use inv_cxx
implicit none
private
!..procedures
public :: cx_b431, cx_f431
!..data
integer, parameter, public :: &
  cx_nb431(-1:ubound(mg431_nb,dim=1))=(/0,mg431_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b431.f90'
include 'cx_f431.f90'
END MODULE inv_cx431
