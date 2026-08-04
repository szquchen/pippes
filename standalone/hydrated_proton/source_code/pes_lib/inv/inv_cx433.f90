MODULE inv_cx433
!..use and access
use inv_core
use inv_mg433
use inv_cxx
implicit none
private
!..procedures
public :: cx_b433, cx_f433
!..data
integer, parameter, public :: &
  cx_nb433(-1:ubound(mg433_nb,dim=1))=(/0,mg433_nb(0:)/)
!..procedures
CONTAINS
include 'cx_b433.f90'
include 'cx_f433.f90'
END MODULE inv_cx433
