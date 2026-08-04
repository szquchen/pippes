MODULE inv_cxv522
!..use and access
use inv_core
use inv_mg522
use inv_mgv522
use inv_cxx
implicit none
private
!..procedures
public :: cxv_b522, cxv_f522
!..data
integer, parameter, public :: &
  cxv_nb522(-1:ubound(mgv522_nb,dim=1))=(/0,mgv522_nb(0:)/)
!..procedures
CONTAINS
include 'cxv_b522.f90'
include 'cxv_f522.f90'
END MODULE inv_cxv522
