MODULE pes_y1z3
!..use and access
use inv, wp=>inv_wp
use pes0
private
public :: pes_y1z3_read, pes_y1z3_add, pes_y1z3_vread, pes_y1z3_vadd
!..data
save
integer, parameter, public :: &
  pes_y1z3_nki(0:1)=(/1,3/), pes_y1z3_nk=4, &
  pes_y1z3_nb(-1:size(cx_nb31(-1:))-2)=cx_nb31(-1:), &
  pes_y1z3_nvb(-1:size(cxv_nb31(-1:))-2)=cxv_nb31(-1:)
type (cx_t), public :: &
  pes_y1z3_pc = cx_null, &
  pes_y1z3_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_y1z3_cf(:), pes_y1z3_vcf(:)
!..procedures
CONTAINS
include 'pes_y1z3_read.f90'
include 'pes_y1z3_add.f90'
include 'pes_y1z3_vread.f90'
include 'pes_y1z3_vadd.f90'
END MODULE pes_y1z3
