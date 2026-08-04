MODULE pes_z3
!..use and access
use inv, wp=>inv_wp
use pes0
private
public :: pes_z3_read, pes_z3_add, pes_z3_vread, pes_z3_vadd
!..data
save
integer, parameter, public :: &
  pes_z3_nki(0:0)=(/3/), pes_z3_nk=3, &
  pes_z3_nb(-1:size(cx_nb3(-1:))-2)=cx_nb3(-1:), &
  pes_z3_nvb(-1:size(cxv_nb3(-1:))-2)=cxv_nb3(-1:)
type (cx_t), public :: &
  pes_z3_pc = cx_null, &
  pes_z3_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_z3_cf(:), &
  pes_z3_vcf(:)
!..procedures
CONTAINS
include 'pes_z3_read.f90'
include 'pes_z3_add.f90'
include 'pes_z3_vread.f90'
include 'pes_z3_vadd.f90'
END MODULE pes_z3
