MODULE pes_x3y1u1
!..use and access
use inv, wp=>inv_wp
use pes0
private
public :: pes_x3y1u1_read, pes_x3y1u1_add, &
  pes_x3y1u1_vread, pes_x3y1u1_vadd
!..data
save
integer, parameter, public :: &
  pes_x3y1u1_nki(0:2)=(/3,1,1/), pes_x3y1u1_nk=5, &
  pes_x3y1u1_nb(-1:size(cx_nb311(-1:))-2)=cx_nb311(-1:), &
  pes_x3y1u1_nvb(-1:size(cxv_nb311(-1:))-2)=cxv_nb311(-1:)
type (cx_t), public :: &
  pes_x3y1u1_pc = cx_null, &
  pes_x3y1u1_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x3y1u1_cf(:), &
  pes_x3y1u1_vcf(:)
!..procedures
CONTAINS
include 'pes_x3y1u1_read.f90'
include 'pes_x3y1u1_add.f90'
include 'pes_x3y1u1_vread.f90'
include 'pes_x3y1u1_vadd.f90'
END MODULE pes_x3y1u1
