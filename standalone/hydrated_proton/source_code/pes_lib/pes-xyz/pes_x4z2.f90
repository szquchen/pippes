MODULE pes_x4z2
!..use and access
use inv, wp=>inv_wp
use pes0
private
public :: pes_x4z2_read, pes_x4z2_add, &
  pes_x4z2_vread, pes_x4z2_vadd
!..data
save
integer, parameter, public :: &
  pes_x4z2_nki(0:1)=(/4,2/), pes_x4z2_nk=6, &
  pes_x4z2_nb(-1:size(cx_nb42(-1:))-2)=cx_nb42(-1:), &
  pes_x4z2_nvb(-1:size(cxv_nb42(-1:))-2)=cxv_nb42(-1:)
type (cx_t), public :: &
  pes_x4z2_pc = cx_null, &
  pes_x4z2_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x4z2_cf(:), pes_x4z2_vcf(:)
!..procedures
CONTAINS
include 'pes_x4z2_read.f90'
include 'pes_x4z2_add.f90'
include 'pes_x4z2_vread.f90'
include 'pes_x4z2_vadd.f90'
END MODULE pes_x4z2
