MODULE pes_y3z1
!..use and access
use inv, wp=>inv_wp
use pes0
private
public :: pes_y3z1_read, pes_y3z1_add, &
  pes_y3z1_vread, pes_y3z1_vadd
!..data
save
integer, parameter, public :: &
  pes_y3z1_nki(0:1)=(/3,1/), pes_y3z1_nk=4, &
  pes_y3z1_nb(-1:size(cx_nb31(-1:))-2)=cx_nb31(-1:), &
  pes_y3z1_nvb(-1:size(cxv_nb31(-1:))-2)=cxv_nb31(-1:)
type (cx_t), public :: &
  pes_y3z1_pc = cx_null, &
  pes_y3z1_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_y3z1_cf(:), pes_y3z1_vcf(:)
!..procedures
CONTAINS
include 'pes_y3z1_read.f90'
include 'pes_y3z1_add.f90'
include 'pes_y3z1_vread.f90'
include 'pes_y3z1_vadd.f90'
END MODULE pes_y3z1
