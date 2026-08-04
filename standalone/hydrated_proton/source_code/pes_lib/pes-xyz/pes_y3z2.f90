MODULE pes_y3z2
!..use and access
use inv, wp=>inv_wp
use pes0
private
public :: pes_y3z2_read, pes_y3z2_add, pes_y3z2_vread, pes_y3z2_vadd
!..data
save
integer, parameter, public :: &
  pes_y3z2_nki(0:1)=(/3,2/), pes_y3z2_nk=5, &
  pes_y3z2_nb(-1:size(cx_nb32(-1:))-2)=cx_nb32(-1:), &
  pes_y3z2_nvb(-1:size(cxv_nb32(-1:))-2)=cxv_nb32(-1:)
type (cx_t), public :: &
  pes_y3z2_pc = cx_null, &
  pes_y3z2_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_y3z2_cf(:), pes_y3z2_vcf(:)
!..procedures
CONTAINS
include 'pes_y3z2_read.f90'
include 'pes_y3z2_add.f90'
include 'pes_y3z2_vread.f90'
include 'pes_y3z2_vadd.f90'
END MODULE pes_y3z2
