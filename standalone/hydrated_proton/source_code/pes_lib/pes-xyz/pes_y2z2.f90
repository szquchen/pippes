MODULE pes_y2z2
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_y1
use pes_z1
use pes_y2
use pes_y1z1
use pes_z2
use pes_y2z1
use pes_y1z2
private
public :: pes_y2z2_read, pes_y2z2_add, pes_y2z2_vread, pes_y2z2_vadd
!..data
save
integer, parameter, public :: &
  pes_y2z2_nki(0:1)=(/2,2/), pes_y2z2_nk=4, &
  pes_y2z2_nb(-1:size(cx_nb22(-1:))-2)=cx_nb22(-1:), &
  pes_y2z2_nvb(-1:size(cxv_nb22(-1:))-2)=cxv_nb22(-1:)
character (len=*), parameter, public :: &
  pes_y2z2_sysall='y1 z1 y2 y1z1 z2 y2z1 y1z2 y2z2'
type (cx_t), public :: &
  pes_y2z2_pc = cx_null, &
  pes_y2z2_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_y2z2_cf(:), pes_y2z2_vcf(:)
!..procedures
CONTAINS
include 'pes_y2z2_read.f90'
include 'pes_y2z2_add.f90'
include 'pes_y2z2_vread.f90'
include 'pes_y2z2_vadd.f90'
END MODULE pes_y2z2
