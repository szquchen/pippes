MODULE pes_x2z2
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_z1
use pes_x2
use pes_x1z1
use pes_z2
use pes_x2z1
use pes_x1z2
private
public :: pes_x2z2_read, pes_x2z2_add, pes_x2z2_vread, pes_x2z2_vadd
!..data
save
integer, parameter, public :: &
  pes_x2z2_nki(0:1)=(/2,2/), pes_x2z2_nk=4, &
  pes_x2z2_nb(-1:size(cx_nb22(-1:))-2)=cx_nb22(-1:), &
  pes_x2z2_nvb(-1:size(cxv_nb22(-1:))-2)=cxv_nb22(-1:)
character (len=*), parameter, public :: &
  pes_x2z2_sysall='x1 z1 x2 x1z1 z2 x2z1 x1z2 x2z2'
type (cx_t), public :: &
  pes_x2z2_pc = cx_null, &
  pes_x2z2_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x2z2_cf(:), pes_x2z2_vcf(:)
!..procedures
CONTAINS
include 'pes_x2z2_read.f90'
include 'pes_x2z2_add.f90'
include 'pes_x2z2_vread.f90'
include 'pes_x2z2_vadd.f90'
END MODULE pes_x2z2
