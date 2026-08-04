MODULE pes_y2z1
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_y1
use pes_z1
use pes_y2
use pes_y1z1
private
public :: pes_y2z1_read, pes_y2z1_pot, pes_y2z1_add, &
  pes_y2z1_getcf, pes_y2z1_vread, pes_y2z1_vfun, &
  pes_y2z1_vadd, pes_y2z1_getvcf
!..data
save
integer, parameter, public :: &
  pes_y2z1_nki(0:1)=(/2,1/), pes_y2z1_nk=3, &
  pes_y2z1_nb(-1:size(cx_nb21(-1:))-2)=cx_nb21(-1:), &
  pes_y2z1_nvb(-1:size(cxv_nb21(-1:))-2)=cxv_nb21(-1:)
character (len=*), parameter, public :: &
  pes_y2z1_sysall='y1 z1 y2 y1z1 y2z1 y2z1'
type (cx_t), public :: &
  pes_y2z1_pc = cx_null, &
  pes_y2z1_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_y2z1_cf(:), &
  pes_y2z1_vcf(:)
!..procedures
CONTAINS
include 'pes_y2z1_read.f90'
include 'pes_y2z1_pot.f90'
include 'pes_y2z1_add.f90'
include 'pes_y2z1_getcf.f90'
include 'pes_y2z1_vread.f90'
include 'pes_y2z1_vfun.f90'
include 'pes_y2z1_vadd.f90'
include 'pes_y2z1_getvcf.f90'
END MODULE pes_y2z1
