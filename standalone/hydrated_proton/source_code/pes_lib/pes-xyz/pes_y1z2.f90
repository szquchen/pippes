MODULE pes_y1z2
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_y1
use pes_z1
use pes_y1z1
use pes_z2
private
public :: pes_y1z2_read, pes_y1z2_pot, pes_y1z2_add, pes_y1z2_getcf, &
  pes_y1z2_vread, pes_y1z2_vfun, pes_y1z2_vadd, pes_y1z2_getvcf
!..data
save
integer, parameter, public :: &
  pes_y1z2_nki(0:1)=(/1,2/), pes_y1z2_nk=3, &
  pes_y1z2_nb(-1:size(cx_nb21(-1:))-2)=cx_nb21(-1:), &
  pes_y1z2_nvb(-1:size(cxv_nb21(-1:))-2)=cxv_nb21(-1:)
character (len=*), parameter, public :: &
  pes_y1z2_sysall='y1 z1 y1z1 z2 y1z2'
type (cx_t), public :: &
  pes_y1z2_pc = cx_null, &
  pes_y1z2_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_y1z2_cf(:), pes_y1z2_vcf(:)
!..procedures
CONTAINS
include 'pes_y1z2_read.f90'
include 'pes_y1z2_pot.f90'
include 'pes_y1z2_add.f90'
include 'pes_y1z2_getcf.f90'
include 'pes_y1z2_vread.f90'
include 'pes_y1z2_vfun.f90'
include 'pes_y1z2_vadd.f90'
include 'pes_y1z2_getvcf.f90'
END MODULE pes_y1z2
