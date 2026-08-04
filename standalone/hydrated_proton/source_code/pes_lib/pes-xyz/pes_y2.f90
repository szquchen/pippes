MODULE pes_y2
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_y1
private
public :: pes_y2_read, pes_y2_pot, pes_y2_add, pes_y2_getcf, &
  pes_y2_vread, pes_y2_vfun, pes_y2_vadd, pes_y2_getvcf
!..data
save
integer, parameter, public :: &
  pes_y2_nki(0:0)=(/2/), pes_y2_nk=2, &
  pes_y2_nb(-1:size(cx_nb2(-1:))-2)=cx_nb2(-1:), &
  pes_y2_nvb(-1:size(cxv_nb2(-1:))-2)=cxv_nb2(-1:)
character (len=*), parameter, public :: &
  pes_y2_sysall='y1 y2'
type (cx_t), public :: &
  pes_y2_pc = cx_null, &
  pes_y2_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_y2_cf(:), pes_y2_vcf(:)
!..procedures
CONTAINS
include 'pes_y2_read.f90'
include 'pes_y2_pot.f90'
include 'pes_y2_add.f90'
include 'pes_y2_getcf.f90'
include 'pes_y2_vread.f90'
include 'pes_y2_vfun.f90'
include 'pes_y2_vadd.f90'
include 'pes_y2_getvcf.f90'
END MODULE pes_y2
