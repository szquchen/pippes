MODULE pes_x2
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
private
public :: pes_x2_read, pes_x2_pot, pes_x2_add, pes_x2_getcf, &
  pes_x2_vread, pes_x2_vfun, pes_x2_vadd, pes_x2_getvcf
!..data
save
integer, parameter, public :: &
  pes_x2_nki(0:0)=(/2/), pes_x2_nk=2, &
  pes_x2_nb(-1:size(cx_nb2(-1:))-2)=cx_nb2(-1:), &
  pes_x2_nvb(-1:size(cxv_nb2(-1:))-2)=cxv_nb2(-1:)
character (len=*), parameter, public :: &
  pes_x2_sysall='x1 x2'
type (cx_t), public :: &
  pes_x2_pc = cx_null, &
  pes_x2_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x2_cf(:), pes_x2_vcf(:)
!..procedures
CONTAINS
include 'pes_x2_read.f90'
include 'pes_x2_pot.f90'
include 'pes_x2_add.f90'
include 'pes_x2_getcf.f90'
include 'pes_x2_vread.f90'
include 'pes_x2_vfun.f90'
include 'pes_x2_vadd.f90'
include 'pes_x2_getvcf.f90'
END MODULE pes_x2
