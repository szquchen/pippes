MODULE pes_x4
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_x2
use pes_x3
private
public :: pes_x4_read, pes_x4_pot, pes_x4_add, pes_x4_getcf, &
  pes_x4_vread, pes_x4_vfun, pes_x4_vadd, pes_x4_getvcf
!..data
save
integer, parameter, public :: &
  pes_x4_nki(0:0)=(/4/), pes_x4_nk=4, &
  pes_x4_nb(-1:size(cx_nb4(-1:))-2)=cx_nb4(-1:), &
  pes_x4_nvb(-1:size(cxv_nb4(-1:))-2)=cxv_nb4(-1:)
character (len=*), parameter, public :: &
  pes_x4_sysall='x1 x2 x3 x4'
type (cx_t), public :: &
  pes_x4_pc = cx_null, &
  pes_x4_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x4_cf(:), pes_x4_vcf(:)
!..procedures
CONTAINS
include 'pes_x4_read.f90'
include 'pes_x4_pot.f90'
include 'pes_x4_add.f90'
include 'pes_x4_getcf.f90'
include 'pes_x4_vread.f90'
include 'pes_x4_vfun.f90'
include 'pes_x4_vadd.f90'
include 'pes_x4_getvcf.f90'
END MODULE pes_x4
