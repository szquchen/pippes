MODULE pes_x7
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_x2
use pes_x3
use pes_x4
use pes_x5
use pes_x6
private
public :: pes_x7_read, pes_x7_pot, pes_x7_add, pes_x7_getcf, &
  pes_x7_vread, pes_x7_vfun, pes_x7_vadd, pes_x7_getvcf
!..data
save
integer, parameter, public :: &
  pes_x7_nki(0:0)=(/7/), pes_x7_nk=7, &
  pes_x7_nb(-1:size(cx_nb7(-1:))-2)=cx_nb7(-1:), &
  pes_x7_nvb(-1:size(cxv_nb7(-1:))-2)=cxv_nb7(-1:)
character (len=*), parameter, public :: &
  pes_x7_sysall='x1 x2 x3 x4 x5 x6 x7'
type (cx_t), public :: &
  pes_x7_pc = cx_null, &
  pes_x7_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x7_cf(:), pes_x7_vcf(:)
!..procedures
CONTAINS
include 'pes_x7_read.f90'
include 'pes_x7_pot.f90'
include 'pes_x7_add.f90'
include 'pes_x7_getcf.f90'
include 'pes_x7_vread.f90'
include 'pes_x7_vfun.f90'
include 'pes_x7_vadd.f90'
include 'pes_x7_getvcf.f90'
END MODULE pes_x7
