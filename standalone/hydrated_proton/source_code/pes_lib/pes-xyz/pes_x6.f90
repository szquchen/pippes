MODULE pes_x6
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_x2
use pes_x3
use pes_x4
use pes_x5
private
public :: pes_x6_read, pes_x6_pot, pes_x6_add, pes_x6_getcf, &
  pes_x6_vread, pes_x6_vfun, pes_x6_vadd, pes_x6_getvcf
!..data
save
integer, parameter, public :: &
  pes_x6_nki(0:0)=(/6/), pes_x6_nk=6, &
  pes_x6_nb(-1:size(cx_nb6(-1:))-2)=cx_nb6(-1:), &
  pes_x6_nvb(-1:size(cxv_nb6(-1:))-2)=cxv_nb6(-1:)
character (len=*), parameter, public :: &
  pes_x6_sysall='x1 x2 x3 x4 x5 x6'
type (cx_t), public :: &
  pes_x6_pc = cx_null, &
  pes_x6_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x6_cf(:), pes_x6_vcf(:)
!..procedures
CONTAINS
include 'pes_x6_read.f90'
include 'pes_x6_pot.f90'
include 'pes_x6_add.f90'
include 'pes_x6_getcf.f90'
include 'pes_x6_vread.f90'
include 'pes_x6_vfun.f90'
include 'pes_x6_vadd.f90'
include 'pes_x6_getvcf.f90'
END MODULE pes_x6
