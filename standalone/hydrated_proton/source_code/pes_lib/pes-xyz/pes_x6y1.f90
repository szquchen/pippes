MODULE pes_x6y1
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_y1
use pes_x2
use pes_x1y1
use pes_x3
use pes_x2y1
use pes_x4
use pes_x3y1
use pes_x5
use pes_x4y1
use pes_x6
use pes_x5y1
private
public :: pes_x6y1_read, pes_x6y1_pot, pes_x6y1_add, pes_x6y1_getcf, &
  pes_x6y1_vread, pes_x6y1_vfun, pes_x6y1_vadd, pes_x6y1_getvcf
!..data
save
integer, parameter, public :: &
  pes_x6y1_nki(0:1)=(/6,1/), pes_x6y1_nk=7, &
  pes_x6y1_nb(-1:size(cx_nb61(-1:))-2)=cx_nb61(-1:), &
  pes_x6y1_nvb(-1:size(cxv_nb61(-1:))-2)=cxv_nb61(-1:)
character (len=*), parameter, public :: &
  pes_x6y1_sysall='x1 y1 x2 x1y1 x3 x2y1 x4 x3y1 x5 x4y1 x6 x5y1 x6y1'
type (cx_t), public :: &
  pes_x6y1_pc = cx_null, &
  pes_x6y1_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x6y1_cf(:), pes_x6y1_vcf(:)
!..procedures
CONTAINS
include 'pes_x6y1_read.f90'
include 'pes_x6y1_pot.f90'
include 'pes_x6y1_add.f90'
include 'pes_x6y1_getcf.f90'
include 'pes_x6y1_vread.f90'
include 'pes_x6y1_vfun.f90'
include 'pes_x6y1_vadd.f90'
include 'pes_x6y1_getvcf.f90'
END MODULE pes_x6y1
