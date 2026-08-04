MODULE pes_x4y3
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_y1
use pes_x2
use pes_x1y1
use pes_y2
use pes_x3
use pes_x2y1
use pes_x1y2
use pes_y3
use pes_x4
use pes_x3y1
use pes_x2y2
use pes_x1y3
use pes_x4y1
use pes_x3y2
use pes_x2y3
use pes_x4y2
use pes_x3y3
private
public :: pes_x4y3_read, pes_x4y3_pot, pes_x4y3_add, pes_x4y3_getcf, &
  pes_x4y3_vread, pes_x4y3_vfun, pes_x4y3_vadd, pes_x4y3_getvcf
!..data
save
integer, parameter, public :: &
  pes_x4y3_nki(0:1)=(/4,3/), pes_x4y3_nk=7, &
  pes_x4y3_nb(-1:size(cx_nb43(-1:))-2)=cx_nb43(-1:), &
  pes_x4y3_nvb(-1:size(cxv_nb43(-1:))-2)=cxv_nb43(-1:)
character (len=*), parameter, public :: &
  pes_x4y3_sysall='x1 y1 x2 x1y1 y2 x3 x2y1 x1y2 y3 '// &
    'x4 x3y1 x2y2 x1y3 x4y1 x3y2 x2y3 x4y2 x3y3 x4y3'
type (cx_t), public :: &
  pes_x4y3_pc = cx_null, &
  pes_x4y3_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x4y3_cf(:), pes_x4y3_vcf(:)
!..procedures
CONTAINS
include 'pes_x4y3_read.f90'
include 'pes_x4y3_pot.f90'
include 'pes_x4y3_add.f90'
include 'pes_x4y3_getcf.f90'
include 'pes_x4y3_vread.f90'
include 'pes_x4y3_vfun.f90'
include 'pes_x4y3_vadd.f90'
include 'pes_x4y3_getvcf.f90'
END MODULE pes_x4y3
