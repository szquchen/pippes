MODULE pes_x1y3
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_y1
use pes_x1y1
use pes_y2
use pes_x1y2
use pes_y3
private
public :: pes_x1y3_read, pes_x1y3_pot, pes_x1y3_add, pes_x1y3_getcf, &
  pes_x1y3_vread, pes_x1y3_vfun, pes_x1y3_vadd, pes_x1y3_getvcf
!..data
save
integer, parameter, public :: &
  pes_x1y3_nki(0:1)=(/1,3/), pes_x1y3_nk=4, &
  pes_x1y3_nb(-1:size(cx_nb31(-1:))-2)=cx_nb31(-1:), &
  pes_x1y3_nvb(-1:size(cxv_nb31(-1:))-2)=cxv_nb31(-1:)
character (len=*), parameter, public :: &
  pes_x1y3_sysall='x1 y1 x1y1 y2 x1y2 y3 x1y3'
type (cx_t), public :: &
  pes_x1y3_pc = cx_null, &
  pes_x1y3_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x1y3_cf(:), &
  pes_x1y3_vcf(:)
!..procedures
CONTAINS
include 'pes_x1y3_read.f90'
include 'pes_x1y3_pot.f90'
include 'pes_x1y3_add.f90'
include 'pes_x1y3_getcf.f90'
include 'pes_x1y3_vread.f90'
include 'pes_x1y3_vfun.f90'
include 'pes_x1y3_vadd.f90'
include 'pes_x1y3_getvcf.f90'
END MODULE pes_x1y3
