MODULE pes_x1y2
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_y1
use pes_x1y1
use pes_y2
private
public :: pes_x1y2_read, pes_x1y2_pot, pes_x1y2_add, pes_x1y2_getcf, &
  pes_x1y2_vread, pes_x1y2_vfun, pes_x1y2_vadd, pes_x1y2_getvcf
!..data
save
integer, parameter, public :: &
  pes_x1y2_nki(0:1)=(/1,2/), pes_x1y2_nk=3, &
  pes_x1y2_nb(-1:size(cx_nb21(-1:))-2)=cx_nb21(-1:), &
  pes_x1y2_nvb(-1:size(cxv_nb21(-1:))-2)=cxv_nb21(-1:)
character (len=*), parameter, public :: &
  pes_x1y2_sysall='x1 y1 x1y1 y2 x1y2'
type (cx_t), public :: &
  pes_x1y2_pc = cx_null, &
  pes_x1y2_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x1y2_cf(:), pes_x1y2_vcf(:)
!..procedures
CONTAINS
include 'pes_x1y2_read.f90'
include 'pes_x1y2_pot.f90'
include 'pes_x1y2_add.f90'
include 'pes_x1y2_getcf.f90'
include 'pes_x1y2_vread.f90'
include 'pes_x1y2_vfun.f90'
include 'pes_x1y2_vadd.f90'
include 'pes_x1y2_getvcf.f90'
END MODULE pes_x1y2
