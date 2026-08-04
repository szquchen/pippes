MODULE pes_x5y1z1
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_y1
use pes_z1
use pes_x2
use pes_x1y1
use pes_x1z1
use pes_y1z1
use pes_x3
use pes_x2y1
use pes_x2z1
use pes_x1y1z1
use pes_x4
use pes_x3y1
use pes_x3z1
use pes_x2y1z1
use pes_x5
use pes_x4y1
use pes_x4z1
use pes_x3y1z1
use pes_x5y1
use pes_x5z1
use pes_x4y1z1
private
public :: pes_x5y1z1_read, pes_x5y1z1_pot, pes_x5y1z1_add, &
  pes_x5y1z1_getcf, pes_x5y1z1_vread, pes_x5y1z1_vfun, &
  pes_x5y1z1_vadd, pes_x5y1z1_getvcf
!..data
save
integer, parameter, public :: &
  pes_x5y1z1_nki(0:2)=(/5,1,1/), pes_x5y1z1_nk=7, &
  pes_x5y1z1_nb(-1:size(cx_nb511(-1:))-2)=cx_nb511(-1:), &
  pes_x5y1z1_nvb(-1:size(cxv_nb511(-1:))-2)=cxv_nb511(-1:)
character (len=*), parameter, public :: &
  pes_x5y1z1_sysall='x1 y1 z1 x2 x1y1 x1z1 y1z1 x3 x2y1 x2z1 x1y1z1 '// &
    'x4 x3y1 x3z1 x2y1z1 x5 x4y1 x4z1 x3y1z1 x5y1 x5z1 x4y1z1 x5y1z1'
type (cx_t), public :: &
  pes_x5y1z1_pc = cx_null, &
  pes_x5y1z1_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x5y1z1_cf(:), &
  pes_x5y1z1_vcf(:)
!..procedures
CONTAINS
include 'pes_x5y1z1_read.f90'
include 'pes_x5y1z1_pot.f90'
include 'pes_x5y1z1_add.f90'
include 'pes_x5y1z1_getcf.f90'
include 'pes_x5y1z1_vread.f90'
include 'pes_x5y1z1_vfun.f90'
include 'pes_x5y1z1_vadd.f90'
include 'pes_x5y1z1_getvcf.f90'
END MODULE pes_x5y1z1
