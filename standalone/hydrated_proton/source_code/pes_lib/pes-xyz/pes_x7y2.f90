MODULE pes_x7y2
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
use pes_x4
use pes_x3y1
use pes_x2y2
use pes_x5
use pes_x4y1
use pes_x3y2
use pes_x6
use pes_x5y1
use pes_x4y2
use pes_x7
use pes_x6y1
use pes_x5y2
use pes_x7y1
use pes_x6y2
private
public :: pes_x7y2_read, pes_x7y2_pot, pes_x7y2_add, pes_x7y2_getcf, &
  pes_x7y2_vread, pes_x7y2_vfun, pes_x7y2_vadd, pes_x7y2_getvcf
!..data
save
integer, parameter, public :: &
  pes_x7y2_nki(0:1)=(/7,2/), pes_x7y2_nk=9, &
  pes_x7y2_nb(-1:size(cx_nb72(-1:))-2)=cx_nb72(-1:), &
  pes_x7y2_nvb(-1:size(cxv_nb72(-1:))-2)=cxv_nb72(-1:)
character (len=*), parameter, public :: &
  pes_x7y2_sysall='x1 y1 x2 x1y1 y2 x3 x2y1 x1y2 x4 x3y1 x2y2 '// &
    'x5 x4y1 x3y2 x6 x5y1 x4y2 x7 x6y1 x5y2 x7y1 x6y2 x7y2'
type (cx_t), public :: &
  pes_x7y2_pc = cx_null, &
  pes_x7y2_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x7y2_cf(:), pes_x7y2_vcf(:)
!..procedures
CONTAINS
include 'pes_x7y2_read.f90'
include 'pes_x7y2_pot.f90'
include 'pes_x7y2_add.f90'
include 'pes_x7y2_getcf.f90'
include 'pes_x7y2_vread.f90'
include 'pes_x7y2_vfun.f90'
include 'pes_x7y2_vadd.f90'
include 'pes_x7y2_getvcf.f90'
END MODULE pes_x7y2
