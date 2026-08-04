MODULE pes_x3y2z1u1
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_y1
use pes_z1
use pes_u1
use pes_x2
use pes_x1y1
use pes_y2
use pes_x1z1
use pes_y1z1
use pes_x1u1
use pes_y1u1
use pes_z1u1
use pes_x3
use pes_x2y1
use pes_x1y2
use pes_x2z1
use pes_x1y1z1
use pes_y2z1
use pes_x2u1
use pes_x1y1u1
use pes_y2u1
use pes_x1z1u1
use pes_y1z1u1
use pes_x3y1
use pes_x2y2
use pes_x3z1
use pes_x2y1z1
use pes_x1y2z1
use pes_x3u1
use pes_x2y1u1
use pes_x1y2u1
use pes_x2z1u1
use pes_x1y1z1u1
use pes_y2z1u1
use pes_x3y2
use pes_x3y1z1
use pes_x2y2z1
use pes_x3y1u1
use pes_x2y2u1
use pes_x3z1u1
use pes_x2y1z1u1
use pes_x1y2z1u1
use pes_x3y2z1
use pes_x3y2u1
use pes_x3y1z1u1
use pes_x2y2z1u1
private
public :: pes_x3y2z1u1_read, pes_x3y2z1u1_pot, pes_x3y2z1u1_add, &
  pes_x3y2z1u1_getcf, pes_x3y2z1u1_vread, pes_x3y2z1u1_vfun, &
  pes_x3y2z1u1_vadd, pes_x3y2z1u1_getvcf
!..data
save
integer, parameter, public :: &
  pes_x3y2z1u1_nki(0:3)=(/3,2,1,1/), pes_x3y2z1u1_nk=7, &
  pes_x3y2z1u1_nb(-1:size(cx_nb3211(-1:))-2)=cx_nb3211(-1:), &
  pes_x3y2z1u1_nvb(-1:size(cxv_nb3211(-1:))-2)=cxv_nb3211(-1:)
character (len=*), parameter, public :: &
  pes_x3y2z1u1_sysall='x1 y1 z1 u1 '// &
    'x2 x1y1 y2 x1z1 y1z1 x1u1 y1u1 z1u1 '// &
    'x3 x2y1 x1y2 x2z1 x1y1z1 y2z1 x2u1 x1y1u1 y2u1 x1z1u1 y1z1u1 '// &
    'x3y1 x2y2 x3z1 x2y1z1 x1y2z1 x3u1 x2y1u1 x1y2u1 '// &
    'x2z1u1 x1y1z1u1 y2z1u1 '// &
    'x3y2 x3y1z1 x2y2z1 x3y1u1 x2y2u1 x3z1u1 x2y1z1u1 x1y2z1u1 '// &
    'x3y2z1 x3y2u1 x3y1z1u1 x2y2z1u1 '// &
    'x3y2z1u1'
type (cx_t), public :: &
  pes_x3y2z1u1_pc = cx_null, &
  pes_x3y2z1u1_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x3y2z1u1_cf(:), &
  pes_x3y2z1u1_vcf(:)
!..procedures
CONTAINS
include 'pes_x3y2z1u1_read.f90'
include 'pes_x3y2z1u1_pot.f90'
include 'pes_x3y2z1u1_add.f90'
include 'pes_x3y2z1u1_getcf.f90'
include 'pes_x3y2z1u1_vread.f90'
include 'pes_x3y2z1u1_vfun.f90'
include 'pes_x3y2z1u1_vadd.f90'
include 'pes_x3y2z1u1_getvcf.f90'
END MODULE pes_x3y2z1u1
