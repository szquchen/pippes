MODULE pes_x4y3z2
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
use pes_x4y1
use pes_x3y2
private
public :: pes_x4y3z2_read, pes_x4y3z2_pot, pes_x4y3z2_add, &
  pes_x4y3z2_getcf, pes_x4y3z2_vread, pes_x4y3z2_vfun, pes_x4y3z2_vadd, &
  pes_x4y3z2_getvcf
!..data
save
integer, parameter, public :: &
  pes_x4y3z2_nki(0:1)=(/4,3,2/), pes_x4y3z2_nk=9, &
  pes_x4y3z2_nb(-1:size(cx_nb432(-1:))-2)=cx_nb432(-1:), &
  pes_x4y3z2_nvb(-1:size(cxv_nb432(-1:))-2)=cxv_nb432(-1:)
character (len=*), parameter, public :: &
  pes_x4y3z2_sysall='x1 y1 z1 x2 x1y1 y2 x1z1 y1z1 z2 '//
    'x3 x2y1 x1y2 x2z1 x1y1z1 y2z1 x1z2 y1z2 '//
    'x4 x3y1 x2y2 x1y3 x3z1 x2y1z1 x1y2z1 y3z1 x2z2 x1y1z2 y2z2 '// &
    'x4y1 x3y2 x2y3 x4z1 x3y1z1 x2y2z1 x1y3z1 x3z2 x2y1z2 x1y2z2 '//
    'x4y2 x3y3 x4y1z1 x3y2z1 x2y3z1 x4z2 x3y1z2 x2y2z2 x1y3z2 '//
    'x4y3 x4y2z1 x3y3z1 x4y1z2 x3y2z2 x2y3z2 '//
    'x4y3z1 x4y2z2 x3y3z2 x4y3z2'
type (cx_t), public :: &
  pes_x4y3z2_pc = cx_null, &
  pes_x4y3z2_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x4y3z2_cf(:), pes_x4y3z2_vcf(:)
!..procedures
CONTAINS
include 'pes_x4y3z2_read.f90'
include 'pes_x4y3z2_pot.f90'
include 'pes_x4y3z2_add.f90'
include 'pes_x4y3z2_getcf.f90'
include 'pes_x4y3z2_vread.f90'
include 'pes_x4y3z2_vfun.f90'
include 'pes_x4y3z2_vadd.f90'
include 'pes_x4y3z2_getvcf.f90'
END MODULE pes_x4y3z2
