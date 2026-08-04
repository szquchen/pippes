MODULE pes_x4y4z1
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_y1
use pes_z1
use pes_x2
use pes_x1y1
use pes_y2
use pes_x1z1
use pes_y1z1
private
public :: pes_x4y4z1_read, pes_x4y4z1_pot, pes_x4y4z1_add, &
  pes_x4y4z1_getcf, pes_x4y4z1_vread, pes_x4y4z1_vfun, pes_x4y4z1_vadd, &
  pes_x4y4z1_getvcf
!..data
save
integer, parameter, public :: &
  pes_x4y4z1_nki(0:2)=(/4,4,1/), pes_x4y4z1_nk=9, &
  pes_x4y4z1_nb(-1:size(cx_nb441(-1:))-2)=cx_nb441(-1:), &
  pes_x4y4z1_nvb(-1:size(cxv_nb441(-1:))-2)=cxv_nb441(-1:)
character (len=*), parameter, public :: &
!!   pes_x4y4z1_sysall='x1 y1 z1 x2 x1y1 y2 x1z1 y1z1 '// &
!!     'x3 x2y1 x1y2 y3 x2z1 x1y1z1 y2z1 '// &
!!     'x4 x3y1 x2y2 x1y3 y4 x3z1 x2y1z1 x1y2z1 y3z1 '// &
!!     'x4y1 x3y2 x2y3 x1y4 x4z1 x3y1z1 x2y2z1 x1y3z1 y4z1 '// &
!!     'x4y2 x3y3 x2y4 x4y1z1 x3y2z1 x2y3z1 x1y4z1 '// &
!!     'x4y3 x3y4 x4y2z1 x3y3z1 x2y4z1 '// &
!!     'x4y4 x4y3z1 x3y4z1 '// &
!!     'x4y4z1'
  pes_x4y4z1_sysall='x1 y1 z1 x2 x1y1 y2 x1z1 y1z1 x4y4z1'
type (cx_t), public :: &
  pes_x4y4z1_pc = cx_null, &
  pes_x4y4z1_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x4y4z1_cf(:), pes_x4y4z1_vcf(:)
!..procedures
CONTAINS
include 'pes_x4y4z1_read.f90'
include 'pes_x4y4z1_pot.f90'
include 'pes_x4y4z1_add.f90'
include 'pes_x4y4z1_getcf.f90'
include 'pes_x4y4z1_vread.f90'
include 'pes_x4y4z1_vfun.f90'
include 'pes_x4y4z1_vadd.f90'
include 'pes_x4y4z1_getvcf.f90'
END MODULE pes_x4y4z1
