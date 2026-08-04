MODULE pes_x4y4
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_y1
use pes_x2
use pes_x1y1
use pes_y2
private
public :: pes_x4y4_read, pes_x4y4_pot, pes_x4y4_add, &
  pes_x4y4_getcf, pes_x4y4_vread, pes_x4y4_vfun, pes_x4y4_vadd, &
  pes_x4y4_getvcf
!..data
save
integer, parameter, public :: &
  pes_x4y4_nki(0:1)=(/4,4/), pes_x4y4_nk=8, &
  pes_x4y4_nb(-1:size(cx_nb44(-1:))-2)=cx_nb44(-1:), &
  pes_x4y4_nvb(-1:size(cxv_nb44(-1:))-2)=cxv_nb44(-1:)
character (len=*), parameter, public :: &
!!   pes_x4y4_sysall='x1 y1 x2 x1y1 y2 '// &
!!     'x3 x2y1 x1y2 y3 '// &
!!     'x4 x3y1 x2y2 x1y3 y4 '// &
!!     'x4y1 x3y2 x2y3 x1y4 '// &
!!     'x4y2 x3y3 x2y4 '// &
!!     'x4y3 x3y4 '// &
!!     'x4y4'
  pes_x4y4_sysall='x1 y1 x2 x1y1 y2 x4y4'
type (cx_t), public :: &
  pes_x4y4_pc = cx_null, &
  pes_x4y4_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x4y4_cf(:), pes_x4y4_vcf(:)
!..procedures
CONTAINS
include 'pes_x4y4_read.f90'
include 'pes_x4y4_pot.f90'
include 'pes_x4y4_add.f90'
include 'pes_x4y4_getcf.f90'
include 'pes_x4y4_vread.f90'
include 'pes_x4y4_vfun.f90'
include 'pes_x4y4_vadd.f90'
include 'pes_x4y4_getvcf.f90'
END MODULE pes_x4y4
