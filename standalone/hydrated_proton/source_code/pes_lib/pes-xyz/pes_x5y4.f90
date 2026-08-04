MODULE pes_x5y4
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_y1
use pes_x2
use pes_x1y1
use pes_y2
private
public :: pes_x5y4_read, pes_x5y4_pot, pes_x5y4_add, &
  pes_x5y4_getcf, pes_x5y4_vread, pes_x5y4_vfun, pes_x5y4_vadd, &
  pes_x5y4_getvcf
!..data
save
integer, parameter, public :: &
  pes_x5y4_nki(0:1)=(/5,4/), pes_x5y4_nk=9, &
  pes_x5y4_nb(-1:size(cx_nb54(-1:))-2)=cx_nb54(-1:), &
  pes_x5y4_nvb(-1:size(cxv_nb54(-1:))-2)=cxv_nb54(-1:)
character (len=*), parameter, public :: &
  pes_x5y4_sysall='x1 y1 x2 x1y1 y2 x5y4'
type (cx_t), public :: &
  pes_x5y4_pc = cx_null, &
  pes_x5y4_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x5y4_cf(:), pes_x5y4_vcf(:)
!..procedures
CONTAINS
include 'pes_x5y4_read.f90'
include 'pes_x5y4_pot.f90'
include 'pes_x5y4_add.f90'
include 'pes_x5y4_getcf.f90'
include 'pes_x5y4_vread.f90'
include 'pes_x5y4_vfun.f90'
include 'pes_x5y4_vadd.f90'
include 'pes_x5y4_getvcf.f90'
END MODULE pes_x5y4
