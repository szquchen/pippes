MODULE pes_x1y1
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_y1
private
public :: pes_x1y1_read, pes_x1y1_pot, pes_x1y1_add, pes_x1y1_getcf, &
  pes_x1y1_vread, pes_x1y1_vfun, pes_x1y1_vadd, pes_x1y1_getvcf
!..data
save
integer, parameter, public :: &
  pes_x1y1_nki(0:1)=(/1,1/), pes_x1y1_nk=2, &
  pes_x1y1_nb(-1:size(cx_nb11(-1:))-2)=cx_nb11(-1:), &
  pes_x1y1_nvb(-1:size(cxv_nb11(-1:))-2)=cxv_nb11(-1:)
character (len=*), parameter, public :: &
  pes_x1y1_sysall='x1 y1 x1y1'
type (cx_t), public :: &
  pes_x1y1_pc = cx_null, &
  pes_x1y1_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x1y1_cf(:), &
  pes_x1y1_vcf(:)
!..procedures
CONTAINS
include 'pes_x1y1_read.f90'
include 'pes_x1y1_pot.f90'
include 'pes_x1y1_add.f90'
include 'pes_x1y1_getcf.f90'
include 'pes_x1y1_vread.f90'
include 'pes_x1y1_vfun.f90'
include 'pes_x1y1_vadd.f90'
include 'pes_x1y1_getvcf.f90'
END MODULE pes_x1y1
