MODULE pes_x5
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_x2
use pes_x3
use pes_x4
private
public :: pes_x5_read, pes_x5_pot, pes_x5_add, pes_x5_getcf, &
  pes_x5_vread, pes_x5_vfun, pes_x5_vadd, pes_x5_getvcf
!..data
save
integer, parameter, public :: &
  pes_x5_nki(0:0)=(/5/), pes_x5_nk=5, &
  pes_x5_nb(-1:size(cx_nb5(-1:))-2)=cx_nb5(-1:), &
  pes_x5_nvb(-1:size(cxv_nb5(-1:))-2)=cxv_nb5(-1:)
character (len=*), parameter, public :: &
  pes_x5_sysall='x1 x2 x3 x4 x5'
type (cx_t), public :: &
  pes_x5_pc = cx_null, &
  pes_x5_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x5_cf(:), pes_x5_vcf(:)
!..procedures
CONTAINS
include 'pes_x5_read.f90'
include 'pes_x5_pot.f90'
include 'pes_x5_add.f90'
include 'pes_x5_getcf.f90'
include 'pes_x5_vread.f90'
include 'pes_x5_vfun.f90'
include 'pes_x5_vadd.f90'
include 'pes_x5_getvcf.f90'
END MODULE pes_x5
