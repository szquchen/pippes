MODULE pes_x2z1
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_z1
use pes_x2
use pes_x1z1
private
public :: pes_x2z1_read, pes_x2z1_pot, pes_x2z1_add, &
  pes_x2z1_vread, pes_x2z1_vfun, pes_x2z1_vadd
!..data
save
integer, parameter, public :: &
  pes_x2z1_nki(0:1)=(/2,1/), pes_x2z1_nk=3, &
  pes_x2z1_nb(-1:size(cx_nb21(-1:))-2)=cx_nb21(-1:), &
  pes_x2z1_nvb(-1:size(cxv_nb21(-1:))-2)=cxv_nb21(-1:)
character (len=*), parameter, public :: &
  pes_x2z1_sysall='x1 x2 z1 x1z1 x2z1'
type (cx_t), public :: &
  pes_x2z1_pc = cx_null, &
  pes_x2z1_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x2z1_cf(:), pes_x2z1_vcf(:)
!..procedures
CONTAINS
include 'pes_x2z1_read.f90'
include 'pes_x2z1_pot.f90'
include 'pes_x2z1_add.f90'
include 'pes_x2z1_vread.f90'
include 'pes_x2z1_vfun.f90'
include 'pes_x2z1_vadd.f90'
END MODULE pes_x2z1
