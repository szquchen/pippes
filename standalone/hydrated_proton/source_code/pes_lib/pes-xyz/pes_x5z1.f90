MODULE pes_x5z1
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_z1
use pes_x2
use pes_x1z1
use pes_x3
use pes_x2z1
use pes_x4
use pes_x3z1
use pes_x5
use pes_x4z1
private
public :: pes_x5z1_read, pes_x5z1_pot, pes_x5z1_add, &
  pes_x5z1_vread, pes_x5z1_vfun, pes_x5z1_vadd
!..data
save
integer, parameter, public :: &
  pes_x5z1_nki(0:1)=(/5,1/), pes_x5z1_nk=6, &
  pes_x5z1_nb(-1:size(cx_nb51(-1:))-2)=cx_nb51(-1:), &
  pes_x5z1_nvb(-1:size(cxv_nb51(-1:))-2)=cxv_nb51(-1:)
character (len=*), parameter, public :: &
  pes_x5z1_sysall='x1 z1 x2 x1z1 x3 x2z1 x4 x3z1 x5 x4z1 x5z1'
type (cx_t), public :: &
  pes_x5z1_pc = cx_null, &
  pes_x5z1_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x5z1_cf(:), pes_x5z1_vcf(:)
!..procedures
CONTAINS
include 'pes_x5z1_read.f90'
include 'pes_x5z1_pot.f90'
include 'pes_x5z1_add.f90'
include 'pes_x5z1_vread.f90'
include 'pes_x5z1_vfun.f90'
include 'pes_x5z1_vadd.f90'
END MODULE pes_x5z1
