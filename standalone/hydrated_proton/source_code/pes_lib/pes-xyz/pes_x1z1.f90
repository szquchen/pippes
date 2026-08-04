MODULE pes_x1z1
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_z1
private
public :: pes_x1z1_read, pes_x1z1_pot, pes_x1z1_add, &
  pes_x1z1_vread, pes_x1z1_vfun, pes_x1z1_vadd
!..data
save
integer, parameter, public :: &
  pes_x1z1_nki(0:1)=(/1,1/), pes_x1z1_nk=2, &
  pes_x1z1_nb(-1:size(cx_nb11(-1:))-2)=cx_nb11(-1:), &
  pes_x1z1_nvb(-1:size(cxv_nb11(-1:))-2)=cxv_nb11(-1:)
character (len=*), parameter, public :: &
  pes_x1z1_sysall='x1 z1 x1z1'
type (cx_t), public :: &
  pes_x1z1_pc = cx_null, &
  pes_x1z1_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x1z1_cf(:), &
  pes_x1z1_vcf(:)
!..procedures
CONTAINS
include 'pes_x1z1_read.f90'
include 'pes_x1z1_pot.f90'
include 'pes_x1z1_add.f90'
include 'pes_x1z1_vread.f90'
include 'pes_x1z1_vfun.f90'
include 'pes_x1z1_vadd.f90'
END MODULE pes_x1z1
