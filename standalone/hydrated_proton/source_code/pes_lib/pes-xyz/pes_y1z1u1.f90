MODULE pes_y1z1u1
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_y1
use pes_z1
use pes_u1
use pes_y1z1
use pes_y1u1
use pes_z1u1
private
public :: pes_y1z1u1_read, pes_y1z1u1_pot, pes_y1z1u1_add, &
  pes_y1z1u1_getcf, pes_y1z1u1_vread, pes_y1z1u1_vfun, &
  pes_y1z1u1_vadd, pes_y1z1u1_getvcf
!..data
save
integer, parameter, public :: &
  pes_y1z1u1_nki(0:2)=(/1,1,1/), pes_y1z1u1_nk=3, &
  pes_y1z1u1_nb(-1:size(cx_nb111(-1:))-2)=cx_nb111(-1:), &
  pes_y1z1u1_nvb(-1:size(cxv_nb111(-1:))-2)=cxv_nb111(-1:)
character (len=*), parameter, public :: &
  pes_y1z1u1_sysall='y1 z1 u1 y1z1 y1u1 z1u1 y1z1u1'
type (cx_t), public :: &
  pes_y1z1u1_pc = cx_null, &
  pes_y1z1u1_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_y1z1u1_cf(:), pes_y1z1u1_vcf(:)
!..procedures
CONTAINS
include 'pes_y1z1u1_read.f90'
include 'pes_y1z1u1_pot.f90'
include 'pes_y1z1u1_add.f90'
include 'pes_y1z1u1_getcf.f90'
include 'pes_y1z1u1_vread.f90'
include 'pes_y1z1u1_vfun.f90'
include 'pes_y1z1u1_vadd.f90'
include 'pes_y1z1u1_getvcf.f90'
END MODULE pes_y1z1u1
