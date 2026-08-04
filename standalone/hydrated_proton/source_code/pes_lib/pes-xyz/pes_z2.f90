MODULE pes_z2
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_z1
private
public :: pes_z2_read, pes_z2_pot, pes_z2_add, pes_z2_getcf, &
  pes_z2_vread, pes_z2_vfun, pes_z2_vadd, pes_z2_getvcf
!..data
save
integer, parameter, public :: &
  pes_z2_nki(0:0)=(/2/), pes_z2_nk=2, &
  pes_z2_nb(-1:size(cx_nb2(-1:))-2)=cx_nb2(-1:), &
  pes_z2_nvb(-1:size(cxv_nb2(-1:))-2)=cxv_nb2(-1:)
character (len=*), parameter, public :: &
  pes_z2_sysall='z1 z2'
type (cx_t), public :: &
  pes_z2_pc = cx_null, &
  pes_z2_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_z2_cf(:), pes_z2_vcf(:)
!..procedures
CONTAINS
include 'pes_z2_read.f90'
include 'pes_z2_pot.f90'
include 'pes_z2_add.f90'
include 'pes_z2_getcf.f90'
include 'pes_z2_vread.f90'
include 'pes_z2_vfun.f90'
include 'pes_z2_vadd.f90'
include 'pes_z2_getvcf.f90'
END MODULE pes_z2
