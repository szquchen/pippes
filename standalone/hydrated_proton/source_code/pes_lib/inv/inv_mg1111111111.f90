MODULE inv_mg1111111111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg1111111111_prims, mg1111111111_prib, &
  mg1111111111_secs, mg1111111111_base
!..data
integer, parameter, private :: nkk=10, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/1,1,1,1,1,1,1,1,1,1/)
integer, parameter, public :: &
  mg1111111111_id=138, &
  mg1111111111_nkk=nkk, mg1111111111_nk=nk, mg1111111111_nr=nr, &
  mg1111111111_ngrp=1, &
  mg1111111111_nkj(0:nkk-1)=nkj, &
  mg1111111111_dnpr(0:7) = (/ 0, 45, 0, 0, 0, 0, 0, 0 /), &
  mg1111111111_npr(0:7) = (/ 0, 45, 45, 45, 45, 45, 45, 45 /), &
  mg1111111111_dnpb(0:7) = (/ 1, 45, 1035, 16215, 194580, 1906884, &
    15890700, 115775100 /), &
  mg1111111111_npb(0:7) = (/ 1, 46, 1081, 17296, 211876, 2118760, &
    18009460, 133784560 /), &
  mg1111111111_dnsc(0:7) = (/ 1, 0, 0, 0, 0, 0, 0, 0 /), &
  mg1111111111_nsc(0:7) = (/ 1, 1, 1, 1, 1, 1, 1, 1 /), &
  mg1111111111_dnb(0:7) = (/ 1, 45, 1035, 16215, 194580, 1906884, &
    15890700, 115775100 /), &
  mg1111111111_nb(0:7) = (/ 1, 46, 1081, 17296, 211876, 2118760, &
    18009460, 133784560 /)
!..procedures
CONTAINS
include 'mg1111111111_prims.f90'
include 'mg1111111111_prib.f90'
include 'mg1111111111_secs.f90'
include 'mg1111111111_base.f90'
END MODULE inv_mg1111111111
