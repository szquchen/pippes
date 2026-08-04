MODULE inv_mg41
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg41_prims, mg41_prib, mg41_isecs, mg41_secs, mg41_base
!..data
integer, parameter, private :: nkk=2, nk=5, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,1/)
integer, parameter, public :: &
  mg41_id=13, &
  mg41_nkk=nkk, mg41_nk=nk, mg41_nr=nr, mg41_ngrp=24, &
  mg41_nkj(0:nkk-1)=nkj, &
  mg41_dnpr(0:9) = (/ 0, 2, 3, 3, 2, 0, 0, 0, 0, 0 /), &
  mg41_npr(0:9) = (/ 0, 2, 5, 8, 10, 10, 10, 10, 10, 10 /), &
  mg41_dnpb(0:9) = (/ 1, 2, 6, 13, 28, 52, 98, 168, 286, 462 /), &
  mg41_npb(0:9) = (/ 1, 3, 9, 22, 50, 102, 200, 368, 654, 1116 /), &
  mg41_dnsc(0:9) = (/ 1, 0, 1, 5, 9, 12, 18, 21, 24, 26 /), &
  mg41_nsc(0:9) = (/ 1, 1, 2, 7, 16, 28, 46, 67, 91, 117 /), &
  mg41_dnb(0:9) = (/ 1, 2, 7, 20, 53, 125, 287, 606, 1226, 2358 /), &
  mg41_nb(0:9) = (/ 1, 3, 10, 30, 83, 208, 495, 1101, 2327, 4685 /)
!..procedures
CONTAINS
include 'mg41_prims.f90'
include 'mg41_prib.f90'
include 'mg41_isecs.f90'
include 'mg41_secs.f90'
include 'mg41_base.f90'
END MODULE inv_mg41
