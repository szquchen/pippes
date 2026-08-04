MODULE inv_mg6
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg6_prims, mg6_prib, mg6_isecs, mg6_secs, &
  mg6_base
!..data
integer, parameter, private :: nkk=1, nk=6, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/6/)
integer, parameter, public :: &
  mg6_id=19, &
  mg6_nkk=nkk, mg6_nk=nk, mg6_nr=nr, mg6_ngrp=720, &
  mg6_nkj(0:nkk-1)=nkj, &
  mg6_dnpr(0:9) = (/ 0, 1, 2, 4, 3, 3, 2, 0, 0, 0 /), &
  mg6_npr(0:9) = (/ 0, 1, 3, 7, 10, 13, 15, 15, 15, 15 /), &
  mg6_dnpb(0:9) = (/ 1, 1, 3, 7, 13, 24, 46, 76, 132, 218 /), &
  mg6_npb(0:9) = (/ 1, 2, 5, 12, 25, 49, 95, 171, 303, 521 /), &
  mg6_dnsc(0:9) = (/ 1, 0, 0, 1, 7, 18, 40, 81, 167, 328 /), &
  mg6_nsc(0:9) = (/ 1, 1, 1, 2, 9, 27, 67, 148, 315, 643 /), &
  mg6_dnb(0:9) = (/ 1, 1, 3, 8, 21, 52, 132, 313, 741, 1684 /), &
  mg6_nb(0:9) = (/ 1, 2, 5, 13, 34, 86, 218, 531, 1272, 2956 /)
!..procedures
CONTAINS
include 'mg6_prims.f90'
include 'mg6_prib.f90'
include 'mg6_isecs.f90'
include 'mg6_secs.f90'
include 'mg6_base.f90'
END MODULE inv_mg6
