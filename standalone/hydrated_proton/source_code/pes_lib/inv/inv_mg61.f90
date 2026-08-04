MODULE inv_mg61
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg61_prims, mg61_prib, mg61_isecs, mg61_secs, &
  mg61_base
!..data
integer, parameter, private :: nkk=2, nk=7, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/6,1/)
integer, parameter, public :: &
  mg61_id=31, &
  mg61_nkk=nkk, mg61_nk=nk, mg61_nr=nr, mg61_ngrp=720, &
  mg61_nkj(0:nkk-1)=nkj, &
  mg61_dnpr(0:9) = (/ 0, 2, 3, 5, 4, 4, 3, 0, 0, 0 /), &
  mg61_npr(0:9) = (/ 0, 2, 5, 10, 14, 18, 21, 21, 21, 21 /), &
  mg61_dnpb(0:9) = (/ 1, 2, 6, 15, 34, 72, 150, 290, 553, 1016 /), &
  mg61_npb(0:9) = (/ 1, 3, 9, 24, 58, 130, 280, 570, 1123, 2139 /), &
  mg61_dnsc(0:9) = (/ 1, 0, 1, 6, 24, 74, 216, 590, 1581, 3958 /), &
  mg61_nsc(0:9) = (/ 1, 1, 2, 8, 32, 106, 322, 912, 2493, 6451 /), &
  mg61_dnb(0:9) = (/ 1, 2, 7, 23, 76, 245, 782, 2392, 7118, 20350 /), &
  mg61_nb(0:9) = (/ 1, 3, 10, 33, 109, 354, 1136, 3528, 10646, 30996 /)
!..procedures
CONTAINS
include 'mg61_prims.f90'
include 'mg61_prib.f90'
include 'mg61_isecs.f90'
include 'mg61_secs.f90'
include 'mg61_base.f90'
END MODULE inv_mg61
