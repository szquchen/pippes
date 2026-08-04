MODULE inv_mg332
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg332_prims, mg332_prib, mg332_isecs, mg332_secs, &
  mg332_base
!..data
integer, parameter, private :: nkk=3, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,3,2/)
integer, parameter, public :: &
  mg332_id=54, &
  mg332_nkk=nkk, mg332_nk=nk, mg332_nr=nr, mg332_ngrp=72, &
  mg332_nkj(0:nkk-1)=nkj, &
  mg332_dnpr(0:9) = (/ 0, 6, 11, 7, 1, 0, 3, 0, 0, 0 /), &
  mg332_npr(0:9) = (/ 0, 6, 17, 24, 25, 25, 28, 28, 28, 28 /), &
  mg332_dnpb(0:9) = (/ 1, 6, 32, 129, 466, 1494, 4437, 12252, 31998, &
    79389 /), &
  mg332_npb(0:9) = (/ 1, 7, 39, 168, 634, 2128, 6565, 18817, 50815, &
    130204 /), &
  mg332_dnsc(0:9) = (/ 1, 0, 7, 48, 170, 625, 2294, 6997, 18939, 46563 /), &
  mg332_nsc(0:9) = (/ 1, 1, 8, 56, 226, 851, 3145, 10142, 29081, 75644 /), &
  mg332_dnb(0:9) = (/ 1, 6, 39, 219, 1148, 5578, 25375, 107769, 428943, &
    1603386 /), &
  mg332_nb(0:9) = (/ 1, 7, 46, 265, 1413, 6991, 32366, 140135, 569078, &
    2172464 /)
!..procedures
CONTAINS
include 'mg332_prims.f90'
include 'mg332_prib.f90'
include 'mg332_isecs.f90'
include 'mg332_secs.f90'
include 'mg332_base.f90'
END MODULE inv_mg332
