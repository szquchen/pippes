MODULE inv_mg311
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg311_prims, mg311_prib, mg311_isecs, mg311_secs, &
  mg311_base
!..data
integer, parameter, private :: nkk=3, nk=5, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,1,1/)
integer, parameter, public :: &
  mg311_id=15, &
  mg311_nkk=nkk, mg311_nk=nk, mg311_nr=nr, mg311_ngrp=6, &
  mg311_nkj(0:nkk-1)=nkj, &
  mg311_dnpr(0:9) = (/ 0, 4, 3, 3, 0, 0, 0, 0, 0, 0 /), &
  mg311_npr(0:9) = (/ 0, 4, 7, 10, 10, 10, 10, 10, 10, 10 /), &
  mg311_dnpb(0:9) = (/ 1, 4, 13, 35, 83, 179, 361, 685, 1240, 2155 /), &
  mg311_npb(0:9) = (/ 1, 5, 18, 53, 136, 315, 676, 1361, 2601, 4756 /), &
  mg311_dnsc(0:9) = (/ 1, 0, 3, 7, 6, 6, 10, 3, 0, 0 /), &
  mg311_nsc(0:9) = (/ 1, 1, 4, 11, 17, 23, 33, 36, 36, 36 /), &
  mg311_dnb(0:9) = (/ 1, 4, 16, 54, 156, 405, 967, 2134, 4426, 8698 /), &
  mg311_nb(0:9) = (/ 1, 5, 21, 75, 231, 636, 1603, 3737, 8163, 16861 /)
!..procedures
CONTAINS
include 'mg311_prims.f90'
include 'mg311_prib.f90'
include 'mg311_isecs.f90'
include 'mg311_secs.f90'
include 'mg311_base.f90'
END MODULE inv_mg311
