MODULE inv_mg5
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg5_prims, mg5_prib, mg5_isecs, mg5_secs, &
  mg5_base
!..data
integer, parameter, private :: nkk=1, nk=5, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/5/)
integer, parameter, public :: &
  mg5_id=12, &
  mg5_nkk=nkk, mg5_nk=nk, mg5_nr=nr, mg5_ngrp=120, &
  mg5_nkj(0:nkk-1)=nkj, &
  mg5_dnpr(0:9) = (/ 0, 1, 2, 2, 2, 2, 1, 0, 0, 0 /), &
  mg5_npr(0:9) = (/ 0, 1, 3, 5, 7, 9, 10, 10, 10, 10 /), &
  mg5_dnpb(0:9) = (/ 1, 1, 3, 5, 10, 16, 28, 42, 68, 100 /), &
  mg5_npb(0:9) = (/ 1, 2, 5, 10, 20, 36, 64, 106, 174, 274 /), &
  mg5_dnsc(0:9) = (/ 1, 0, 0, 2, 5, 8, 15, 23, 33, 46 /), &
  mg5_nsc(0:9) = (/ 1, 1, 1, 3, 8, 16, 31, 54, 87, 133 /), &
  mg5_dnb(0:9) = (/ 1, 1, 3, 7, 17, 35, 76, 149, 291, 539 /), &
  mg5_nb(0:9) = (/ 1, 2, 5, 12, 29, 64, 140, 289, 580, 1119 /)
!..procedures
CONTAINS
include 'mg5_prims.f90'
include 'mg5_prib.f90'
include 'mg5_isecs.f90'
include 'mg5_secs.f90'
include 'mg5_base.f90'
END MODULE inv_mg5
