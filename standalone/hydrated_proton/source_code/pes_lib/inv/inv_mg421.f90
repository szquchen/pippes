MODULE inv_mg421
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg421_prims, mg421_prib, mg421_isecs, mg421_secs, &
  mg421_base
!..data
integer, parameter, private :: nkk=3, nk=7, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,2,1/)
integer, parameter, public :: &
  mg421_id=35, &
  mg421_nkk=nkk, mg421_nk=nk, mg421_nr=nr, mg421_ngrp=48, &
  mg421_nkj(0:nkk-1)=nkj, &
  mg421_dnpr(0:9) = (/ 0, 5, 7, 4, 4, 0, 1, 0, 0, 0 /), &
  mg421_npr(0:9) = (/ 0, 5, 12, 16, 20, 20, 21, 21, 21, 21 /), &
  mg421_dnpb(0:9) = (/ 1, 5, 22, 74, 227, 619, 1583, 3775, 8583, 18599 /), &
  mg421_npb(0:9) = (/ 1, 6, 28, 102, 329, 948, 2531, 6306, 14889, 33488 /), &
  mg421_dnsc(0:9) = (/ 1, 0, 4, 23, 71, 199, 548, 1323, 2809, 5307 /), &
  mg421_nsc(0:9) = (/ 1, 1, 5, 28, 99, 298, 846, 2169, 4978, 10285 /), &
  mg421_dnb(0:9) = (/ 1, 5, 26, 117, 501, 1975, 7298, 25167, 81475, &
    248240 /), &
  mg421_nb(0:9) = (/ 1, 6, 32, 149, 650, 2625, 9923, 35090, 116565, &
    364805 /)
!..procedures
CONTAINS
include 'mg421_prims.f90'
include 'mg421_prib.f90'
include 'mg421_isecs.f90'
include 'mg421_secs.f90'
include 'mg421_base.f90'
END MODULE inv_mg421
