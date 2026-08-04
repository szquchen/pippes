MODULE inv_mg72
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg72_prims, mg72_prib, mg72_isecs, mg72_secs, &
  mg72_base
!..data
integer, parameter, private :: nkk=2, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/7,2/)
integer, parameter, public :: &
  mg72_id=69, &
  mg72_nkk=nkk, mg72_nk=nk, mg72_nr=nr, mg72_ngrp=5040*2, &
  mg72_nkj(0:nkk-1)=nkj, &
  mg72_dnpr(0:9) = (/ 0, 3, 5, 7, 5, 4, 4, 4, 0, 0 /), &
  mg72_npr(0:9) = (/ 0, 3, 8, 15, 20, 24, 28, 32, 32, 32 /), &
  mg72_dnpb(0:9) = (/ 1, 3, 11, 32, 86, 212, 502, 1120, 2414, 5016 /), &
  mg72_npb(0:9) = (/ 1, 4, 15, 47, 133, 345, 847, 1967, 4381, 9397 /), &
  mg72_dnsc(0:9) = (/ 1, 0, 1, 9, 46, 171, 670, 2584, 9905, 36365 /), &
  mg72_nsc(0:9) = (/ 1, 1, 2, 11, 57, 228, 898, 3482, 13387, 49752 /), &
  mg72_dnb(0:9) = (/ 1, 3, 12, 44, 170, 652, 2565, 10053, 39279, 151056 /), &
  mg72_nb(0:9) = (/ 1, 4, 16, 60, 230, 882, 3447, 13500, 52779, 203835 /)
!..procedures
CONTAINS
include 'mg72_prims.f90'
include 'mg72_prib.f90'
include 'mg72_isecs.f90'
include 'mg72_secs.f90'
include 'mg72_base.f90'
END MODULE inv_mg72
