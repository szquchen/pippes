MODULE inv_mg73
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg73_prims, mg73_prib, mg73_isecs, mg73_secs, &
  mg73_base
!..data
integer, parameter, private :: nkk=2, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/7,3/)
integer, parameter, public :: &
  mg73_id=100, &
  mg73_nkk=nkk, mg73_nk=nk, mg73_nr=nr, mg73_ngrp=5040*6, &
  mg73_nkj(0:nkk-1)=nkj, &
  mg73_dnpr(0:9) = (/ 0, 3, 6, 9, 7, 5, 4, 4, 0, 0 /), &
  mg73_npr(0:9) = (/ 0, 3, 9, 18, 25, 30, 34, 38, 38, 38 /), &
  mg73_dnpb(0:9) = (/ 1, 3, 12, 37, 106, 278, 700, 1658, 3792, &
    8349 /), &
  mg73_npb(0:9) = (/ 1, 4, 16, 53, 159, 437, 1137, 2795, 6587, &
    14936 /), &
  mg73_dnsc(0:9) = (/ 1, 0, 2, 16, 74, 314, 1395, 6133, 26939, &
    114976 /), &
  mg73_nsc(0:9) = (/ 1, 1, 3, 19, 93, 407, 1802, 7935, 34874, &
    149850 /), &
  mg73_dnb(0:9) = (/ 1, 3, 14, 59, 252, 1080, 4729, 20734, 91180, &
    397725 /), &
  mg73_nb(0:9) = (/ 1, 4, 18, 77, 329, 1409, 6138, 26872, 118052, &
    515777 /)
!..procedures
CONTAINS
include 'mg73_prims.f90'
include 'mg73_prib.f90'
include 'mg73_isecs.f90'
include 'mg73_secs.f90'
include 'mg73_base.f90'
END MODULE inv_mg73
