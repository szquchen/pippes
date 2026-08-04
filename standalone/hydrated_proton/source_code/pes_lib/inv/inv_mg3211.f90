MODULE inv_mg3211
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg3211_prims, mg3211_prib, mg3211_isecs, mg3211_secs, &
  mg3211_base
!..data
integer, parameter, private :: nkk=4, nk=7, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,2,1,1/)
integer, parameter, public :: &
  mg3211_id=39, &
  mg3211_nkk=nkk, mg3211_nk=nk, mg3211_nr=nr, mg3211_ngrp=12, &
  mg3211_nkj(0:nkk-1)=nkj, &
  mg3211_dnpr(0:9) = (/ 0, 8, 8, 4, 0, 0, 1, 0, 0, 0 /), &
  mg3211_npr(0:9) = (/ 0, 8, 16, 20, 20, 20, 21, 21, 21, 21 /), &
  mg3211_dnpb(0:9) = (/ 1, 8, 44, 188, 686, 2216, 6519, 17752, 45337, &
    109584 /), &
  mg3211_npb(0:9) = (/ 1, 9, 53, 241, 927, 3143, 9662, 27414, 72751, &
    182335 /), &
  mg3211_dnsc(0:9) = (/ 1, 0, 9, 32, 82, 200, 442, 716, 1000, 1264 /), &
  mg3211_nsc(0:9) = (/ 1, 1, 10, 42, 124, 324, 766, 1482, 2482, 3746 /), &
  mg3211_dnb(0:9) = (/ 1, 8, 53, 292, 1420, 6172, 24359, 88116, 294948, &
    920736 /), &
  mg3211_nb(0:9) = (/ 1, 9, 62, 354, 1774, 7946, 32305, 120421, 415369, &
    1336105 /)
!..procedures
CONTAINS
include 'mg3211_prims.f90'
include 'mg3211_prib.f90'
include 'mg3211_isecs.f90'
include 'mg3211_secs.f90'
include 'mg3211_base.f90'
END MODULE inv_mg3211
