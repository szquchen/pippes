MODULE inv_mg22211
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg22211_prims, mg22211_prib, mg22211_isecs, mg22211_secs, &
  mg22211_base
!..data
integer, parameter, private :: nkk=5, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,2,2,1,1/)
integer, parameter, public :: &
  mg22211_id=62, &
  mg22211_nkk=nkk, mg22211_nk=nk, mg22211_nr=nr, mg22211_ngrp=8, &
  mg22211_nkj(0:nkk-1)=nkj, &
  mg22211_dnpr(0:9) = (/ 0, 13, 15, 0, 0, 0, 0, 0, 0, 0 /), &
  mg22211_npr(0:9) = (/ 0, 13, 28, 28, 28, 28, 28, 28, 28, 28 /), &
  mg22211_dnpb(0:9) = (/ 1, 13, 106, 650, 3305, 14573, 57464, 206648, &
    687770, 2141490 /), &
  mg22211_npb(0:9) = (/ 1, 14, 120, 770, 4075, 18648, 76112, 282760, &
    970530, 3112020 /), &
  mg22211_dnsc(0:9) = (/ 1, 0, 18, 49, 159, 402, 636, 783, 783, 636 /), &
  mg22211_nsc(0:9) = (/ 1, 1, 19, 68, 227, 629, 1265, 2048, 2831, 3467 /), &
  mg22211_dnb(0:9) = (/ 1, 13, 124, 933, 6009, 33936, 171520, 785920, &
    3301372, 12829820 /), &
  mg22211_nb(0:9) = (/ 1, 14, 138, 1071, 7080, 41016, 212536, 998456, &
    4299828, 17129648 /)
!..procedures
CONTAINS
include 'mg22211_prims.f90'
include 'mg22211_prib.f90'
include 'mg22211_isecs.f90'
include 'mg22211_secs.f90'
include 'mg22211_base.f90'
END MODULE inv_mg22211
