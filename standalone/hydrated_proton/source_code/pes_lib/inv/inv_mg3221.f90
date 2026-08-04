MODULE inv_mg3221
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg3221_prims, mg3221_prib, mg3221_isecs, mg3221_secs, &
  mg3221_base
!..data
integer, parameter, private :: nkk=4, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,2,2,1/)
integer, parameter, public :: &
  mg3221_id=58, &
  mg3221_nkk=nkk, mg3221_nk=nk, mg3221_nr=nr, mg3221_ngrp=24, &
  mg3221_nkj(0:nkk-1)=nkj, &
  mg3221_dnpr(0:9) = (/ 0, 9, 13, 4, 0, 0, 2, 0, 0, 0 /), &
  mg3221_npr(0:9) = (/ 0, 9, 22, 26, 26, 26, 28, 28, 28, 28 /), &
  mg3221_dnpb(0:9) = (/ 1, 9, 58, 286, 1207, 4483, 15128, 47068, 136949, &
    375901 /), &
  mg3221_npb(0:9) = (/ 1, 10, 68, 354, 1561, 6044, 21172, 68240, 205189, &
    581090 /), &
  mg3221_dnsc(0:9) = (/ 1, 0, 12, 58, 196, 684, 2096, 5053, 10665, &
    20252 /), &
  mg3221_nsc(0:9) = (/ 1, 1, 13, 71, 267, 951, 3047, 8100, 18765, &
    39017 /), &
  mg3221_dnb(0:9) = (/ 1, 9, 70, 452, 2621, 13727, 65820, 290515, &
    1188405, 4531164 /), &
  mg3221_nb(0:9) = (/ 1, 10, 80, 532, 3153, 16880, 82700, 373215, &
    1561620, 6092784 /)
!..procedures
CONTAINS
include 'mg3221_prims.f90'
include 'mg3221_prib.f90'
include 'mg3221_isecs.f90'
include 'mg3221_secs.f90'
include 'mg3221_base.f90'
END MODULE inv_mg3221
