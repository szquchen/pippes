MODULE inv_mg3322
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg3322_prims, mg3322_prib, mg3322_isecs, mg3322_secs, &
  mg3322_base
!..data
integer, parameter, private :: nkk=4, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,3,2,2/)
integer, parameter, public :: &
  mg3322_id=119, &
  mg3322_nkk=nkk, mg3322_nk=nk, mg3322_nr=nr, mg3322_ngrp=6*6*4, &
  mg3322_nkj(0:nkk-1)=nkj, &
  mg3322_dnpr(0:9) = (/ 0, 10, 20, 9, 1, 0, 5, 0, 0, 0 /), &
  mg3322_npr(0:9) = (/ 0, 10, 30, 39, 40, 40, 45, 45, 45, 45 /), &
  mg3322_dnpb(0:9) = (/ 1, 10, 75, 429, 2116, 9187, 36300, 132234, &
    450499, 1447142 /), &
  mg3322_npb(0:9) = (/ 1, 11, 86, 515, 2631, 11818, 48118, 180352, &
    630851, 2077993 /), &
  mg3322_dnsc(0:9) = (/ 1, 0, 18, 133, 676, 3776, 19645, 88674, &
    363384, 1355950 /), &
  mg3322_nsc(0:9) = (/ 1, 1, 19, 152, 828, 4604, 24249, 112923, &
    476307, 1832257 /), &
  mg3322_dnb(0:9) = (/ 1, 10, 93, 742, 5472, 37420, 239550, 1437356, &
    8099589, 42923727 /), &
  mg3322_nb(0:9) = (/ 1, 11, 104, 846, 6318, 43738, 283288, 1720644, &
    9820233, 52743960 /)
!..procedures
CONTAINS
include 'mg3322_prims.f90'
include 'mg3322_prib.f90'
include 'mg3322_isecs.f90'
include 'mg3322_secs.f90'
include 'mg3322_base.f90'
END MODULE inv_mg3322
