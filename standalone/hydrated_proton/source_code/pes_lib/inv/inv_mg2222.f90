MODULE inv_mg2222
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg2222_prims, mg2222_prib, mg2222_isecs, mg2222_secs, &
  mg2222_base
!..data
integer, parameter, private :: nkk=4, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,2,2,2/)
integer, parameter, public :: &
  mg2222_id=59, &
  mg2222_nkk=nkk, mg2222_nk=nk, mg2222_nr=nr, mg2222_ngrp=16, &
  mg2222_nkj(0:nkk-1)=nkj, &
  mg2222_dnpr(0:9) = (/ 0, 10, 18, 0, 0, 0, 0, 0, 0, 0 /), &
  mg2222_npr(0:9) = (/ 0, 10, 28, 28, 28, 28, 28, 28, 28, 28 /), &
  mg2222_dnpb(0:9) = (/ 1, 10, 73, 400, 1876, 7672, 28420, 96496, 305350, &
    907532 /), &
  mg2222_npb(0:9) = (/ 1, 11, 84, 484, 2360, 10032, 38452, 134948, 440298, &
    1347830 /), &
  mg2222_dnsc(0:9) = (/ 1, 0, 12, 58, 165, 516, 1215, 2022, 2703, 3000 /), &
  mg2222_nsc(0:9) = (/ 1, 1, 13, 71, 236, 752, 1967, 3989, 6692, 9692 /), &
  mg2222_dnb(0:9) = (/ 1, 10, 85, 578, 3497, 18872, 92552, 415208, &
    1718924, 6611376 /), &
  mg2222_nb(0:9) = (/ 1, 11, 96, 674, 4171, 23043, 115595, 530803, &
    2249727, 8861103 /)
!..procedures
CONTAINS
include 'mg2222_prims.f90'
include 'mg2222_prib.f90'
include 'mg2222_isecs.f90'
include 'mg2222_secs.f90'
include 'mg2222_base.f90'
END MODULE inv_mg2222
