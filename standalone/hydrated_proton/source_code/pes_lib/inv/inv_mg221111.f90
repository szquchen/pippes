MODULE inv_mg221111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg221111_prims, mg221111_prib, mg221111_isecs, mg221111_secs, &
  mg221111_base
!..data
integer, parameter, private :: nkk=6, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,2,1,1,1,1/)
integer, parameter, public :: &
  mg221111_id=64, &
  mg221111_nkk=nkk, mg221111_nk=nk, mg221111_nr=nr, mg221111_ngrp=4, &
  mg221111_nkj(0:nkk-1)=nkj, &
  mg221111_dnpr(0:9) = (/ 0, 17, 11, 0, 0, 0, 0, 0, 0, 0 /), &
  mg221111_npr(0:9) = (/ 0, 17, 28, 28, 28, 28, 28, 28, 28, 28 /), &
  mg221111_dnpb(0:9) = (/ 1, 17, 164, 1156, 6594, 32130, 138292, 537812, &
    1920743, 6376887 /), &
  mg221111_npb(0:9) = (/ 1, 18, 182, 1338, 7932, 40062, 178354, 716166, &
    2636909, 9013796 /), &
  mg221111_dnsc(0:9) = (/ 1, 0, 20, 25, 110, 100, 100, 110, 25, 20 /), &
  mg221111_nsc(0:9) = (/ 1, 1, 21, 46, 156, 256, 356, 466, 491, 511 /), &
  mg221111_dnb(0:9) = (/ 1, 17, 184, 1521, 10409, 61320, 318912, 1490632, &
    6349068, 24918212 /), &
  mg221111_nb(0:9) = (/ 1, 18, 202, 1723, 12132, 73452, 392364, 1882996, &
    8232064, 33150276 /)
!..procedures
CONTAINS
include 'mg221111_prims.f90'
include 'mg221111_prib.f90'
include 'mg221111_isecs.f90'
include 'mg221111_secs.f90'
include 'mg221111_base.f90'
END MODULE inv_mg221111
