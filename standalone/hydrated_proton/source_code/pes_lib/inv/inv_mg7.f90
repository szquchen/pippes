MODULE inv_mg7
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg7_prims, mg7_prib, mg7_isecs, mg7_secs, &
  mg7_base
!..data
integer, parameter, private :: nkk=1, nk=7, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/7/)
integer, parameter, public :: &
  mg7_id=30, &
  mg7_nkk=nkk, mg7_nk=nk, mg7_nr=nr, mg7_ngrp=5040, &
  mg7_nkj(0:nkk-1)=nkj, &
  mg7_dnpr(0:9) = (/ 0, 1, 2, 5, 3, 3, 2, 3, 0, 0 /), &
  mg7_npr(0:9) = (/ 0, 1, 3, 8, 11, 14, 16, 19, 19, 19 /), &
  mg7_dnpb(0:9) = (/ 1, 1, 3, 8, 14, 27, 54, 93, 162, 281 /), &
  mg7_npb(0:9) = (/ 1, 2, 5, 13, 27, 54, 108, 201, 363, 644 /), &
  mg7_dnsc(0:9) = (/ 1, 0, 0, 0, 8, 25, 70, 169, 450, 1146 /), &
  mg7_nsc(0:9) = (/ 1, 1, 1, 1, 9, 34, 104, 273, 723, 1869 /), &
  mg7_dnb(0:9) = (/ 1, 1, 3, 8, 22, 60, 173, 471, 1303, 3510 /), &
  mg7_nb(0:9) = (/ 1, 2, 5, 13, 35, 95, 268, 739, 2042, 5552 /)
!..procedures
CONTAINS
include 'mg7_prims.f90'
include 'mg7_prib.f90'
include 'mg7_isecs.f90'
include 'mg7_secs.f90'
include 'mg7_base.f90'
END MODULE inv_mg7
