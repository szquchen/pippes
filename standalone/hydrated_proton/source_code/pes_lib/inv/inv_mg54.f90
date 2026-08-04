MODULE inv_mg54
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg54_prims, mg54_prib, mg54_isecs, mg54_secs, &
  mg54_base
!..data
integer, parameter, private :: nkk=2, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/5,4/)
integer, parameter, public :: &
  mg54_id=71, &
  mg54_nkk=nkk, mg54_nk=nk, mg54_nr=nr, mg54_ngrp=120*24, &
  mg54_nkj(0:nkk-1)=nkj, &
  mg54_dnpr(0:9) = (/ 0, 3, 7, 9, 8, 3, 2, 0, 0, 0 /), &
  mg54_npr(0:9) = (/ 0, 3, 10, 19, 27, 30, 32, 32, 32, 32 /), &
  mg54_dnpb(0:9) = (/ 1, 3, 13, 40, 120, 319, 824, 1980, 4613, 10274 /), &
  mg54_npb(0:9) = (/ 1, 4, 17, 57, 177, 496, 1320, 3300, 7913, 18187 /), &
  mg54_dnsc(0:9) = (/ 1, 0, 2, 20, 92, 376, 1599, 6600, 26105, 97430 /), &
  mg54_nsc(0:9) = (/ 1, 1, 3, 23, 115, 491, 2090, 8690, 34795, 132225 /), &
  mg54_dnb(0:9) = (/ 1, 3, 15, 66, 298, 1311, 5787, 24983, 105413, &
    430687 /), &
  mg54_nb(0:9) = (/ 1, 4, 19, 85, 383, 1694, 7481, 32464, 137877, &
    568564 /)
!..procedures
CONTAINS
include 'mg54_prims.f90'
include 'mg54_prib.f90'
include 'mg54_isecs.f90'
include 'mg54_secs.f90'
include 'mg54_base.f90'
END MODULE inv_mg54
