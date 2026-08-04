MODULE inv_mg31111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg31111_prims, mg31111_prib, mg31111_isecs, mg31111_secs, &
  mg31111_base
!..data
integer, parameter, private :: nkk=5, nk=7, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,1,1,1,1/)
integer, parameter, public :: &
  mg31111_id=41, &
  mg31111_nkk=nkk, mg31111_nk=nk, mg31111_nr=nr, mg31111_ngrp=6, &
  mg31111_nkj(0:nkk-1)=nkj, &
  mg31111_dnpr(0:9) = (/ 0, 11, 5, 5, 0, 0, 0, 0, 0, 0 /), &
  mg31111_npr(0:9) = (/ 0, 11, 16, 21, 21, 21, 21, 21, 21, 21 /), &
  mg31111_dnpb(0:9) = (/ 1, 11, 71, 346, 1401, 4953, 15758, 46033, &
    125248, 320783 /), &
  mg31111_npb(0:9) = (/ 1, 12, 83, 429, 1830, 6783, 22541, 68574, &
    193822, 514605 /), &
  mg31111_dnsc(0:9) = (/ 1, 0, 10, 30, 55, 125, 215, 215, 215, 205 /), &
  mg31111_nsc(0:9) = (/ 1, 1, 11, 41, 96, 221, 436, 651, 866, 1071 /), &
  mg31111_dnb(0:9) = (/ 1, 11, 81, 486, 2496, 11273, 45643, 168078, &
    569568, 1793618 /), &
  mg31111_nb(0:9) = (/ 1, 12, 93, 579, 3075, 14348, 59991, 228069, &
    797637, 2591255 /)
!..procedures
CONTAINS
include 'mg31111_prims.f90'
include 'mg31111_prib.f90'
include 'mg31111_isecs.f90'
include 'mg31111_secs.f90'
include 'mg31111_base.f90'
END MODULE inv_mg31111
