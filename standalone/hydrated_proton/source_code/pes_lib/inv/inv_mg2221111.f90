MODULE inv_mg2221111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg2221111_prims, mg2221111_prib, mg2221111_isecs, mg2221111_secs, &
  mg2221111_base
!..data
integer, parameter, private :: nkk=7, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,2,2,1,1,1,1/)
integer, parameter, public :: &
  mg2221111_id=134, &
  mg2221111_nkk=nkk, mg2221111_nk=nk, mg2221111_nr=nr, mg2221111_ngrp=8, &
  mg2221111_nkj(0:nkk-1)=nkj, &
  mg2221111_dnpr(0:9) = (/ 0, 24, 21, 0, 0, 0, 0, 0, 0, 0 /), &
  mg2221111_npr(0:9) = (/ 0, 24, 45, 45, 45, 45, 45, 45, 45, 45 /), &
  mg2221111_dnpb(0:9) = (/ 1, 24, 321, 3104, 24081, 158424, 914641, &
    4742784, 22460121, 98362904 /), &
  mg2221111_npb(0:9) = (/ 1, 25, 346, 3450, 27531, 185955, 1100596, &
    5843380, 28303501, 126666405 /), &
  mg2221111_dnsc(0:9) = (/ 1, 0, 45, 109, 828, 2385, 7068, &
    14556, 24726, 37596 /), &
  mg2221111_nsc(0:9) = (/ 1, 1, 46, 155, 983, 3368, 10436, &
    24992, 49718, 87314 /), &
  mg2221111_dnb(0:9) = (/ 1, 24, 366, 4293, 41970, 355350, 2666718, &
    18016578, 110872188, 627334878 /), &
  mg2221111_nb(0:9) = (/ 1, 25, 391, 4684, 46654, 402004, 3068722, &
    21085300, 131957488, 759292366 /)
!..procedures
CONTAINS
include 'mg2221111_prims.f90'
include 'mg2221111_prib.f90'
include 'mg2221111_isecs.f90'
include 'mg2221111_secs.f90'
include 'mg2221111_base.f90'
END MODULE inv_mg2221111
