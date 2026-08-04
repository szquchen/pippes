MODULE inv_mg2221
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg2221_prims, mg2221_prib, mg2221_isecs, mg2221_secs, &
  mg2221_base
!..data
integer, parameter, private :: nkk=4, nk=7, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,2,2,1/)
integer, parameter, public :: &
  mg2221_id=40, &
  mg2221_nkk=nkk, mg2221_nk=nk, mg2221_nr=nr, mg2221_ngrp=8, &
  mg2221_nkj(0:nkk-1)=nkj, &
  mg2221_dnpr(0:9) = (/ 0, 9, 12, 0, 0, 0, 0, 0, 0, 0 /), &
  mg2221_npr(0:9) = (/ 0, 9, 21, 21, 21, 21, 21, 21, 21, 21 /), &
  mg2221_dnpb(0:9) = (/ 1, 9, 57, 273, 1113, 3969, 12817, 38025, &
    105261, 274261 /), &
  mg2221_npb(0:9) = (/ 1, 10, 67, 340, 1453, 5422, 18239, 56264, &
    161525, 435786 /), &
  mg2221_dnsc(0:9) = (/ 1, 0, 9, 28, 54, 108, 126, 84, 57, 36 /), &
  mg2221_nsc(0:9) = (/ 1, 1, 10, 38, 92, 200, 326, 410, 467, 503 /), &
  mg2221_dnb(0:9) = (/ 1, 9, 66, 382, 1932, 8616, 34654, 127026, 429327, &
    1349627 /), &
  mg2221_nb(0:9) = (/ 1, 10, 76, 458, 2390, 11006, 45660, 172686, 602013, &
    1951640 /)
!..procedures
CONTAINS
include 'mg2221_prims.f90'
include 'mg2221_prib.f90'
include 'mg2221_isecs.f90'
include 'mg2221_secs.f90'
include 'mg2221_base.f90'
END MODULE inv_mg2221
