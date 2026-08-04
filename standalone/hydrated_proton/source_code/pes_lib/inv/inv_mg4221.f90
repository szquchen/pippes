MODULE inv_mg4221
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg4221_prims, mg4221_prib, mg4221_isecs, mg4221_secs, &
  mg4221_base
!..data
integer, parameter, private :: nkk=4, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,2,2,1/)
integer, parameter, public :: &
  mg4221_id=82, &
  mg4221_nkk=nkk, mg4221_nk=nk, mg4221_nr=nr, mg4221_ngrp=96, &
  mg4221_nkj(0:nkk-1)=nkj, &
  mg4221_dnpr(0:9) = (/ 0, 9, 14, 5, 6, 0, 2, 0, 0, 0 /), &
  mg4221_npr(0:9) = (/ 0, 9, 23, 28, 34, 34, 36, 36, 36, 36 /), &
  mg4221_dnpb(0:9) = (/ 1, 9, 59, 296, 1281, 4891, 17044, 54897, &
    165851, 473648 /), &
  mg4221_npb(0:9) = (/ 1, 10, 69, 365, 1646, 6537, 23581, 78478, &
    244329, 717977 /), &
  mg4221_dnsc(0:9) = (/ 1, 0, 12, 70, 313, 1443, 6088, 22436, &
    74543, 224066 /), &
  mg4221_nsc(0:9) = (/ 1, 1, 13, 83, 396, 1839, 7927, 30363, &
    104906, 328972 /), &
  mg4221_dnb(0:9) = (/ 1, 9, 71, 474, 2932, 16833, 90678, 458272, &
    2176489, 9725583 /), &
  mg4221_nb(0:9) = (/ 1, 10, 81, 555, 3487, 20320, 110998, 569270, &
    2745759, 12471342 /)
!..procedures
CONTAINS
include 'mg4221_prims.f90'
include 'mg4221_prib.f90'
include 'mg4221_isecs.f90'
include 'mg4221_secs.f90'
include 'mg4221_base.f90'
END MODULE inv_mg4221
