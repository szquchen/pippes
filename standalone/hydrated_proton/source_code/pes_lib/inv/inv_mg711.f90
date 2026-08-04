MODULE inv_mg711
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg711_prims, mg711_prib, mg711_isecs, mg711_secs, &
  mg711_base
!..data
integer, parameter, private :: nkk=3, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/7,1,1/)
integer, parameter, public :: &
  mg711_id=72, &
  mg711_nkk=nkk, mg711_nk=nk, mg711_nr=nr, mg711_ngrp=5040, &
  mg711_nkj(0:nkk-1)=nkj, &
  mg711_dnpr(0:9) = (/ 0, 4, 4, 7, 5, 5, 4, 5, 0, 0 /), &
  mg711_npr(0:9) = (/ 0, 4, 8, 15, 20, 25, 29, 34, 34, 34 /), &
  mg711_dnpb(0:9) = (/ 1, 4, 14, 43, 118, 299, 718, 1637, 3581, 7562 /), &
  mg711_npb(0:9) = (/ 1, 5, 19, 62, 180, 479, 1197, 2834, 6415, 13977 /), &
  mg711_dnsc(0:9) = (/ 1, 0, 3, 14, 64, 251, 1023, 4007, 15341, 56200 /), &
  mg711_nsc(0:9) = (/ 1, 1, 4, 18, 82, 333, 1356, 5363, 20704, 76904 /), &
  mg711_dnb(0:9) = (/ 1, 4, 17, 69, 280, 1131, 4597, 18551, 73957, &
    288930 /), &
  mg711_nb(0:9) = (/ 1, 5, 22, 91, 371, 1502, 6099, 24650, 98607, &
    387537 /)
!..procedures
CONTAINS
include 'mg711_prims.f90'
include 'mg711_prib.f90'
include 'mg711_isecs.f90'
include 'mg711_secs.f90'
include 'mg711_base.f90'
END MODULE inv_mg711
