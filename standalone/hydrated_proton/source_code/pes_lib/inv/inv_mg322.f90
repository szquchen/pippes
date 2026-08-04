MODULE inv_mg322
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg322_prims, mg322_prib, mg322_isecs, mg322_secs, &
  mg322_base
!..data
integer, parameter, private :: nkk=3, nk=7, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,2,2/)
integer, parameter, public :: &
  mg322_id=37, &
  mg322_nkk=nkk, mg322_nk=nk, mg322_nr=nr, mg322_ngrp=24, &
  mg322_nkj(0:nkk-1)=nkj, &
  mg322_dnpr(0:9) = (/ 0, 6, 10, 3, 0, 0, 2, 0, 0, 0 /), &
  mg322_npr(0:9) = (/ 0, 6, 16, 19, 19, 19, 21, 21, 21, 21 /), &
  mg322_dnpb(0:9) = (/ 1, 6, 31, 119, 409, 1235, 3453, 8933, 21846, &
    50627 /), &
  mg322_npb(0:9) = (/ 1, 7, 38, 157, 566, 1801, 5254, 14187, 36033, &
    86660 /), &
  mg322_dnsc(0:9) = (/ 1, 0, 5, 30, 77, 189, 497, 999, 1609, 2428 /), &
  mg322_nsc(0:9) = (/ 1, 1, 6, 36, 113, 302, 799, 1798, 3407, 5835 /), &
  mg322_dnb(0:9) = (/ 1, 6, 36, 179, 821, 3411, 13086, 46381, 153155, &
    473472 /), &
  mg322_nb(0:9) = (/ 1, 7, 43, 222, 1043, 4454, 17540, 63921, 217076, &
    690548 /)
!..procedures
CONTAINS
include 'mg322_prims.f90'
include 'mg322_prib.f90'
include 'mg322_isecs.f90'
include 'mg322_secs.f90'
include 'mg322_base.f90'
END MODULE inv_mg322
