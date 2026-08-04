MODULE inv_mg33
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg33_prims, mg33_prib, mg33_isecs, mg33_secs, &
  mg33_base
!..data
integer, parameter, private :: nkk=2, nk=6, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,3/)
integer, parameter, public :: &
  mg33_id=22, &
  mg33_nkk=nkk, mg33_nk=nk, mg33_nr=nr, mg33_ngrp=36, &
  mg33_nkj(0:nkk-1)=nkj, &
  mg33_dnpr(0:9) = (/ 0, 3, 5, 5, 1, 0, 1, 0, 0, 0 /), &
  mg33_npr(0:9) = (/ 0, 3, 8, 13, 14, 14, 15, 15, 15, 15 /), &
  mg33_dnpb(0:9) = (/ 1, 3, 11, 30, 76, 174, 380, 774, 1522, 2864 /), &
  mg33_npb(0:9) = (/ 1, 4, 15, 45, 121, 295, 675, 1449, 2971, 5835 /), &
  mg33_dnsc(0:9) = (/ 1, 0, 2, 12, 29, 58, 125, 224, 362, 500 /), &
  mg33_nsc(0:9) = (/ 1, 1, 3, 15, 44, 102, 227, 451, 813, 1313 /), &
  mg33_dnb(0:9) = (/ 1, 3, 13, 48, 163, 511, 1510, 4141, 10723, 26226 /), &
  mg33_nb(0:9) = (/ 1, 4, 17, 65, 228, 739, 2249, 6390, 17113, 43339 /)
!..procedures
CONTAINS
include 'mg33_prims.f90'
include 'mg33_prib.f90'
include 'mg33_isecs.f90'
include 'mg33_secs.f90'
include 'mg33_base.f90'
END MODULE inv_mg33
