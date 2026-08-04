MODULE inv_mg221
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg221_prims, mg221_prib, mg221_isecs, mg221_secs, &
  mg221_base
!..data
integer, parameter, private :: nkk=3, nk=5, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,2,1/)
integer, parameter, public :: &
  mg221_id=16, &
  mg221_nkk=nkk, mg221_nk=nk, mg221_nr=nr, mg221_ngrp=4, &
  mg221_nkj(0:nkk-1)=nkj, &
  mg221_dnpr(0:9) = (/ 0, 5, 5, 0, 0, 0, 0, 0, 0, 0 /), &
  mg221_npr(0:9) = (/ 0, 5, 10, 10, 10, 10, 10, 10, 10, 10 /), &
  mg221_dnpb(0:9) = (/ 1, 5, 20, 60, 160, 376, 820, 1660, 3190, 5830 /), &
  mg221_npb(0:9) = (/ 1, 6, 26, 86, 246, 622, 1442, 3102, 6292, 12122 /), &
  mg221_dnsc(0:9) = (/ 1, 0, 2, 4, 1, 0, 0, 0, 0, 0 /), &
  mg221_nsc(0:9) = (/ 1, 1, 3, 7, 8, 8, 8, 8, 8, 8 /), &
  mg221_dnb(0:9) = (/ 1, 5, 22, 74, 221, 581, 1400, 3112, 6494, 12806 /), &
  mg221_nb(0:9) = (/ 1, 6, 28, 102, 323, 904, 2304, 5416, 11910, 24716 /)
!..procedures
CONTAINS
include 'mg221_prims.f90'
include 'mg221_prib.f90'
include 'mg221_isecs.f90'
include 'mg221_secs.f90'
include 'mg221_base.f90'
END MODULE inv_mg221
