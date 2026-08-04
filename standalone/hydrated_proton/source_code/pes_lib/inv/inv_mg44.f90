MODULE inv_mg44
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg44_prims, mg44_prib, mg44_isecs, mg44_secs, &
  mg44_base
!..data
integer, parameter, private :: nkk=2, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,4/)
integer, parameter, public :: &
  mg44_id=49, &
  mg44_nkk=nkk, mg44_nk=nk, mg44_nr=nr, mg44_ngrp=576, &
  mg44_nkj(0:nkk-1)=nkj, &
  mg44_dnpr(0:9) = (/ 0, 3, 7, 9, 7, 0, 1, 0, 0, 0 /), &
  mg44_npr(0:9) = (/ 0, 3, 10, 19, 26, 26, 27, 27, 27, 27 /), &
  mg44_dnpb(0:9) = (/ 1, 3, 13, 40, 119, 313, 801, 1898, 4360, 9558 /), &
  mg44_npb(0:9) = (/ 1, 4, 17, 57, 176, 489, 1290, 3188, 7548, 17106 /), &
  mg44_dnsc(0:9) = (/ 1, 0, 2, 18, 75, 272, 993, 3406, 10971, 32544 /), &
  mg44_nsc(0:9) = (/ 1, 1, 3, 21, 96, 368, 1361, 4767, 15738, 48282 /), &
  mg44_dnb(0:9) = (/ 1, 3, 15, 64, 274, 1124, 4543, 17587, 65499, &
    233070 /), &
  mg44_nb(0:9) = (/ 1, 4, 19, 83, 357, 1481, 6024, 23611, 89110, &
    322180 /)
! Note: there is also a primary at degree 12
!..procedures
CONTAINS
include 'mg44_prims.f90'
include 'mg44_prib.f90'
include 'mg44_isecs.f90'
include 'mg44_secs.f90'
include 'mg44_base.f90'
END MODULE inv_mg44
