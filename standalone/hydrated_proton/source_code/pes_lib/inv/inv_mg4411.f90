MODULE inv_mg4411
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg4411_prims, mg4411_prib, mg4411_isecs, mg4411_secs, &
  mg4411_base
!..data
integer, parameter, private :: nkk=4, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,4,1,1/)
integer, parameter, public :: &
  mg4411_id=114, &
  mg4411_nkk=nkk, mg4411_nk=nk, mg4411_nr=nr, mg4411_ngrp=24*24, &
  mg4411_nkj(0:nkk-1)=nkj, &
  mg4411_dnpr(0:9) = (/ 0, 8, 11, 13, 11, 0, 1, 0, 0, 0 /), &
  mg4411_npr(0:9) = (/ 0, 8, 19, 32, 43, 43, 44, 44, 44, 44 /), &
  mg4411_dnpb(0:9) = (/ 1, 8, 47, 221, 907, 3339, 11321, 35815, &
    106954, 303789 /), &
  mg4411_npb(0:9) = (/ 1, 9, 56, 277, 1184, 4523, 15844, 51659, &
    158613, 462402 /), &
  mg4411_dnsc(0:9) = (/ 1, 0, 12, 76, 404, 2119, 11023, 52829, &
    235898, 978165 /), &
  mg4411_nsc(0:9) = (/ 1, 1, 13, 89, 493, 2612, 13635, 66464, &
    302362, 1280527 /), &
  mg4411_dnb(0:9) = (/ 1, 8, 59, 393, 2483, 14914, 85964, 474705, &
    2507908, 12649249 /), &
  mg4411_nb(0:9) = (/ 1, 9, 68, 461, 2944, 17858, 103822, 578527, &
    3086435, 15735684 /)
!..procedures
CONTAINS
include 'mg4411_prims.f90'
include 'mg4411_prib.f90'
include 'mg4411_isecs.f90'
include 'mg4411_secs.f90'
include 'mg4411_base.f90'
END MODULE inv_mg4411
