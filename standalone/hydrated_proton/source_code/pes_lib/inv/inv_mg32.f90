MODULE inv_mg32
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg32_prims, mg32_prib, mg32_isecs, mg32_secs, &
  mg32_base
!..data
integer, parameter, private :: nkk=2, nk=5, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,2/)
integer, parameter, public :: &
  mg32_id=14, &
  mg32_nkk=nkk, mg32_nk=nk, mg32_nr=nr, mg32_ngrp=12, &
  mg32_nkj(0:nkk-1)=nkj, &
  mg32_dnpr(0:9) = (/ 0, 3, 4, 2, 0, 0, 1, 0, 0, 0 /), &
  mg32_npr(0:9) = (/ 0, 3, 7, 9, 9, 9, 10, 10, 10, 10 /), &
  mg32_dnpb(0:9) = (/ 1, 3, 10, 24, 55, 111, 216, 390, 684, 1144 /), &
  mg32_npb(0:9) = (/ 1, 4, 14, 38, 93, 204, 420, 810, 1494, 2638 /), &
  mg32_dnsc(0:9) = (/ 1, 0, 1, 6, 8, 6, 12, 14, 9, 8 /), &
  mg32_nsc(0:9) = (/ 1, 1, 2, 8, 16, 22, 34, 48, 57, 65 /), &
  mg32_dnb(0:9) = (/ 1, 3, 11, 33, 91, 225, 525, 1133, 2321, 4511 /), &
  mg32_nb(0:9) = (/ 1, 4, 15, 48, 139, 364, 889, 2022, 4343, 8854 /)
!..procedures
CONTAINS
include 'mg32_prims.f90'
include 'mg32_prib.f90'
include 'mg32_isecs.f90'
include 'mg32_secs.f90'
include 'mg32_base.f90'
END MODULE inv_mg32
