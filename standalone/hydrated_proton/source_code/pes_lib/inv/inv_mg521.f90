MODULE inv_mg521
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg521_prims, mg521_prib, mg521_isecs, mg521_secs, &
  mg521_base
!..data
integer, parameter, private :: nkk=3, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/5,2,1/)
integer, parameter, public :: &
  mg521_id=51, &
  mg521_nkk=nkk, mg521_nk=nk, mg521_nr=nr, mg521_ngrp=240, &
  mg521_nkj(0:nkk-1)=nkj, &
  mg521_dnpr(0:9) = (/ 0, 5, 7, 4, 5, 4, 2, 0, 0, 0 /), &
  mg521_npr(0:9) = (/ 0, 5, 12, 16, 21, 25, 27, 27, 27, 27 /), &
  mg521_dnpb(0:9) = (/ 1, 5, 22, 74, 228, 628, 1626, 3942, 9129, 20209 /), &
  mg521_npb(0:9) = (/ 1, 6, 28, 102, 330, 958, 2584, 6526, 15655, 35864 /), &
  mg521_dnsc(0:9) = (/ 1, 0, 4, 26, 99, 372, 1365, 4568, 14179, 40446 /), &
  mg521_nsc(0:9) = (/ 1, 1, 5, 31, 130, 502, 1867, 6435, 20614, 61060 /), &
  mg521_dnb(0:9) = (/ 1, 5, 26, 120, 545, 2363, 9865, 39285, 149110, &
    538088 /), &
  mg521_nb(0:9) = (/ 1, 6, 32, 152, 697, 3060, 12925, 52210, 201320, &
    739408 /)
!..procedures
CONTAINS
include 'mg521_prims.f90'
include 'mg521_prib.f90'
include 'mg521_isecs.f90'
include 'mg521_secs.f90'
include 'mg521_base.f90'
END MODULE inv_mg521
