MODULE inv_mg111111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg111111_prims, mg111111_prib, mg111111_secs, &
  mg111111_base
!..data
integer, parameter, private :: nkk=6, nk=6, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/1,1,1,1,1,1/)
integer, parameter, public :: &
  mg111111_id=29, &
  mg111111_nkk=nkk, mg111111_nk=nk, mg111111_nr=nr, mg111111_ngrp=1, &
  mg111111_nkj(0:nkk-1)=nkj, &
  mg111111_dnpr(0:9) = (/ 0, 15, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  mg111111_npr(0:9) = (/ 0, 15, 15, 15, 15, 15, 15, 15, 15, 15 /), &
  mg111111_dnpb(0:9) = (/ 1, 15, 120, 680, 3060, 11628, 38760, 116280, &
    319770, 817190 /), &
  mg111111_npb(0:9) = (/ 1, 16, 136, 816, 3876, 15504, 54264, 170544, &
    490314, 1307504 /), &
  mg111111_dnsc(0:9) = (/ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  mg111111_nsc(0:9) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  mg111111_dnb(0:9) = (/ 1, 15, 120, 680, 3060, 11628, 38760, 116280, &
    319770, 817190 /), &
  mg111111_nb(0:9) = (/ 1, 16, 136, 816, 3876, 15504, 54264, 170544, &
    490314, 1307504 /)
!..procedures
CONTAINS
include 'mg111111_prims.f90'
include 'mg111111_prib.f90'
include 'mg111111_secs.f90'
include 'mg111111_base.f90'
END MODULE inv_mg111111
