MODULE inv_mg71
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg71_prims, mg71_prib, mg71_isecs, mg71_secs, &
  mg71_base
!..data
integer, parameter, private :: nkk=2, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/7,1/)
integer, parameter, public :: &
  mg71_id=46, &
  mg71_nkk=nkk, mg71_nk=nk, mg71_nr=nr, mg71_ngrp=5040, &
  mg71_nkj(0:nkk-1)=nkj, &
  mg71_dnpr(0:9) = (/ 0, 2, 3, 6, 4, 4, 3, 4, 0, 0 /), &
  mg71_npr(0:9) = (/ 0, 2, 5, 11, 15, 19, 22, 26, 26, 26 /), &
  mg71_dnpb(0:9) = (/ 1, 2, 6, 16, 36, 78, 166, 330, 639, 1206 /), &
  mg71_npb(0:9) = (/ 1, 3, 9, 25, 61, 139, 305, 635, 1274, 2480 /), &
  mg71_dnsc(0:9) = (/ 1, 0, 1, 5, 26, 88, 301, 977, 3184, 9920 /), &
  mg71_nsc(0:9) = (/ 1, 1, 2, 7, 33, 121, 422, 1399, 4583, 14503 /), &
  mg71_dnb(0:9) = (/ 1, 2, 7, 23, 78, 264, 915, 3111, 10483, &
    34528 /), &
  mg71_nb(0:9) = (/ 1, 3, 10, 33, 111, 375, 1290, 4401, 14884, &
    49412 /)
! Note: there are also primaries at degrees 10 and 12.
!..procedures
CONTAINS
include 'mg71_prims.f90'
include 'mg71_prib.f90'
include 'mg71_isecs.f90'
include 'mg71_secs.f90'
include 'mg71_base.f90'
END MODULE inv_mg71
