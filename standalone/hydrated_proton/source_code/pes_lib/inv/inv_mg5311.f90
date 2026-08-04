MODULE inv_mg5311
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg5311_prims, mg5311_prib, mg5311_isecs, mg5311_secs, &
  mg5311_base
!..data
integer, parameter, private :: nkk=4, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/5,3,1,1/)
integer, parameter, public :: &
  mg5311_id=113, &
  mg5311_nkk=nkk, mg5311_nk=nk, mg5311_nr=nr, mg5311_ngrp=120*6, &
  mg5311_nkj(0:nkk-1)=nkj, &
  mg5311_dnpr(0:9) = (/ 0, 8, 10, 10, 7, 5, 2, 0, 0, 0 /), &
  mg5311_npr(0:9) = (/ 0, 8, 18, 28, 35, 40, 42, 42, 42, 42 /), &
  mg5311_dnpb(0:9) = (/ 1, 8, 46, 210, 832, 2953, 9635, 29318, &
    84175, 229868 /), &
  mg5311_npb(0:9) = (/ 1, 9, 55, 265, 1097, 4050, 13685, 43003, &
    127178, 357046 /), &
  mg5311_dnsc(0:9) = (/ 1, 0, 12, 71, 350, 1887, 9674, 46167, &
    207156, 868082 /), &
  mg5311_nsc(0:9) = (/ 1, 1, 13, 84, 434, 2321, 11995, 58162, &
    265318, 1133400 /), &
  mg5311_dnb(0:9) = (/ 1, 8, 58, 377, 2302, 13426, 75399, 407687, &
    2118424, 10549855 /), &
  mg5311_nb(0:9) = (/ 1, 9, 67, 444, 2746, 16172, 91571, 499258, &
    2617682, 13167537 /)
!..procedures
CONTAINS
include 'mg5311_prims.f90'
include 'mg5311_prib.f90'
include 'mg5311_isecs.f90'
include 'mg5311_secs.f90'
include 'mg5311_base.f90'
END MODULE inv_mg5311
