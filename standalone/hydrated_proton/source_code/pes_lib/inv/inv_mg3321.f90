MODULE inv_mg3321
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg3321_prims, mg3321_prib, mg3321_isecs, mg3321_secs, &
  mg3321_base
!..data
integer, parameter, private :: nkk=4, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,3,2,1/)
integer, parameter, public :: &
  mg3321_id=83, &
  mg3321_nkk=nkk, mg3321_nk=nk, mg3321_nr=nr, mg3321_ngrp=72, &
  mg3321_nkj(0:nkk-1)=nkj, &
  mg3321_dnpr(0:9) = (/ 0, 9, 14, 9, 1, 0, 3, 0, 0, 0 /), &
  mg3321_npr(0:9) = (/ 0, 9, 23, 32, 33, 33, 36, 36, 36, 36 /), &
  mg3321_dnpb(0:9) = (/ 1, 9, 59, 300, 1312, 5082, 17944, 58620, &
    179489, 519559 /), &
  mg3321_npb(0:9) = (/ 1, 10, 69, 369, 1681, 6763, 24707, 83327, &
    262816, 782375 /), &
  mg3321_dnsc(0:9) = (/ 1, 0, 15, 89, 389, 1836, 7849, 28516, &
    93509, 276745 /), &
  mg3321_nsc(0:9) = (/ 1, 1, 16, 105, 494, 2330, 10179, 38695, &
    132204, 408949 /), &
  mg3321_dnb(0:9) = (/ 1, 9, 74, 524, 3387, 20170, 111648, 575799, &
    2775359, 12537075 /), &
  mg3321_nb(0:9) = (/ 1, 10, 84, 608, 3995, 24165, 135813, 711612, &
    3486971, 16024046 /)
!..procedures
CONTAINS
include 'mg3321_prims.f90'
include 'mg3321_prib.f90'
include 'mg3321_isecs.f90'
include 'mg3321_secs.f90'
include 'mg3321_base.f90'
END MODULE inv_mg3321
