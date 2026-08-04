MODULE inv_mg5211
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg5211_prims, mg5211_prib, mg5211_isecs, mg5211_secs, &
  mg5211_base
!..data
integer, parameter, private :: nkk=4, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/5,2,1,1/)
integer, parameter, public :: &
  mg5211_id=80, &
  mg5211_nkk=nkk, mg5211_nk=nk, mg5211_nr=nr, mg5211_ngrp=120*2, &
  mg5211_nkj(0:nkk-1)=nkj, &
  mg5211_dnpr(0:9) = (/ 0, 8, 9, 5, 6, 5, 2, 0, 0, 0 /), &
  mg5211_npr(0:9) = (/ 0, 8, 17, 22, 28, 33, 35, 35, 35, 35 /), &
  mg5211_dnpb(0:9) = (/ 1, 8, 45, 197, 745, 2510, 7758, 22318, 60529, &
    156027 /), &
  mg5211_npb(0:9) = (/ 1, 9, 54, 251, 996, 3506, 11264, 33582, 94111, &
    250138 /), &
  mg5211_dnsc(0:9) = (/ 1, 0, 9, 47, 216, 994, 4282, 16864, 61454, &
    205983 /), &
  mg5211_nsc(0:9) = (/ 1, 1, 10, 57, 273, 1267, 5549, 22413, 83867, &
    289850 /), &
  mg5211_dnb(0:9) = (/ 1, 8, 54, 316, 1742, 9120, 45676, 218325, 994115, &
    4304254 /), &
  mg5211_nb(0:9) = (/ 1, 9, 63, 379, 2121, 11241, 56917, 275242, 1269357, &
    5573611 /)
!..procedures
CONTAINS
include 'mg5211_prims.f90'
include 'mg5211_prib.f90'
include 'mg5211_isecs.f90'
include 'mg5211_secs.f90'
include 'mg5211_base.f90'
END MODULE inv_mg5211
