MODULE inv_mg4211
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg4211_prims, mg4211_prib, mg4211_isecs, mg4211_secs, &
  mg4211_base
!..data
integer, parameter, private :: nkk=4, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,2,1,1/)
integer, parameter, public :: &
  mg4211_id=56, &
  mg4211_nkk=nkk, mg4211_nk=nk, mg4211_nr=nr, mg4211_ngrp=48, &
  mg4211_nkj(0:nkk-1)=nkj, &
  mg4211_dnpr(0:9) = (/ 0, 8, 9, 5, 5, 0, 1, 0, 0, 0 /), &
  mg4211_npr(0:9) = (/ 0, 8, 17, 22, 27, 27, 28, 28, 28, 28 /), &
  mg4211_dnpb(0:9) = (/ 1, 8, 45, 197, 744, 2497, 7672, 21888, 58754, &
    149600 /), &
  mg4211_npb(0:9) = (/ 1, 9, 54, 251, 995, 3492, 11164, 33052, 91806, &
    241406 /), &
  mg4211_dnsc(0:9) = (/ 1, 0, 9, 43, 170, 617, 2059, 5974, 15514, 35968 /), &
  mg4211_nsc(0:9) = (/ 1, 1, 10, 53, 223, 840, 2899, 8873, 24387, 60355 /), &
  mg4211_dnb(0:9) = (/ 1, 8, 54, 312, 1663, 8182, 37484, 160054, 639163, &
    2394559 /), &
  mg4211_nb(0:9) = (/ 1, 9, 63, 375, 2038, 10220, 47704, 207758, 846921, &
    3241480 /)
!..procedures
CONTAINS
include 'mg4211_prims.f90'
include 'mg4211_prib.f90'
include 'mg4211_isecs.f90'
include 'mg4211_secs.f90'
include 'mg4211_base.f90'
END MODULE inv_mg4211
