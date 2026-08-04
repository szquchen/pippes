MODULE inv_mg42111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg42111_prims, mg42111_prib, mg42111_isecs, mg42111_secs, &
  mg42111_base
!..data
integer, parameter, private :: nkk=5, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,2,1,1,1/)
integer, parameter, public :: &
  mg42111_id=86, &
  mg42111_nkk=nkk, mg42111_nk=nk, mg42111_nr=nr, mg42111_ngrp=48, &
  mg42111_nkj(0:nkk-1)=nkj, &
  mg42111_dnpr(0:9) = (/ 0, 12, 11, 6, 6, 0, 1, 0, 0, 0 /), &
  mg42111_npr(0:9) = (/ 0, 12, 23, 29, 35, 35, 36, 36, 36, 36 /), &
  mg42111_dnpb(0:9) = (/ 1, 12, 89, 502, 2367, 9770, 36357, 124338, &
    396246, 1188792 /), &
  mg42111_npb(0:9) = (/ 1, 13, 102, 604, 2971, 12741, 49098, 173436, &
    569682, 1758474 /), &
  mg42111_dnsc(0:9) = (/ 1, 0, 16, 71, 356, 1552, 6086, 20776, &
    64001, 175855 /), &
  mg42111_nsc(0:9) = (/ 1, 1, 17, 88, 444, 1996, 8082, 28858, &
    92859, 268714 /), &
  mg42111_dnb(0:9) = (/ 1, 12, 105, 765, 4999, 29945, 166265, 859363, &
    4148351, 18759354 /), &
  mg42111_nb(0:9) = (/ 1, 13, 118, 883, 5882, 35827, 202092, 1061455, &
    5209806, 23969160 /)
!..procedures
CONTAINS
include 'mg42111_prims.f90'
include 'mg42111_prib.f90'
include 'mg42111_isecs.f90'
include 'mg42111_secs.f90'
include 'mg42111_base.f90'
END MODULE inv_mg42111
