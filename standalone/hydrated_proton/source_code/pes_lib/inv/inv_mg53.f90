MODULE inv_mg53
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg53_prims, mg53_prib, mg53_isecs, mg53_secs, &
  mg53_base
!..data
integer, parameter, private :: nkk=2, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/5,3/)
integer, parameter, public :: &
  mg53_id=48, &
  mg53_nkk=nkk, mg53_nk=nk, mg53_nr=nr, mg53_ngrp=120*6, &
  mg53_nkj(0:nkk-1)=nkj, &
  mg53_dnpr(0:9) = (/ 0, 3, 6, 6, 5, 3, 2, 0, 0, 0 /), &
  mg53_npr(0:9) = (/ 0, 3, 9, 15, 20, 23, 25, 25, 25, 25 /), &
  mg53_dnpb(0:9) = (/ 1, 3, 12, 34, 95, 234, 560, 1247, 2697, 5572 /), &
  mg53_npb(0:9) = (/ 1, 4, 16, 50, 145, 379, 939, 2186, 4883, 10455 /), &
  mg53_dnsc(0:9) = (/ 1, 0, 2, 18, 68, 249, 929, 3213, 10524, 32140 /), &
  mg53_nsc(0:9) = (/ 1, 1, 3, 21, 89, 338, 1267, 4480, 15004, 47144 /), &
  mg53_dnb(0:9) = (/ 1, 3, 14, 58, 241, 971, 3854, 14725, 54266, &
    191567 /), &
  mg53_nb(0:9) = (/ 1, 4, 18, 76, 317, 1288, 5142, 19867, 74133, &
    265700 /)
! Note: there are also primaries at degrees 10, 12, and 15.
!..procedures
CONTAINS
include 'mg53_prims.f90'
include 'mg53_prib.f90'
include 'mg53_isecs.f90'
include 'mg53_secs.f90'
include 'mg53_base.f90'
END MODULE inv_mg53
