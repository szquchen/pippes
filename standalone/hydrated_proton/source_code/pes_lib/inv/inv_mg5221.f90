MODULE inv_mg5221
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg5221_prims, mg5221_prib, mg5221_isecs, mg5221_secs, &
  mg5221_base
!..data
integer, parameter, private :: nkk=4, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/5,2,2,1/)
integer, parameter, public :: &
  mg5221_id=115, &
  mg5221_nkk=nkk, mg5221_nk=nk, mg5221_nr=nr, mg5221_ngrp=120*4, &
  mg5221_nkj(0:nkk-1)=nkj, &
  mg5221_dnpr(0:9) = (/ 0, 9, 14, 5, 7, 5, 3, 0, 0, 0 /), &
  mg5221_npr(0:9) = (/ 0, 9, 23, 28, 35, 40, 43, 43, 43, 43 /), &
  mg5221_dnpb(0:9) = (/ 1, 9, 59, 296, 1282, 4905, 17149, 55497, &
    168672, 485254 /), &
  mg5221_npb(0:9) = (/ 1, 10, 69, 365, 1647, 6552, 23701, 79198, &
    247870, 733124 /), &
  mg5221_dnsc(0:9) = (/ 1, 0, 12, 74, 367, 1975, 10072, 47142, &
    205657, 830978 /), &
  mg5221_nsc(0:9) = (/ 1, 1, 13, 87, 454, 2429, 12501, 59643, &
    265300, 1096278 /), &
  mg5221_dnb(0:9) = (/ 1, 9, 71, 478, 3023, 18101, 103937, 572172, &
    3016707, 15196910 /), &
  mg5221_nb(0:9) = (/ 1, 10, 81, 559, 3582, 21683, 125620, 697792, &
    3714499, 18911409 /)
!..procedures
CONTAINS
include 'mg5221_prims.f90'
include 'mg5221_prib.f90'
include 'mg5221_isecs.f90'
include 'mg5221_secs.f90'
include 'mg5221_base.f90'
END MODULE inv_mg5221
