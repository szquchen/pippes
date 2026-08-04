MODULE inv_mg522
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg522_prims, mg522_prib, mg522_isecs, mg522_secs, &
  mg522_base
!..data
integer, parameter, private :: nkk=3, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/5,2,2/)
integer, parameter, public :: &
  mg522_id=76, &
  mg522_nkk=nkk, mg522_nk=nk, mg522_nr=nr, mg522_ngrp=120*2*2, &
  mg522_nkj(0:nkk-1)=nkj, &
  mg522_dnpr(0:9) = (/ 0, 6, 11, 4, 6, 4, 3, 0, 0, 0 /), &
  mg522_npr(0:9) = (/ 0, 6, 17, 21, 27, 31, 34, 34, 34, 34 /), &
  mg522_dnpb(0:9) = (/ 1, 6, 32, 126, 453, 1432, 4237, 11630, 30371, &
    75356 /), &
  mg522_npb(0:9) = (/ 1, 7, 39, 165, 618, 2050, 6287, 17917, 48288, &
    123644 /), &
  mg522_dnsc(0:9) = (/ 1, 0, 5, 41, 171, 756, 3359, 13584, 50979, &
    177237 /), &
  mg522_nsc(0:9) = (/ 1, 1, 6, 47, 218, 974, 4333, 17917, 68896, &
    246133 /), &
  mg522_dnb(0:9) = (/ 1, 6, 37, 197, 1030, 5156, 25035, 116839, 522958, &
    2235596 /), &
  mg522_nb(0:9) = (/ 1, 7, 44, 241, 1271, 6427, 31462, 148301, 671259, &
    2906855 /)
!..procedures
CONTAINS
include 'mg522_prims.f90'
include 'mg522_prib.f90'
include 'mg522_isecs.f90'
include 'mg522_secs.f90'
include 'mg522_base.f90'
END MODULE inv_mg522
