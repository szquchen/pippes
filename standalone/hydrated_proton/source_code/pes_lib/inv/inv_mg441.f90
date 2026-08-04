MODULE inv_mg441
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg441_prims, mg441_prib, mg441_isecs, mg441_secs, &
  mg441_base
!..data
integer, parameter, private :: nkk=3, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,4,1/)
integer, parameter, public :: &
  mg441_id=75, &
  mg441_nkk=nkk, mg441_nk=nk, mg441_nr=nr, mg441_ngrp=24*24, &
  mg441_nkj(0:nkk-1)=nkj, &
  mg441_dnpr(0:9) = (/ 0, 5, 9, 11, 9, 0, 1, 0, 0, 0 /), &
  mg441_npr(0:9) = (/ 0, 5, 14, 25, 34, 34, 35, 35, 35, 35 /), &
  mg441_dnpb(0:9) = (/ 1, 5, 24, 91, 314, 975, 2843, 7768, 20229, &
    50337 /), &
  mg441_npb(0:9) = (/ 1, 6, 30, 121, 435, 1410, 4253, 12021, 32250, &
    82587 /), &
  mg441_dnsc(0:9) = (/ 1, 0, 6, 41, 188, 830, 3706, 15307, 58747, &
    208730 /), &
  mg441_nsc(0:9) = (/ 1, 1, 7, 48, 236, 1066, 4772, 20079, 78826, &
    287556 /), &
  mg441_dnb(0:9) = (/ 1, 5, 30, 162, 851, 4275, 20826, 97357, 436050, &
    1864507 /), &
  mg441_nb(0:9) = (/ 1, 6, 36, 198, 1049, 5324, 26150, 123507, 559557, &
    2424064 /)
!..procedures
CONTAINS
include 'mg441_prims.f90'
include 'mg441_prib.f90'
include 'mg441_isecs.f90'
include 'mg441_secs.f90'
include 'mg441_base.f90'
END MODULE inv_mg441
