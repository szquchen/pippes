MODULE inv_mg622
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg622_prims, mg622_prib, mg622_isecs, mg622_secs, &
  mg622_base
!..data
integer, parameter, private :: nkk=3, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/6,2,2/)
integer, parameter, public :: &
  mg622_id=107, &
  mg622_nkk=nkk, mg622_nk=nk, mg622_nr=nr, mg622_ngrp=720*4, &
  mg622_nkj(0:nkk-1)=nkj, &
  mg622_dnpr(0:9) = (/ 0, 6, 11, 8, 7, 5, 6, 0, 0, 0 /), &
  mg622_npr(0:9) = (/ 0, 6, 17, 25, 32, 37, 43, 43, 43, 43 /), &
  mg622_dnpb(0:9) = (/ 1, 6, 32, 130, 478, 1567, 4792, 13682, &
    37123, 96018 /), &
  mg622_npb(0:9) = (/ 1, 7, 39, 169, 647, 2214, 7006, 20688, &
    57811, 153829 /), &
  mg622_dnsc(0:9) = (/ 1, 0, 5, 38, 180, 852, 4206, 19731, &
    88469, 375003 /), &
  mg622_nsc(0:9) = (/ 1, 1, 6, 44, 224, 1076, 5282, 25013, &
    113482, 488485 /), &
  mg622_dnb(0:9) = (/ 1, 6, 37, 198, 1046, 5365, 27200, 135312, &
    658876, 3119829 /), &
  mg622_nb(0:9) = (/ 1, 7, 44, 242, 1288, 6653, 33853, 169165, &
    828041, 3947870 /)
!..procedures
CONTAINS
include 'mg622_prims.f90'
include 'mg622_prib.f90'
include 'mg622_isecs.f90'
include 'mg622_secs.f90'
include 'mg622_base.f90'
END MODULE inv_mg622
