MODULE inv_mg621
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg621_prims, mg621_prib, mg621_isecs, mg621_secs, &
  mg621_base
!..data
integer, parameter, private :: nkk=3, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/6,2,1/)
integer, parameter, public :: &
  mg621_id=73, &
  mg621_nkk=nkk, mg621_nk=nk, mg621_nr=nr, mg621_ngrp=720*2, &
  mg621_nkj(0:nkk-1)=nkj, &
  mg621_dnpr(0:9) = (/ 0, 5, 7, 7, 6, 5, 5, 0, 0, 0 /), &
  mg621_npr(0:9) = (/ 0, 5, 12, 19, 25, 30, 35, 35, 35, 35 /), &
  mg621_dnpb(0:9) = (/ 1, 5, 22, 77, 244, 700, 1884, 4770, 11532, &
    26715 /), &
  mg621_npb(0:9) = (/ 1, 6, 28, 105, 349, 1049, 2933, 7703, 19235, &
    45950 /), &
  mg621_dnsc(0:9) = (/ 1, 0, 4, 24, 107, 451, 1932, 7781, 29929, &
    108067 /), &
  mg621_nsc(0:9) = (/ 1, 1, 5, 29, 136, 587, 2519, 10300, 40229, &
    148296 /), &
  mg621_dnb(0:9) = (/ 1, 5, 26, 121, 559, 2522, 11249, 49028, 208041, &
    853613 /), &
  mg621_nb(0:9) = (/ 1, 6, 32, 153, 712, 3234, 14483, 63511, 271552, &
    1125165 /)
!..procedures
CONTAINS
include 'mg621_prims.f90'
include 'mg621_prib.f90'
include 'mg621_isecs.f90'
include 'mg621_secs.f90'
include 'mg621_base.f90'
END MODULE inv_mg621
