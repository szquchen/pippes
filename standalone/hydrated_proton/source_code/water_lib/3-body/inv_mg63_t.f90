MODULE inv_mg63_t
!..use and access
use inv_wp_t
use inv_mgx_t
implicit none
private
public :: mg63_prims, mg63_prib, mg63_secs, mg63_base, &
     mg63_msecs, mg63_isecs,mg63_isecs5, mg63_isecs6
!..data
integer, parameter, private :: nkk=2, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/6,3/)
integer, parameter, public :: &
  mg63_id=70, &
  mg63_nkk=nkk, mg63_nk=nk, mg63_nr=nr, mg63_ngrp=720*6, &
  mg63_nkj(0:nkk-1)=nkj, &
  mg63_dnpr(0:9) = (/ 0, 3, 6, 8, 7, 5, 4, 0, 0, 0 /), &
  mg63_npr(0:9) = (/ 0, 3, 9, 17, 24, 29, 33, 33, 33, 33 /), &
  mg63_dnpb(0:9) = (/ 1, 3, 12, 36, 103, 266, 663, 1548, 3502, 7601 /), &
  mg63_npb(0:9) = (/ 1, 4, 16, 52, 155, 421, 1084, 2632, 6134, 13735 /), &
  mg63_dnsc(0:9) = (/ 1, 0, 2, 17, 72, 295, 1239, 4985, 19528, 72756 /), &
  mg63_nsc(0:9) = (/ 1, 1, 3, 20, 92, 387, 1626, 6611, 26139, 98895 /), &
  mg63_dnb(0:9) = (/ 1, 3, 14, 59, 250, 1053, 4469, 18665, 76737, &
    307269 /), &
  mg63_nb(0:9) = (/ 1, 4, 18, 77, 327, 1380, 5849, 24514, 101251, &
    408520 /)
! Note: there are also primaries at degrees 10, 12, and 15.

integer,save, private :: mg63_mii(0:4,0:719), mg63_miij0(0:5,0:2159), &
     mg63_miijj(0:7,0:4319)
!..procedures
CONTAINS
include 'mg63_prims.f90'
include 'mg63_prib.f90'
include 'mg63_isecs.f90'
include 'mg63_secs.f90'
include 'mg63_base.f90'
include 'mg63_msecs.f90'
include 'mg63_isecs5_nonmp.f90'
include 'mg63_isecs6_nonmp.f90'
END MODULE inv_mg63_t
