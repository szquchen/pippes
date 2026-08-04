MODULE inv_mg4321
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg4321_prims, mg4321_prib, mg4321_isecs, mg4321_secs, &
  mg4321_base
!..data
integer, parameter, private :: nkk=4, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,3,2,1/)
integer, parameter, public :: &
  mg4321_id=116, &
  mg4321_nkk=nkk, mg4321_nk=nk, mg4321_nr=nr, mg4321_ngrp=24*6*2, &
  mg4321_nkj(0:nkk-1)=nkj, &
  mg4321_dnpr(0:9) = (/ 0, 9, 15, 10, 7, 0, 3, 0, 0, 0 /), &
  mg4321_npr(0:9) = (/ 0, 9, 24, 34, 41, 41, 44, 44, 44, 44 /), &
  mg4321_dnpb(0:9) = (/ 1, 9, 60, 310, 1387, 5505, 19986, 67252, &
    212563, 636091 /), &
  mg4321_npb(0:9) = (/ 1, 10, 70, 380, 1767, 7272, 27258, 94510, &
    307073, 943164 /), &
  mg4321_dnsc(0:9) = (/ 1, 0, 15, 101, 518, 2861, 14966, 70349, &
    305117, 1217592 /), &
  mg4321_nsc(0:9) = (/ 1, 1, 16, 117, 635, 3496, 18462, 88811, &
    393928, 1611520 /), &
  mg4321_dnb(0:9) = (/ 1, 9, 75, 546, 3714, 23738, 143896, 827197, &
    4509952, 23307299 /), &
  mg4321_nb(0:9) = (/ 1, 10, 85, 631, 4345, 28083, 171979, 999176, &
    5509128, 28816427 /)
!..procedures
CONTAINS
include 'mg4321_prims.f90'
include 'mg4321_prib.f90'
include 'mg4321_isecs.f90'
include 'mg4321_secs.f90'
include 'mg4321_base.f90'
END MODULE inv_mg4321
