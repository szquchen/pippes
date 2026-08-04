MODULE inv_mg5111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg5111_prims, mg5111_prib, mg5111_isecs, mg5111_secs, &
  mg5111_base
!..data
integer, parameter, private :: nkk=4, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/5,1,1,1/)
integer, parameter, public :: &
  mg5111_id=55, &
  mg5111_nkk=nkk, mg5111_nk=nk, mg5111_nr=nr, mg5111_ngrp=120, &
  mg5111_nkj(0:nkk-1)=nkj, &
  mg5111_dnpr(0:9) = (/ 0, 7, 5, 5, 5, 5, 1, 0, 0, 0 /), &
  mg5111_npr(0:9) = (/ 0, 7, 12, 17, 22, 27, 28, 28, 28, 28 /), &
  mg5111_dnpb(0:9) = (/ 1, 7, 33, 124, 405, 1192, 3240, 8253, 19921, &
    45924 /), &
  mg5111_npb(0:9) = (/ 1, 8, 41, 165, 570, 1762, 5002, 13255, 33176, &
    79100 /), &
  mg5111_dnsc(0:9) = (/ 1, 0, 6, 30, 120, 456, 1605, 5196, 15416, 41821 /), &
  mg5111_nsc(0:9) = (/ 1, 1, 7, 37, 157, 613, 2218, 7414, 22830, 64651 /), &
  mg5111_dnb(0:9) = (/ 1, 7, 39, 196, 933, 4222, 18147, 73914, 285018, &
    1040583 /), &
  mg5111_nb(0:9) = (/ 1, 8, 47, 243, 1176, 5398, 23545, 97459, 382477, &
    1423060 /)
!..procedures
CONTAINS
include 'mg5111_prims.f90'
include 'mg5111_prib.f90'
include 'mg5111_isecs.f90'
include 'mg5111_secs.f90'
include 'mg5111_base.f90'
END MODULE inv_mg5111
