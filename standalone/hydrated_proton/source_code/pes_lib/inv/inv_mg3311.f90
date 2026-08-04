MODULE inv_mg3311
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg3311_prims, mg3311_prib, mg3311_isecs, mg3311_secs, &
  mg3311_base
!..data
integer, parameter, private :: nkk=4, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,3,1,1/)
integer, parameter, public :: &
  mg3311_id=57, &
  mg3311_nkk=nkk, mg3311_nk=nk, mg3311_nr=nr, mg3311_ngrp=36, &
  mg3311_nkj(0:nkk-1)=nkj, &
  mg3311_dnpr(0:9) = (/ 0, 8, 9, 9, 1, 0, 1, 0, 0, 0 /), &
  mg3311_npr(0:9) = (/ 0, 8, 17, 26, 27, 27, 28, 28, 28, 28 /), &
  mg3311_dnpb(0:9) = (/ 1, 8, 45, 201, 772, 2645, 8290, 24140, 66094, &
    171618 /), &
  mg3311_npb(0:9) = (/ 1, 9, 54, 255, 1027, 3672, 11962, 36102, 102196, &
    273814 /), &
  mg3311_dnsc(0:9) = (/ 1, 0, 12, 56, 202, 789, 2607, 7256, 18269, &
    40659 /), &
  mg3311_nsc(0:9) = (/ 1, 1, 13, 69, 271, 1060, 3667, 10923, 29192, &
    69851 /), &
  mg3311_dnb(0:9) = (/ 1, 8, 57, 353, 1962, 9982, 46819, 203331, &
    821859, 3106274 /), &
  mg3311_nb(0:9) = (/ 1, 9, 66, 419, 2381, 12363, 59182, 262513, &
    1084372, 4190646 /)
!..procedures
CONTAINS
include 'mg3311_prims.f90'
include 'mg3311_prib.f90'
include 'mg3311_isecs.f90'
include 'mg3311_secs.f90'
include 'mg3311_base.f90'
END MODULE inv_mg3311
