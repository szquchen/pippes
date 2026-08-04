MODULE inv_mg3111111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg3111111_prims, mg3111111_prib, mg3111111_isecs, mg3111111_secs, &
  mg3111111_base
!..data
integer, parameter, private :: nkk=7, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,1,1,1,1,1,1/)
integer, parameter, public :: &
  mg3111111_id=93, &
  mg3111111_nkk=nkk, mg3111111_nk=nk, mg3111111_nr=nr, mg3111111_ngrp=6, &
  mg3111111_nkj(0:nkk-1)=nkj, &
  mg3111111_dnpr(0:9) = (/ 0, 22, 7, 7, 0, 0, 0, 0, 0, 0 /), &
  mg3111111_npr(0:9) = (/ 0, 22, 29, 36, 36, 36, 36, 36, 36, 36 /), &
  mg3111111_dnpb(0:9) = (/ 1, 22, 260, 2185, 14603, 82384, 407002, &
    1804779, 7311105, 27415070 /), &
  mg3111111_npb(0:9) = (/ 1, 23, 283, 2468, 17071, 99455, 506457, &
    2311236, 9622341, 37037411 /), &
  mg3111111_dnsc(0:9) = (/ 1, 0, 21, 77, 231, 735, 1638, &
    2793, 4557, 6293 /), &
  mg3111111_nsc(0:9) = (/ 1, 1, 22, 99, 330, 1065, 2703, &
    5496, 10053, 16346 /), &
  mg3111111_dnb(0:9) = (/ 1, 22, 281, 2724, 21988, 154106, 959778, &
    5393938, 27672866, 130830249 /), &
  mg3111111_nb(0:9) = (/ 1, 23, 304, 3028, 25016, 179122, 1138900, &
    6532838, 34205704, 165035953 /)
!..procedures
CONTAINS
include 'mg3111111_prims.f90'
include 'mg3111111_prib.f90'
include 'mg3111111_isecs.f90'
include 'mg3111111_secs.f90'
include 'mg3111111_base.f90'
END MODULE inv_mg3111111
