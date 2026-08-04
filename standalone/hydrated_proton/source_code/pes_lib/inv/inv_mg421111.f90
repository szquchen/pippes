MODULE inv_mg421111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg421111_prims, mg421111_prib, mg421111_isecs, mg421111_secs, &
  mg421111_base
!..data
integer, parameter, private :: nkk=6, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,2,1,1,1,1/)
integer, parameter, public :: &
  mg421111_id=128, &
  mg421111_nkk=nkk, mg421111_nk=nk, mg421111_nr=nr, mg421111_ngrp=24*2, &
  mg421111_nkj(0:nkk-1)=nkj, &
  mg421111_dnpr(0:9) = (/ 0, 17, 13, 7, 7, 0, 1, 0, 0, 0 /), &
  mg421111_npr(0:9) = (/ 0, 17, 30, 37, 44, 44, 45, 45, 45, 45 /), &
  mg421111_dnpb(0:9) = (/ 1, 17, 166, 1197, 7051, 35774, 161497, 662955, &
    2513371, 8902104 /), &
  mg421111_npb(0:9) = (/ 1, 18, 184, 1381, 8432, 44206, 205703, 868658, &
    3382029, 12284133 /), &
  mg421111_dnsc(0:9) = (/ 1, 0, 25, 108, 675, 3372, 15247, 60087, &
    214251, 680494 /), &
  mg421111_nsc(0:9) = (/ 1, 1, 26, 134, 809, 4181, 19428, 79515, &
    293766, 974260 /), &
  mg421111_dnb(0:9) = (/ 1, 17, 191, 1730, 13712, 98474, 651669, 4005826, &
    22976829, 123388939 /), &
  mg421111_nb(0:9) = (/ 1, 18, 209, 1939, 15651, 114125, 765794, 4771620, &
    27748449, 151137388 /)
!..procedures
CONTAINS
include 'mg421111_prims.f90'
include 'mg421111_prib.f90'
include 'mg421111_isecs.f90'
include 'mg421111_secs.f90'
include 'mg421111_base.f90'
END MODULE inv_mg421111
