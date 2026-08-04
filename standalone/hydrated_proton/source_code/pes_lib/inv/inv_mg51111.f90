MODULE inv_mg51111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg51111_prims, mg51111_prib, mg51111_isecs, mg51111_secs, &
  mg51111_base
!..data
integer, parameter, private :: nkk=5, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/5,1,1,1,1/)
integer, parameter, public :: &
  mg51111_id=85, &
  mg51111_nkk=nkk, mg51111_nk=nk, mg51111_nr=nr, mg51111_ngrp=120, &
  mg51111_nkj(0:nkk-1)=nkj, &
  mg51111_dnpr(0:9) = (/ 0, 11, 6, 6, 6, 6, 1, 0, 0, 0 /), &
  mg51111_npr(0:9) = (/ 0, 11, 17, 23, 29, 35, 36, 36, 36, 36 /), &
  mg51111_dnpb(0:9) = (/ 1, 11, 72, 358, 1490, 5454, 18088, 55418, &
    159006, 431536 /), &
  mg51111_npb(0:9) = (/ 1, 12, 84, 442, 1932, 7386, 25474, 80892, &
    239898, 671434 /), &
  mg51111_dnsc(0:9) = (/ 1, 0, 10, 50, 235, 1061, 4361, 16431, &
    56597, 178367 /), &
  mg51111_nsc(0:9) = (/ 1, 1, 11, 61, 296, 1357, 5718, 22149, &
    78746, 257113 /), &
  mg51111_dnb(0:9) = (/ 1, 11, 82, 518, 2995, 16280, 83840, 409382, &
    1893904, 8297900 /), &
  mg51111_nb(0:9) = (/ 1, 12, 94, 612, 3607, 19887, 103727, 513109, &
    2407013, 10704913 /)
!..procedures
CONTAINS
include 'mg51111_prims.f90'
include 'mg51111_prib.f90'
include 'mg51111_isecs.f90'
include 'mg51111_secs.f90'
include 'mg51111_base.f90'
END MODULE inv_mg51111
