MODULE inv_mg433
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg433_prims, mg433_prib, mg433_isecs, mg433_secs, &
  mg433_base
!..data
integer, parameter, private :: nkk=3, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,3,3/)
integer, parameter, public :: &
  mg433_id=110, &
  mg433_nkk=nkk, mg433_nk=nk, mg433_nr=nr, mg433_ngrp=24*6*6, &
  mg433_nkj(0:nkk-1)=nkj, &
  mg433_dnpr(0:9) = (/ 0, 6, 13, 13, 8, 0, 3, 0, 0, 0 /), &
  mg433_npr(0:9) = (/ 0, 6, 19, 32, 40, 40, 43, 43, 43, 43 /), &
  mg433_dnpb(0:9) = (/ 1, 6, 34, 147, 576, 2016, 6574, 20004, &
    57748, 158738 /), &
  mg433_npb(0:9) = (/ 1, 7, 41, 188, 764, 2780, 9354, 29358, &
    87106, 245844 /), &
  mg433_dnsc(0:9) = (/ 1, 0, 9, 75, 372, 1984, 10542, 51555, &
    236124, 1007927 /), &
  mg433_nsc(0:9) = (/ 1, 1, 10, 85, 457, 2441, 12983, 64538, &
    300662, 1308589 /), &
  mg433_dnb(0:9) = (/ 1, 6, 43, 276, 1704, 10105, 57877, 318295, &
    1677916, 8451775 /), &
  mg433_nb(0:9) = (/ 1, 7, 50, 326, 2030, 12135, 70012, 388307, &
    2066223, 10517998 /)
!..procedures
CONTAINS
include 'mg433_prims.f90'
include 'mg433_prib.f90'
include 'mg433_isecs.f90'
include 'mg433_secs.f90'
include 'mg433_base.f90'
END MODULE inv_mg433
