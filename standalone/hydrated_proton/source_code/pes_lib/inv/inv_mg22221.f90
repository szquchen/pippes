MODULE inv_mg22221
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg22221_prims, mg22221_prib, mg22221_isecs, mg22221_secs, &
  mg22221_base
!..data
integer, parameter, private :: nkk=5, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,2,2,2,1/)
integer, parameter, public :: &
  mg22221_id=89, &
  mg22221_nkk=nkk, mg22221_nk=nk, mg22221_nr=nr, mg22221_ngrp=16, &
  mg22221_nkj(0:nkk-1)=nkj, &
  mg22221_dnpr(0:9) = (/ 0, 14, 22, 0, 0, 0, 0, 0, 0, 0 /), &
  mg22221_npr(0:9) = (/ 0, 14, 36, 36, 36, 36, 36, 36, 36, 36 /), &
  mg22221_dnpb(0:9) = (/ 1, 14, 127, 868, 4943, 24430, 108081, &
    436032, 1627704, 5681104 /), &
  mg22221_npb(0:9) = (/ 1, 15, 142, 1010, 5953, 30383, 138464, &
    574496, 2202200, 7883304 /), &
  mg22221_dnsc(0:9) = (/ 1, 0, 24, 100, 415, 1632, 4656, &
    10736, 20250, 30880 /), &
  mg22221_nsc(0:9) = (/ 1, 1, 25, 125, 540, 2172, 6828, &
    17564, 37814, 68694 /), &
  mg22221_dnb(0:9) = (/ 1, 14, 151, 1304, 9806, 65404, 393722, &
    2160056, 10894435, 50878658 /), &
  mg22221_nb(0:9) = (/ 1, 15, 166, 1470, 11276, 76680, 470402, &
    2630458, 13524893, 64403551 /)
!..procedures
CONTAINS
include 'mg22221_prims.f90'
include 'mg22221_prib.f90'
include 'mg22221_isecs.f90'
include 'mg22221_secs.f90'
include 'mg22221_base.f90'
END MODULE inv_mg22221
