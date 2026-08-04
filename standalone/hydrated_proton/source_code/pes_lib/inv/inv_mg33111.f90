MODULE inv_mg33111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg33111_prims, mg33111_prib, mg33111_isecs, mg33111_secs, &
  mg33111_base
!..data
integer, parameter, private :: nkk=5, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,3,1,1,1/)
integer, parameter, public :: &
  mg33111_id=87, &
  mg33111_nkk=nkk, mg33111_nk=nk, mg33111_nr=nr, mg33111_ngrp=36, &
  mg33111_nkj(0:nkk-1)=nkj, &
  mg33111_dnpr(0:9) = (/ 0, 12, 11, 11, 1, 0, 1, 0, 0, 0 /), &
  mg33111_npr(0:9) = (/ 0, 12, 23, 34, 35, 35, 36, 36, 36, 36 /), &
  mg33111_dnpb(0:9) = (/ 1, 12, 89, 507, 2422, 10155, 38437, 133818, &
    434306, 1327187 /), &
  mg33111_npb(0:9) = (/ 1, 13, 102, 609, 3031, 13186, 51623, 185441, &
    619747, 1946934 /), &
  mg33111_dnsc(0:9) = (/ 1, 0, 20, 95, 425, 2011, 7731, 25747, &
    77457, 205597 /), &
  mg33111_nsc(0:9) = (/ 1, 1, 21, 116, 541, 2552, 10283, 36030, &
    113487, 319084 /), &
  mg33111_dnb(0:9) = (/ 1, 12, 109, 842, 5767, 35861, 204730, 1079981, &
    5291178, 24187760 /), &
  mg33111_nb(0:9) = (/ 1, 13, 122, 964, 6731, 42592, 247322, 1327303, &
    6618481, 30806241 /)
!..procedures
CONTAINS
include 'mg33111_prims.f90'
include 'mg33111_prib.f90'
include 'mg33111_isecs.f90'
include 'mg33111_secs.f90'
include 'mg33111_base.f90'
END MODULE inv_mg33111
