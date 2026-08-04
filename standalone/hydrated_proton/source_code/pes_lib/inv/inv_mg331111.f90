MODULE inv_mg331111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg331111_prims, mg331111_prib, mg331111_isecs, mg331111_secs, &
  mg331111_base
!..data
integer, parameter, private :: nkk=6, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,3,1,1,1,1/)
integer, parameter, public :: &
  mg331111_id=129, &
  mg331111_nkk=nkk, mg331111_nk=nk, mg331111_nr=nr, mg331111_ngrp=6*6, &
  mg331111_nkj(0:nkk-1)=nkj, &
  mg331111_dnpr(0:9) = (/ 0, 17, 13, 13, 1, 0, 1, 0, 0, 0 /), &
  mg331111_npr(0:9) = (/ 0, 17, 30, 43, 44, 44, 45, 45, 45, 45 /), &
  mg331111_dnpb(0:9) = (/ 1, 17, 166, 1203, 7147, 36668, 167704, &
    698400, 2688598, 9675914 /), &
  mg331111_npb(0:9) = (/ 1, 18, 184, 1387, 8534, 45202, 212906, &
    911306, 3599904, 13275818 /), &
  mg331111_dnsc(0:9) = (/ 1, 0, 30, 148, 807, 4414, 19415, &
    75606, 264375, 815767 /), &
  mg331111_nsc(0:9) = (/ 1, 1, 31, 179, 986, 5400, 24815, &
    100421, 364796, 1180563 /), &
  mg331111_dnb(0:9) = (/ 1, 17, 196, 1861, 15450, 115459, 788573, &
    4965402, 28996820, 157803023 /), &
  mg331111_nb(0:9) = (/ 1, 18, 214, 2075, 17525, 132984, 921557, &
    5886959, 34883779, 192686802 /)
!..procedures
CONTAINS
include 'mg331111_prims.f90'
include 'mg331111_prib.f90'
include 'mg331111_isecs.f90'
include 'mg331111_secs.f90'
include 'mg331111_base.f90'
END MODULE inv_mg331111
