MODULE inv_mg322111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg322111_prims, mg322111_prib, mg322111_isecs, mg322111_secs, &
  mg322111_base
!..data
integer, parameter, private :: nkk=6, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,2,2,1,1,1/)
integer, parameter, public :: &
  mg322111_id=130, &
  mg322111_nkk=nkk, mg322111_nk=nk, mg322111_nr=nr, mg322111_ngrp=6*2*2, &
  mg322111_nkj(0:nkk-1)=nkj, &
  mg322111_dnpr(0:9) = (/ 0, 18, 19, 6, 0, 0, 2, 0, 0, 0 /), &
  mg322111_npr(0:9) = (/ 0, 18, 37, 43, 43, 43, 45, 45, 45, 45 /), &
  mg322111_dnpb(0:9) = (/ 1, 18, 190, 1488, 9532, 52554, 257397, &
    1143948, 4684317, 17876902 /), &
  mg322111_npb(0:9) = (/ 1, 19, 209, 1697, 11229, 63783, 321180, &
    1465128, 6149445, 24026347 /), &
  mg322111_dnsc(0:9) = (/ 1, 0, 35, 148, 894, 4374, 17876, &
    62691, 192926, 517135 /), &
  mg322111_nsc(0:9) = (/ 1, 1, 36, 184, 1078, 5452, 23328, &
    86019, 278945, 796080 /), &
  mg322111_dnb(0:9) = (/ 1, 18, 225, 2266, 19740, 153220, 1077709, &
    6939865, 41219128, 227186663 /), &
  mg322111_nb(0:9) = (/ 1, 19, 244, 2510, 22250, 175470, 1253179, &
    8193044, 49412172, 276598835 /)
!..procedures
CONTAINS
include 'mg322111_prims.f90'
include 'mg322111_prib.f90'
include 'mg322111_isecs.f90'
include 'mg322111_secs.f90'
include 'mg322111_base.f90'
END MODULE inv_mg322111
