MODULE inv_mg42211
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg42211_prims, mg42211_prib, mg42211_isecs, mg42211_secs, &
  mg42211_base
!..data
integer, parameter, private :: nkk=5, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,2,2,1,1/)
integer, parameter, public :: &
  mg42211_id=123, &
  mg42211_nkk=nkk, mg42211_nk=nk, mg42211_nr=nr, mg42211_ngrp=24*4, &
  mg42211_nkj(0:nkk-1)=nkj, &
  mg42211_dnpr(0:9) = (/ 0, 13, 17, 6, 7, 0, 2, 0, 0, 0 /), &
  mg42211_npr(0:9) = (/ 0, 13, 30, 36, 43, 43, 45, 45, 45, 45 /), &
  mg42211_dnpb(0:9) = (/ 1, 13, 108, 682, 3605, 16651, 69231, 263989, &
    936212, 3119446 /), &
  mg42211_npb(0:9) = (/ 1, 14, 122, 804, 4409, 21060, 90291, 354280, &
    1290492, 4409938 /), &
  mg42211_dnsc(0:9) = (/ 1, 0, 22, 114, 645, 3500, 16980, 73010, &
    284201, 998547 /), &
  mg42211_nsc(0:9) = (/ 1, 1, 23, 137, 782, 4282, 21262, 94272, &
    378473, 1377020 /), &
  mg42211_dnb(0:9) = (/ 1, 13, 130, 1082, 8108, 55852, 358429, 2152921, &
    12136904, 64335533 /), &
  mg42211_nb(0:9) = (/ 1, 14, 144, 1226, 9334, 65186, 423615, 2576536, &
    14713440, 79048973 /)
!..procedures
CONTAINS
include 'mg42211_prims.f90'
include 'mg42211_prib.f90'
include 'mg42211_isecs.f90'
include 'mg42211_secs.f90'
include 'mg42211_base.f90'
END MODULE inv_mg42211
