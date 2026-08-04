MODULE inv_mgx
!..use and access
use inv_core
implicit none
private
public :: mgx_id, mgx_mk1d, mgx_mk2d, mgx_mkrl1d, mgx_mkrl2d, &
  mgx_ngen, mgx_gens, mgx_gens2d, mgx_setd
!..types
type, public :: mgx_t
 integer :: nkk, nk, nr, nkj(0:9), ngrp, ngen, &
   dnpr(0:9), npr(0:9), dnpb(0:9), npb(0:9), dnsc(0:9), nsc(0:9), &
   dnb(0:9), nb(0:9)
 integer, pointer :: gens(:,:), mk1d(:,:)
end type mgx_t
!..procedures
CONTAINS
include 'mgx_id.f90'
include 'mgx_mk1d.f90'
include 'mgx_mk2d.f90'
include 'mgx_mkrl1d.f90'
include 'mgx_mkrl2d.f90'
include 'mgx_ngen.f90'
include 'mgx_gens.f90'
include 'mgx_gens2d.f90'
include 'mgx_setd.f90'
END MODULE inv_mgx
