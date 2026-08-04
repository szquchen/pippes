MODULE pes2
!..use and access
use inv, wp=>inv_wp
use pes0
implicit none
private
public :: pes2_init, pes2_reinit, pes2_readf, pes2_readf_old, &
  pes2_grad, pes2_grad2, pes2_errf, pes2_errvf
!..data
save
integer, parameter, public :: pes2_nd=3
integer, allocatable, public :: pes2_ikord(:), pes2_ikord2(:)
logical, public :: &
  pes2_havegf=.false., pes2_haveg2f=.false., pes2_havedip=.false.
real (kind=wp), public :: &
  pes2_dwt(0:1)=(/0.1_wp,1.0_wp/), &
  pes2_dfdis=1.0e-3_wp, pes2_df2dis=5.0e-3_wp
real (kind=wp), allocatable, public :: &
  pes2_xn(:,:,:), pes2_f(:), pes2_gf(:,:,:), pes2_g2f(:,:,:), &
  pes2_df(:), pes2_dgf(:,:,:), pes2_dg2f(:,:,:), &
  pes2_dip(:,:), pes2_ddip(:,:)
character (len=32), allocatable, public :: &
  pes2_lab(:)
real (kind=wp), public :: &
  pes2_fmin=huge(0.0_wp), pes2_fmax=-huge(0.0_wp)
!..procedures
CONTAINS
include 'pes2_init.f90'
include 'pes2_reinit.f90'
include 'pes2_readf.f90'
include 'pes2_readf_old.f90'
include 'pes2_grad.f90'
include 'pes2_grad2.f90'
include 'pes2_errf.f90'
include 'pes2_errvf.f90'
END MODULE pes2
