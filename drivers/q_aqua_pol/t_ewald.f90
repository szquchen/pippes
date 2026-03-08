module t_ewald_mod
implicit none

   type t_ewaldenv
      double precision :: a, eps, rc
      integer :: nkvec, hmax, kmax, lmax
      integer, dimension(:,:), allocatable :: k_hkl
      double precision, dimension(:,:), allocatable :: kvec
      double precision, dimension(:), allocatable :: Ak, rQq, iQq, rQm, iQm
      double precision, dimension(3) :: box_init   ! keeps the initialization gprimd
      integer :: debug
   end type t_ewaldenv

   type t_ew
      !*   for STD *!
      double precision, dimension(:,:), allocatable :: &
                                    coskx, cosky, coskz, sinkx, sinky, sinkz
   end type t_ew

end module t_ewald_mod
