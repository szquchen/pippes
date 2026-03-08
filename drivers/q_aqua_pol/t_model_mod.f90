module t_model_mod
implicit none

type t_model
   character(len=20) :: model
   integer           :: nbeads, ncore
!   logical           :: vlist
   double precision  :: rcut, scut, scut_thole
   integer           :: polar_maxiter
   integer           :: debug
   double precision  :: ewald_eps
   double precision  :: rskin
   logical           :: use_vlist
end type t_model

end module t_model_mod
