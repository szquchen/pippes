subroutine potential(nw, box, r, rc, der, vir, watdm, upot)
use pot_ttm_mod, only: pot_ttm
implicit none
   integer, intent( in    ) :: nw
   double precision, dimension(3),     intent( in    ) :: box
   double precision, dimension(3,3*nw),   intent( in    ) :: r
   double precision, intent( in    ) :: rc
   double precision, dimension(3,3*nw),  intent(   out ) :: der
   double precision, dimension(3,3),   intent(   out ) :: vir
   double precision, dimension(3,nw),  intent(   out ) :: watdm
   double precision,          intent(   out ) :: upot
   !... local variables
   !type (t_ewaldenv) :: ewenv

   call pot_ttm(nw, box, r, rc, der, vir, watdm, upot)
!!   print*,'upot=', upot


end subroutine potential
