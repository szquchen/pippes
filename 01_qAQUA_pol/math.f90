module math_mod

double precision, parameter :: g23=1.3541179394264d0


Contains
   !!!!!!!!!!!!!!!!!!!!!!!!!!!
   double precision function erfc_func( X)
   implicit none
   double precision :: x
   
   erfc_func = erfc1(x)
!   erfc_func = erfc(x)
   
   end function erfc_func
   !    *******************************************************************
   !    ** APPROXIMATION TO THE COMPLEMENTARY ERROR FUNCTION             **
   !    **                                                               **
   !    ** REFERENCE:                                                    **
   !    **                                                               **
   !    ** ABRAMOWITZ AND STEGUN, HANDBOOK OF MATHEMATICAL FUNCTIONS,    **
   !    **    NATIONAL BUREAU OF STANDARDS, FORMULA 7.1.26               **
   !    *******************************************************************
   FUNCTION ERFC1 ( X )
   implicit none
   
   double precision :: erfc1
   double precision, parameter ::  A1 = 0.254829592d0, A2 = -0.284496736d0, &
                                   A3 = 1.421413741d0, A4 = -1.453152027d0, &
                                   A5 = 1.061405429d0, P  =  0.3275911d0
   
   double precision :: T, X, XSQ, TP
   
   
   T  = 1.d0 / ( 1.d0 + P * X )
   XSQ = X * X
   
   TP = T * ( A1 + T * ( A2 + T * ( A3 + T * ( A4 + T * A5 ) ) ) )
   
   ERFC1 = TP * exp ( -XSQ )
   
   RETURN
   END function erfc1
   !.........................................................
    subroutine outprod3(a1,a2,a3)
   !.........................................................
   implicit none
   double precision, dimension(3), intent(in) :: a1, a2
   double precision, dimension(3), intent(out) :: a3
   
   a3(1) =  a1(2)*a2(3) - a1(3)*a2(2)
   a3(2) = -a1(1)*a2(3) + a1(3)*a2(1)
   a3(3) =  a1(1)*a2(2) - a1(2)*a2(1)
   
   end  subroutine outprod3
   !*****************************************************************************
   !*** The following subroutines used for the calculation of the Gamma function
   !*** have been taken from the "Numerical Recipes"   
   !*****************************************************************************
   !--------------------------------------------------
   function gammln(XX)
   
   !***   taken from the "Numerical Recipes"
   
   !--------------------------------------------------
   implicit none
   double precision :: XX
   double precision :: GAMMLN
   integer :: J
   double precision :: SER,STP,TMP,X,Y
   double precision, DIMENSION(6) :: COF
   SAVE cof,stp
   data COF,STP/76.18009172947146D0,-86.50532032941677d0,24.01409824083091D0,  &
               -1.231739572450155D0,.1208650973866179D-2,-.5395239384953D-5, &
               2.5066282746310005D0/
   !
   !-------- Executable code
   !
   X=XX
   Y=X
   TMP=X+5.5D0
   TMP=(X+0.5D0)*DLOG(TMP)-TMP
   SER=1.000000000190015D0
   do j=1,6
      Y=Y+1.D0
      SER=SER+COF(J)/Y
   enddo
   GAMMLN=TMP+DLOG(STP*SER/X)
   RETURN
   END FUNCTION gammln
   
   !--------------------------------------------------
   function gammq2_3(x)
   
   !***   taken from the "Numerical Recipes"
   
   !--------------------------------------------------
   implicit none
   double precision :: gammq2_3
   double precision :: a, x
   double precision :: gamser, gammcf
   
   a=2.d0/3.d0
   if (x<0.d0 .or. a <= 0.d0) stop 'bad arguments in gammq'
   if (x<a+1.d0) then
      call gser2_3(gamser, x)
      gammq2_3 = 1.d0 - gamser
   else
      call gcf2_3(gammcf, x)
      gammq2_3 = gammcf
   endif
   return
   end function gammq2_3
   
   !--------------------------------------------------
   subroutine gser2_3(gamser, x)
   
   !***   taken from the "Numerical Recipes"
   
   !--------------------------------------------------
   implicit none
   double precision :: gamser
   double precision :: a, x, gln
   integer          :: n
   integer, parameter :: ITMAX = 800
   double precision, parameter :: EPS = 3.d-7
   double precision :: ap, ssum, del
   
   a=2.d0/3.d0
   !gln = gammln(a)
   gln=0.303150275147547d0
   !print*,'gammln=',gammln(a)
   if (x <= 0.d0) then
      if (x < 0.d0) stop 'x<0 in gser'
      gamser = 0.d0
      return
   endif
   
   ap = a
   ssum = 1.d0 / a
   del =ssum
   
   do n=1, ITMAX
      ap = ap+1.d0
      del = del*x/ap
      ssum = ssum+del
      if (dabs(del) .lt. dabs(ssum)*EPS) goto 1
   enddo
   stop 'a too large, ITMAX too small in gser'
   1 gamser = ssum*dexp(-x+a*dlog(x)-gln)
   return
   end subroutine gser2_3
   
   !--------------------------------------------------
   subroutine gcf2_3(gammcf, x)
   !***   taken from the "Numerical Recipes"
   !--------------------------------------------------
   implicit none
   double precision :: gammcf
   double precision :: a, x, gln
   integer          :: i
   double precision :: an, b, c, d, del, h
   integer,parameter :: ITMAX = 100
   double precision, parameter :: EPS = 3.d-7, FPMIN=1.d-30
   
   a=2.d0/3.d0
   !gln = gammln(a)
   gln=0.303150275147547d0
   b = x+1.d0-a
   c = 1.d0/FPMIN
   d = 1.d0/b
   h=d
   do i=1, ITMAX
      an = -i*(i-a)
      b = b+2.d0
      d = an*d+b
      if (dabs(d) < FPMIN) d=FPMIN
      c = b+an/c
      if (dabs(c) < FPMIN) c=FPMIN
      d = 1.d0/d
      del = d*c
      h = h*del
      if (dabs(del-1.d0) < EPS) goto 1
   enddo
   stop ' a too large ITMAX too small in gcf'
   1 gammcf=dexp(-x+a*dlog(x)-gln)*h
   return
   end subroutine gcf2_3
   !.........................................................
    subroutine matr3inv(aa,ait)
   !.........................................................
    implicit none
    double precision :: aa(3,3),ait(3,3)
    double precision :: dd,t1,t2,t3
      
   
    t1 = aa(2,2) * aa(3,3) - aa(3,2) * aa(2,3)
    t2 = aa(3,2) * aa(1,3) - aa(1,2) * aa(3,3)
    t3 = aa(1,2) * aa(2,3) - aa(2,2) * aa(1,3)
    dd  = 1.d0/ (aa(1,1) * t1 + aa(2,1) * t2 + aa(3,1) * t3)
    ait(1,1) = t1 * dd
    ait(2,1) = t2 * dd
    ait(3,1) = t3 * dd
    ait(1,2) = (aa(3,1)*aa(2,3)-aa(2,1)*aa(3,3)) * dd
    ait(2,2) = (aa(1,1)*aa(3,3)-aa(3,1)*aa(1,3)) * dd
    ait(3,2) = (aa(2,1)*aa(1,3)-aa(1,1)*aa(2,3)) * dd
    ait(1,3) = (aa(2,1)*aa(3,2)-aa(3,1)*aa(2,2)) * dd
    ait(2,3) = (aa(3,1)*aa(1,2)-aa(1,1)*aa(3,2)) * dd
    ait(3,3) = (aa(1,1)*aa(2,2)-aa(2,1)*aa(1,2)) * dd
   
    end subroutine
   !.........................................................
   function matr3det(a)
   !.........................................................
   implicit none
   double precision :: matr3det
   double precision, dimension(3,3) :: a
   
   matr3det = a(1,1)*a(2,2)*a(3,3) - a(1,1)*a(2,3)*a(3,2) - &
              a(3,3)*a(1,2)*a(2,1) - a(3,1)*a(2,2)*a(1,3) + &
              a(3,1)*a(1,2)*a(2,3) + a(1,3)*a(2,1)*a(3,2)
   
   end function matr3det
   !.........................................................
   integer function binomial_coeff(n, k)
   !.........................................................
   integer :: n, k
   !...
   integer :: i
   double precision :: pn, pk, pn_k

   pn=1
   do i=2, n
      pn=pn*i
   enddo
   pk = 1
   do i=2, k
      pk=pk*i
   enddo

   pn_k=1
   do i=2, n-k
     pn_k=pn_k*i
   enddo
!   print*,'pn=', pn, pk, pn_k
   binomial_coeff=dble(pn)/dble(pk*pn_k)
   end function binomial_coeff


end module math_mod
