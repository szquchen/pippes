module shell_hww
use bemsa
use constants
implicit none

  real::coeff(1:4345) ! change to number of coefficients
  save coeff

contains
  !==================================!
  ! read the coefficients of the PES !
  !==================================!
  subroutine pes_init_hww()
    !::::::::::::::::::
    integer::i

    open(10,file='../coef/hww/coeff.dat',status='old')

    do i=1,size(coeff) 
       read (10,*) coeff(i)
    end do

    return
    close (10)
  end subroutine pes_init_hww

  !====================================!
  ! Function to evaluate the potential !
  !====================================!
  function f_hww(xh,xw) result(pot)
    real,dimension(:,:),intent(in)::xh,xw
    real::pot

    real,dimension(3,10)::xyz
    real::f
    !::::::::::::::::::::::::::::::
!    real,dimension(size(xyz,2)*(size(xyz,2)-1)/2)::x
    real,dimension(45)::x
    real,dimension(3)::dr
    real::a0  ! the same as the fitting code
    integer::i,j,k
    integer :: m,n,nw
    real::roo(3),maxoo,s  

    a0 = 2.5
    nw=size(xw,2)/3
    pot=0.d0

    do m=1,nw
       do n=m+1,nw
          xyz(:,1:4)=xh
          xyz(:,5:6)=xw(:,3*m-2:3*m-1)
          xyz(:,7:8)=xw(:,3*n-2:3*n-1)
          xyz(:,9)=xw(:,3*m)
          xyz(:,10)=xw(:,3*n)
          k = 1
          do i=1,size(xyz,2)-1
          do j=i+1,size(xyz,2)
             dr = xyz(:,i) - xyz(:,j)
             x(k) = sqrt(dot_product(dr,dr))
             k = k+1
          end do
          end do

          roo(1)=dsqrt(sum((xh(:,4)-xw(:,3*m))**2))
          roo(2)=dsqrt(sum((xh(:,4)-xw(:,3*n))**2))
          roo(3)=dsqrt(sum((xw(:,3*m)-xw(:,3*n))**2))
          maxoo=maxval(roo)

          call f_switch_3b(s,sum(roo)-maxoo)
          if (s.eq.0.d0) then
              pot=pot+0.d0
          else

            do i=1,size(x)
              x(i)=exp(-x(i)/a0)
            end do

            pot=pot+s*(emsav(x,coeff)+0.1/627.51)
          end if
       end do
     end do
    return
  end function f_hww

!==================================================
!switching functions for weights
!==================================================

subroutine f_switch_3b(s,r)
  real(kind=8),intent(out)::s
  real(kind=8),intent(in)::r
  real(kind=8)::ri,rf
  !::::::::::::::::::::
  real(kind=8)::ra,ra2,ra3


  ri=7.0/auang
  rf=8.0/auang
!   ri=7.5/auang
!   rf=9.5/auang

  if (r.lt.ri) then
    s=1.0
  else
     if (r.le.rf) then
        ra=(r-ri)/(rf-ri)
        ra2=ra*ra
        ra3=ra2*ra
        s=10.0*ra3-15.0*ra*ra3+6.0*ra3*ra2
        s=1-s
     else
        s=0.0
     end if
  end if
 return

end subroutine f_switch_3b
end module shell_hww
