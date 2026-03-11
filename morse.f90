module morse
  implicit none

contains
  subroutine get_morse(xyz,r,morse,a)
    real,dimension(:,:),intent(in)::xyz ! size (3,natom)
    real,dimension(size(xyz,1)*(size(xyz,1)-1)/2)::morse
    real,dimension(size(xyz,1),size(xyz,1))::r
    integer::i,j,k,natm
    real::a

    natm = size(xyz,1)

    k = 1
    do i=1,natm-1
       do j=1,natm
          r(i,j) = norm2(xyz(i,:)-xyz(j,:))
          r(j,i) = r(i,j)
          morse(k) = r(i,j)
          k = k + 1
       end do
    end do

    do i=1,size(morse)
       morse(i) = exp(-morse(i) / a)
    end do

    return
  end subroutine

end module morse
