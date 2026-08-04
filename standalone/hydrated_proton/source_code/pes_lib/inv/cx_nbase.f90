PURE FUNCTION cx_nbase (nki, sys, pcv) RESULT (nb)
integer, intent (in) :: nki(0:)
character (len=*), intent (in) :: sys
type (cx_t), intent (in) :: pcv(2:)
integer :: nb
!-----------------------------------------------------------------------
integer :: nkj(0:size(nki)-1), order, ib, i, k, l
logical :: done
character (len=size(nki)) :: charid
! initialize counter ib
ib = 0
! One-body terms
do i = 0, size(nki)-1
 if (1.le.nki(i)) then
  nkj = 0 ; nkj(i) = 1
  call cx_charid (nkj, charid)
  if (cx_substr(sys,charid)) then
   ib = ib+1
  endif
 endif
enddo
! Further terms grevlex
do order = 2, min(sum(nki),ubound(pcv,dim=1))
 if (0.le.pcv(order)%dg) then
  k = order
  do i = 0, size(nki)-1
   nkj(i) = min(k,nki(i))
   k = k-nkj(i)
  enddo
  done = .false.
  do while (.not.done)
   call cx_charid (nkj, charid)
   if (cx_substr(sys,charid)) then
    ib = ib+cx_nbx(nkj,pcv(order)%dg)
   endif
! set next nkj
   l = size(nki)
   do i = size(nki)-1, 0, -1
    if (nkj(i).lt.nki(i).and.sum(nkj(i:)).lt.order) then
     l = i
    endif
   enddo
   if (l.lt.size(nki)) then
    nkj(l) = nkj(l)+1
    k = order-sum(nkj(l:))
    do i = 0, l-1
     nkj(i) = min(k,nki(i))
     k = k-nkj(i)
    enddo
    done = .false.
   else
    done = .true.
   endif
  enddo
 endif
enddo
nb = ib
return
END FUNCTION cx_nbase
