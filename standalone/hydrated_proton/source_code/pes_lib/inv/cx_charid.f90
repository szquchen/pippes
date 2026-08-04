PURE SUBROUTINE cx_charid (nki, ch)
integer, intent (in) :: nki(0:)
character (len=*), intent (out) :: ch
!-----------------------------------------------------------------------
character, parameter :: &
  digits(0:9)=(/'0','1','2','3','4','5','6','7','8','9'/)
integer :: i, k
if (all(nki.eq.0)) then
 ch = ''
 return
endif
! identify trailing zeros
k = size(nki)
do while (nki(k-1).eq.0)
 k = k-1
enddo
ch = ''
do i = 0, min(len(ch),k)-1
 if (0.le.nki(i).and.nki(i).lt.10) then
  ch(i+1:i+1) = digits(nki(i))
 else
  ch(i+1:i+1) = 'x'
 endif
enddo
return
END SUBROUTINE cx_charid
