program poker 
implicit none
real*8::r
integer:: x(10000000), y(10000000), a(10000000), b(10000000), c(10000000)
integer:: n, i,  p, pp
n=10000000
p=0
pp=0
! 1 to 13 spades 
!14 to 26 Hearts 
!28 to 40 clubs 
!40 to 52 diamonds 
print*, '1 to 13 spades'
print*,' 14 to 26 Hearts '
print*,'28 to 40 clubs'
print*,'40 to 52 diamonds'
do i=1, n
    call random_number(r)
    x(i)=ceiling(r*52)
    print*,x(i)
    10 call random_number(r)
    y(i)=ceiling(r*52)
    if (x(i)==y(i)) then
        goto 10
    end if
    19 call random_number(r)
    a(i)=ceiling(r*52)
    21 call random_number(r)
    b(i)=ceiling(r*52)
    23 call random_number(r)
    c(i)=ceiling(r*52)
    if (a(i)==x(i) .or. a(i)==y(i)) then
        goto 19
    elseif (b(i)==x(i) .or. b(i)==y(i) .or. b(i)==a(i)) then  
        goto 21
    elseif (c(i)==x(i) .or. c(i)==y(i) .or. c(i)==a(i) .or. c(i)==b(i)) then
        goto 23 
    end if
    !pair
    if (mod(x(i)-y(i), 13)==0 .or. mod(x(i)-a(i), 13)==0 .or. mod(x(i)-b(i), 13)==0 .or. mod(x(i)-c(i), 13)==0) then
        p=p+1
    elseif (mod(y(i)-a(i), 13)==0 .or. mod(y(i)-b(i), 13)==0 .or. mod(y(i)-c(i), 13)==0) then
        p=p+1
    elseif (mod(a(i)-b(i), 13)==0 .or. mod(a(i)-c(i), 13)==0) then
        p=p+1
    elseif (mod(b(i)-c(i), 13)==0) then
        p=p+1
    end if
    !triple
    if (mod(x(i)-y(i), 13)==0 .and. mod(x(i)-a(i), 13)==0) then
        pp=pp+1
    elseif (mod(x(i)-y(i), 13)==0 .and. mod(x(i)-b(i), 13)==0) then
        pp=pp+1
    elseif (mod(x(i)-y(i), 13)==0 .and. mod(x(i)-c(i), 13)==0) then
        pp=pp+1
    elseif (mod(y(i)-a(i), 13)==0 .and. mod(y(i)-b(i), 13)==0) then
        pp=pp+1
    elseif (mod(y(i)-a(i), 13)==0 .and. mod(y(i)-c(i), 13)==0) then
        pp=pp+1
    elseif (mod(a(i)-b(i), 13)==0 .and. mod(a(i)-c(i), 13)==0) then
        pp=pp+1
    end if
end do
print*, 'probability of a pair being dealt is:', real(p)/real(n) 
print*, 'probablity of three of a kind being dealt is:', real(pp)/real(n) 
end program poker
