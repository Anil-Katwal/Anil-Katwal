program transport1
implicit none
real*8:: r
integer:: i, j, k, n, x(100), y(100), a(100, 100)
x=0
y=0
a=0
k=0
n=100
do i=1, n
    do j=1, n
        k=k+1
        a(i, j)= k
    end do
end do
k=0
!look at position a(3,1)
!i is row, j is column
i=3
j=1
do k=1, n
    call random_number(r)
    x(k)=ceiling(r*8)
    
    if (x(k)==1) then
        j=j+1
        y(k)=a(i,j)
    else if (x(k)==2) then
        i=i-1
        j=j+1
        y(k)=a(i,j)
    else if (x(k)==3) then
        i=i-1
        y(k)=a(i,j)
    else if (x(k)==4) then
        i=i-1
        j=j-1
        y(k)=a(i,j)
    else if (x(k)==5) then
        j=j-1
        y(k)=a(i,j)
    else if (x(k)==6) then
        i=i+1
        j=j-1
        y(k)=a(i,j)
    else if (x(k)==7) then
        i=i+1
        y(k)=a(i,j)
    else if (x(k)==8) then
        i=i+1
        j=j+1
        y(k)=a(i,j)
    end if
    if (i<1 .or. j<1) then
        print*, 'particle went out at step:', k, 'direction', x(k)
        exit
    end if
    print*, 'step:', k, '         direction:', x(k), '         location:', y(k)
end do
end program transport1
