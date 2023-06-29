program randomchord
implicit none
real*8 :: x1, y1, x2, y2, r, theta1, theta2, pi, h, k, d, test
integer ::i, n
pi=2.d0*asin(1.d0)
n=100000
r=1
!center at (h,k)=(0,0)
h=0
k=0
open(unit=1, file='chordallengths.dat')
do i=1, n
    call random_number(theta1)
    call random_number(theta2)
    theta1=theta1*2*pi
    theta2=theta2*2*pi
    x1=h+cos(theta1)*r
    y1=k+sin(theta1)*r
    x2=h+cos(theta2)*r
    y2=k+sin(theta2)*r
    d=sqrt((x1-x2)**2+(y1-y2)**2)
    test=sqrt(r**2-((1.d0/4.d0)*(d**2)))
    if (test>=(r/2.d0)) then
        write(1,*) d
    end if
end do
end program randomchord
