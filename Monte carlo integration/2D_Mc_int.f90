
program MC_2Dintegral
implicit none
double precision, external::f
double precision:: a,b,c,d,x,y,r,sum1,avg,integral, real_integral, error
integer::i,n
write(*,*)'Insert lower and upper limits of first variable'
read*, a,b
write(*,*)'Insert lower and upper limits of second variable'
read*, c,d
write(*,*)'Insert number of random numbers to be generated'
read*,n
call srand(123456789)
sum1=0.d0
do i =1,n
r=rand()
x=a+r*(b-a)
y=c+r*(d-c)
sum1=sum1+f(x,y)
end do
avg=sum1/n
integral=(b-a)*(d-c)*avg
print*, 'Calculated integral value = ', integral

!for error
real_integral= (b**3+d**3)/3.d0-(a**3+c**3)/3.d0
error = abs(real_integral-integral)/real_integral*100.d0
print*, 'real integral value is =', real_integral
print*, 'Error in method =', error, '%'
end program

double precision function f(x,y)
double precision::x,y
f=x**2+y**2
end function





