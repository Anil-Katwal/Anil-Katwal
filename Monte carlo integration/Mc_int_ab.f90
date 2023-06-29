
program MC_integration
implicit none
double precision, external::f
double precision::a,b,x,y,sum1,avg,integral, real_integral,error
integer::i,n
write(*,*) 'Enter the value of lower limit'
read*,a
write(*,*)'Enter the value of upper limit'
read*, b
write(*,*)'Enter the number of random numbers to be generated'
read*, n
call srand(123456789)
sum1=0.d0
do i =1,n
x=rand()
y= a+x*(b-a)  !y = a+x+x*(b-(a+1))
print*, x, y
sum1=sum1+f(y)
end do
avg=sum1/n
integral=(b-a)*avg
print*, ' the calculated integral value =', integral

!for error calculation
real_integral = (b**3-a**3)/3.d0
error=abs(real_integral-integral)/real_integral*100.d0 
print*, 'real integral value = ', real_integral
print*, 'error in Monte carlo method =', error, '%'
end program

double precision function f(x)
double precision::x
f=(1/(1+x**20))*(1/(1+x**2))
end function





