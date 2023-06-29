!=========================================================================
!===================================== Program created by ==
!=================== Manoj Kumar Shrestha ==============================
!== Student, Central Department of Physics, Tribhuvan University, Nepal =====
! ========== Computational Physics ===================

!Monte-Carlo integration: using random numbers
!lower limit = 0
!upper limit = 1.0
!generated random number lies in between (0,1)


program MC_integration
implicit none
double precision, external :: f
double precision::a,b,x,sum1,avg, integral, real_integral, error
integer::i,n
a=0.d0
b=1.d0
n=200000000
sum1=0.d0
call srand(123456789) !srand(seed)
do i=1,n
x=rand()
sum1=sum1+f(x)
end do
avg = sum1/n
integral = (b-a)*avg
print*, 'The calculated integral value is =', integral

!for error calculation
real_integral= (b**3-a**3)/3.d0
error= (abs(real_integral-integral)/(real_integral))*100.d0
print*, 'Real integral =', real_integral
print*, 'error in Monte-Carlo method = ', error, '%.' 
end program

double precision function f(x)
double precision::x
f=x**2
end function



