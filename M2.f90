program Moments
real,dimension(1066):: x,y,z
integer,dimension(1066)::n
real::dx,dy,dz,r,theta,Yp,hc,S,Ho,bjk,a,d,sum2,sum,M2
integer:: j,k,npoint
character*(20) gg, ch
npoint=1066
n=0
N=1000
! Yp is the geyromagnetic moment of proton
!hc is plank constant/2pi i.e. h cut
!S is spin
!N is number of Spin system
 Yp=2.6752218744E8
 hc=1.05457182E-34
 S=0.5
 Ho=1
 d=10.0
open(7,file='H10K.xyz')
read(7,*) npoint, boxl, ch
open(5, file='xyz.txt')
 open(3,file='bjl.txt')
open(4,file='bkl.txt')
do k=1, npoint
  !read (5,*) gg, x(k), y(k), z(k)
  read (7,*) gg, x(k), y(k), z(k)
end do
! rij=r it is the intermolecular distance from origion of cubical box.
!Hz=Hoi+Hoj+Hok applied magnetic field in x,y,z direction and applied magnetic field in z-direction ,Hz only is Ho
!Theta is angle between Applied magnetic field and intermolecular distance from origin.
! c=bjk**2 calculations of c
 !******************************** calculation for bjk ****************************************************
               sum2=0
                 do j=1,npoint-1
                     sum=0.0
                    do k=1, npoint
                     if(j/=k) then  
                     dx=x(j)-x(k)
                     dy=y(j)-y(k)
                     dz=z(j)-z(k)
                     r=sqrt(dx**2+dy**2+dz**2)
                     a=Ho*dx+Ho*dy+Ho*dz
                     theta=acos((dz/r)) !Magnetic field in (100) directions
                     !theta=acos((dx+dy)/r*sqrt(2.0))!Magnetic field in (110) directions
                     !theta=acos((dx/r+dy/r+dz/r)) !Magnetic field in (111) directions
                     bjk=((1.0-3.0*(cos(theta))**2)**2)/r**6
                     !write(0,*) bjk
                                
                      sum=sum+bjk
                     ! print*,bjk
                      end if
                      end do
                          sum2=(sum2+sum)**2
                     ! print*,sum
                 end do 
                ! print*, sum2   
        
 !M2 is second moment; calculation of second moment by van vleck formula

                         M2=Yp**4*hc**2*sum2
                         print*,sum2
 
end program Moments
