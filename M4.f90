program Moments
real,dimension(1066):: x,y,z
integer,dimension(1066)::n
real::dx,dy,dz,r,theta,Yp,H,S,Ho,bjk,a,sum2,sum,M4
integer:: j,k,l,npoint
character*(20) gg, ch
npoint=1066
n=0
N=1066
! Yp is the geyromagnetic moment of proton
!hc is plank constant/2pi i.e. h cut
!S is spin
!N is number of Spin system
 Yp=2.6752218744E8
 H=1.05457182E-34
 S=0.5
 Ho=1
 
 open(7,file='H10K.xyz')
 read(7,*) npoint, boxl, ch

 open(3,file='bjl.txt')
open(4,file='bkl.txt')
do k=1, npoint
  read (7,*) gg, x(k), y(k), z(k)
end do
! rij=r it is the intermolecular distance from origion of cubical box.
!Hz=Hoi+Hoj+Hok applied magnetic field in x,y,z direction and applied magnetic field in z-direction ,Hz only is Ho
!Theta is angle between Applied magnetic field and intermolecular distance from origin.
! c=bjk**2 calculations of c
 !******************************** calculation for bjk ****************************************************
               sum2=0
               sum5=0                                              
               
                 do j=1,npoint-1
                     sum=0.0
                    do k=1, npoint
                     if(j/=k) then  
                     dx=x(j)-x(k)
                     dy=y(j)-y(k)
                     dz=z(j)-z(k)
                     r=sqrt(dx**2+dy**2+dz**2)
                     a=Ho*dx+Ho*dy+Ho*dz
                     theta=acos((dx/r)) !Magnetic field in (100) directions
                     !theta=acos((dx+dy)/r*sqrt(2.0)) !Magnetic field in (110) directions
                     !heta=acos((dx+dy+dz)/sqrt(3.0)) !Magnetic field in (111) directions
                     bjk=(1.0-3.0*(cos(theta))**2)/r**6          
                      sum=sum+bjk
                     
                      !print*,bjk
                     
                      end if
                      end do
                          sum2=sum2+sum   
                     
                 end do 
               
                
        
 !*****************Calculations for (bjl-bkl)**2 *************************************************
        sum3=0
       
                do j=1,npoint-1
                       do l=1, npoint
                    if(j/=l) then  
                    dx=x(j)-x(l)
                    dy=y(j)-y(l)
                    dz=z(j)-z(l)
                    r=sqrt(dx**2+dy**2+dz**2)
                    a=Ho*dx+Ho*dy+Ho*dz 
                   theta=acos((dx/r)) !Magnetic field in (100) directions
                    !theta=acos((dx+dy)/r*sqrt(2.0)) !Magnetic field in (110) directions
                     !heta=acos((dx+dy+dz)/sqrt(3.0)) !Magnetic field in (111) directions
                    bjl=(1.0-3.0*(cos(theta))**2)/r**6
                    !write(3,*) bjl
                    sum=sum+bjl
                    ! print*,sum3
                     sum3=sum3+sum
                            
                      end if
                      end do
                      end do
                       print*,sum3
                          sum4=0
                          
                                 sum=0
                                 do k=1,npoint-1
                                 do l=1,npoint
                                 if(k/=l) then  
                                   dx=x(k)-x(l)
                                dy=y(k)-y(l)
                                 dz=z(k)-z(l)
                                 r=sqrt(dx**2+dy**2+dz**2)
                                 a=Ho*dx+Ho*dy+Ho*dz
                               theta=acos((dz/r)) !Magnetic field in (100) directions
                               !theta=acos((dx+dy)/r*sqrt(2.0)) !Magnetic field in (110) directions
                               !theta=acos((dx+dy+dz)/sqrt(3.0)) !Magnetic field in (111) directions
                                bkl=(1.0-3.0*(cos(theta))**2)/r**6   
                                 write(4,*) bkl
                                end if
                                sum4=sum+bkl
                              
                                end do  
                               end do         
                              
 !M4 is 4th moment; calculation of fourth moment by van vleck formula
                                P=3*(sum2*sum3)**2/(16*1066)-(sum2*(sum3-sum4)**2)/(36*1066)
                                Q=sum2*sum4*(sum3-sum2)*(sum3-sum4)/(72*1066)+sum2**4/(16*1066)
                                M4=P+Q
                                print*,sum4
                              
						
 
end program Moments
