program second
implicit none
real,dimension(1190):: x,y,z
real::dx1,dy1,dz1,r1,rmin,rmax
integer:: i,k,j,q(10),count, npoint, ind(1190), nsi, nhy
character(5)::ch2

rmin=0.0
rmax=9.0
npoint=160
open(unit = 11, file='04.txt')
read(11, *) npoint, nsi, nhy
!read(11, *) 
!read(11,*) 
do i = 1, 160
  read(11, *) ch2, x(i), y(i), z(i)
  ind(i)=0
  write(0,*) ch2, x(i), y(i), z(i)
end do
!print*, x(1066), y(1066), z(1066)
close(11)

open(100, file='cl6.txt')
open(13,file='group126.txt')
 do k=1,10
  count=0
  q(k) = Int(rand()*npoint-1)+nsi+1
  !print*,q(k)
 do i=1,npoint
    dx1=x(i)-x(q(k))
    dy1=y(i)-y(q(k))
    dz1=z(i)-z(q(k))
    r1=sqrt(dx1**2+dy1**2+dz1**2)

    if((r1.ge.rmin).and.(r1.le.rmax)) then
    
      count=count+1
      ind(i)= k
      write(100,*) x(i), y(i), z(i), ind(i)
    end if 
  end do
  write(13,*) k, count
end do  
  do j=1, npoint
   if (ind(j)==0) then
   write(100,*) x(j), y(j), z(j), ind(j)
   end if
   
 ! write(*,*)
 end do


end program second



