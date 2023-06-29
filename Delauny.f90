

      module coordinate
       implicit none
       integer, parameter :: N = 100
       real, dimension(N) :: x, y
       real, dimension(N, 0:20) :: xc, yc
       integer, dimension(N) :: nn
       integer, dimension(N,0:20) :: clist
       save
       end module

!*********************************************

       program dt
       use coordinate
       implicit none 

       integer  :: ij, i, j, k, ik, nk
       real     :: angle
       real, dimension(:), allocatable     :: m, xt,yt
       real   :: factor
       real   :: xo,yo

       real, parameter :: pi = 3.141593

       nn = 0

       open(1, file = 'randomnumber.txt', status = 'old')
       
       
       do ij = 1, N
          read(1,*) x(ij), y(ij)
       end do
       close(1)

       do i = 1, N-2
          do j = i+1, N-1
             do k = j+1, N
                call colinear(i,j,k, angle)
                !write(*,*) i, j, k, angle
                if(angle > 20 .and. angle < 150) then
                   call delauny(i,j, k)
                   
                end if
             end do
          end do
       end do
!!

       end program dt

!**************************************************************
! Sorting the angle 

       subroutine colinear(ii,jj,kk,angle)
       use coordinate
       implicit none
       integer, intent(IN) :: ii, jj, kk
       real                :: rx1, ry1, rx2, ry2, r1, r2
       real                :: A, factor
       real, intent(OUT)   :: angle
       real, parameter :: pi = 3.141593

        factor = 180./pi
       
        rx1 = x(ii) - x(jj)
        ry1 = y(ii) - y(jj)
        r1 = rx1**2 + ry1**2

        rx2 = x(kk) - x(jj)
        ry2 = y(kk) - y(jj)
        r2 = rx2**2 + ry2**2

       A = (rx1*rx2 + ry1*ry2)/(sqrt(r1*r2))
       angle = acos(A)*factor

       return 
       end subroutine

!*******************************************************************


       subroutine delauny(ii, jj, kk)
       use coordinate
       implicit none

       integer, intent(IN) :: ii, jj, kk
       integer             :: il
       real, dimension(2)  :: A, B, C
       real                :: xo, yo, Dx, Dy, D, r, dr
!Calculation of Radius and Centre usning random points.
!Solving four equations to find the centre and radius 
       A(1) = 2*(x(jj)-x(ii))
       B(1) = 2*(y(jj)-y(ii))
       C(1) = (x(jj)**2 - x(ii)**2) + (y(jj)**2 - y(ii)**2)

       A(2) = 2*(x(kk)-x(jj))
       B(2) = 2*(y(kk)-y(jj))
       C(2) = (x(kk)**2 - x(jj)**2) + (y(kk)**2 - y(jj)**2)

       Dx = B(2)*C(1) - B(1)*C(2)
       Dy = C(2)*A(1) - C(1)*A(2)
       D = A(1)*B(2) - A(2)*B(1)

       xo = Dx/D
       yo = Dy/D

       r = sqrt((x(ii)-xo)**2 + (y(ii)-yo)**2)

       do il = 1, N
          if((il /= ii).and.(il /= jj).and.(il /= kk)) then
             dr = (x(il)-xo)**2 + (y(il)-yo)**2 - r**2
                if(dr <= 0.) then
                  go to 1000
                end if
          end if
       end do
       write(111,*)
       write(111,'(2F12.5)') x(ii), y(ii)
       write(111,'(2F12.5)') x(jj), y(jj)
       write(111,'(2F12.5)') x(kk), y(kk)
       write(111,'(2F12.5)') x(ii), y(ii)
       write(111, *) 
       write(120,'(3F12.5)') xo, yo, r
 


      1000 continue
       return
       end subroutine
!****************************************

       
