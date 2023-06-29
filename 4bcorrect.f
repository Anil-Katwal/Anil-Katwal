       implicit double precision(a-h,o-z)
      dimension ipoint(30),maxl(30,30),ib(30,10,30),ifinal(30,10,30)
      dimension isum(1100),isum2(1100),isum3(1100),isum4(1100)
      dimension isum5(1100),ia(30),iib(30)

      open(1,file='map.txt',status='old')

      read(1,*)n
      do i=1,n
      read(1,*)ipoint(i),maxl(i,1),(ib(i,1,j),j=1,maxl(i,1))
      enddo

      do i=1,n
      print *,ipoint(i),maxl(i,1),(ib(i,1,j),j=1,maxl(i,1))
      enddo

      print *,'Put Order of neighbor'
      read *,inhop

            m=1
       do while   (m.lt.inhop)
      do 991 i=1,n
         k=1
      do  992 j=1,maxl(i,m)

         
         do 993 jj=1,maxl(ib(i,m,j),1)

            itest=ib(ib(i,m,j),1,jj)
            do 994 ihopp=1,m
               do 995 jjj=1,maxl(i,ihopp)

                  if(itest.eq.i.or.itest.eq.ib(i,ihopp,jjj))goto 104

 995           continue

 994           continue
             if(itest.eq.i) goto 102
              ib(i,m+1,k)=ib(ib(i,m,j),1,jj)
              k=k+1
 104          continue

 993          continue

 992       continue

           maxl(i,m+1)=k-1

 991        continue


      do i=1,n
         nmax=maxl(i,m+1)
            do jjj=1,maxl(i,m+1)
               ia(jjj)=ib(i,m+1,jjj)
            enddo            
            call bubblesort(nmax,ia,iib,idum)
            print *,'th neb.=',m+1,'numbers=',i
          ! print *, ' Vertex=',i
            print* ,' neighbors=',(iib(j),j=1,idum)
            print *,' '
            print *,' '
         enddo
         do i=1,n
            do jjj=1,idum
               ifinal(i,m+1,jjj)=iib(jjj)
            enddo
            enddo
         
         print *,' '
         m=m+1
      enddo


         stop
         end
c*********************************************************************
c*******************************************************************
      subroutine links(n,near)
      implicit double precision(a-h,o-z)
      dimension x(1100),y(1100),z(1100),icount(1500),distance(1100,1100)
      dimension near(1100,1100)
      Read*,n

       flx=28.d0
       fly=28.d0
       flz=28.d0
       frc=2.d0
       frc2=frc*frc
       R=3.
    
       npt=0
       x(1)=ran()*flx
       y(1)=ran()*fly
       z(1)=ran()*flz

       
       i=1

       do while(npt.le.n-1)
 10       continue
       xx=ran()*flx
       yy=ran()*fly
       zz=ran()*flz

       icount(i)=0
       do k=1,i
          dis2=(xx-x(k))**2+(yy-y(k))**2+(zz-z(k))**2
         
       if(dis2.lt.frc2)then
          goto 10
       else
          distance(i,k)=dsqrt(dis2)

          idis=dsqrt(dis2)+1
          icount(idis)=icount(idis)+1         
       endif
      
       enddo
       
          x(i+1)=xx
          y(i+1)=yy
          z(i+1)=zz
          npt=npt+1
          i=i+1
       enddo
       itotal=i

       do i=1,5
!          write(3,40)i,icount(i)
       enddo

 

       do i=1,n

       enddo


       do i=1,itotal
          do j=1,itotal
             if(distance(i,j).lt.R.and.distance(i,j).gt.frc)then
                near(i,j)=1
             else
                near(i,j)=0
                endif
             enddo
          enddo

c
          do i=1,itotal
             do j=1,itotal
                if(near(i,j).ne.0)then
                print *,'i ,j ,near(i,j)', i,j,near(i,j),distance(i,j)
             else
             endif
          enddo
       enddo
       print *,'****************************************'
       print *,'sssssssssssssssssssssss'
       print *,'****************************************'
      end
c
c*********************************************************************
      subroutine BubbleSort(nmax,ia,iib,idum)
      implicit double precision(a-h,o-z)
       integer ia(30),iib(30)

      nn=nmax-1
      do 100 j=1,nn
         L=j
         jj=j+1
         do 200 i=jj,nmax
            if(ia(L).lt.ia(i)) go to 200
            L=i
 200     continue


         t=ia(L)
         ia(L)=ia(j)
         ia(j)=t
 100  continue

      idum=1
      do i=1,nn
         if(ia(i).ne.ia(i+1))then
            iib(idum)=ia(i)
            idum=idum+1
         else
         endif
      enddo
 
         iib(idum)=ia(nmax)
      
      return
      end
c*******************************************************************
