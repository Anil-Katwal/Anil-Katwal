      implicit double precision(a-h,o-z)
c
c     sigmael=cross-section for elastic scattering
c     sigmaab=cross-section for absorption
c     sigmatot=sigmael+sigmaab
c     mean free path; flambda=-ln c/sigmatot
c     xk=ran()
c     fmuk=2c-1
c     xkp1=xk+flambdak*fmuk
c     absorption condition; c < sigmaab/sigmatot
c     c=random number
c
c      print*,'Input number of particles'
c      read*,n
      n=1000000
      width=100.d0   ! *10e-15 meter
      sigmael=11.115d0
      sigmaab=0.171d0
      sigmatot=sigmaab+sigmael
      siabdsi=sigmaab/sigmatot
      fab=0.d0
      fback=0.d0
      fout=0.d0
      xkp1=0.d0
      do i=1,n
      call random_number(c0)
         c1=c0
         flambdak=-dlog(c1)/sigmatot
      call random_number(c2)
c         c2=ran()
         fmuk=2.d0*c2-1.d0
         if(c1.lt.siabdsi)then
c     print*,'absorped',xkp1
            fab=fab+1.d0
            goto 1000
         else
            xkp1=xkp1+(flambdak*fmuk)
            if(xkp1.gt.width)then
c               print*,'got through',xkp1
               fout=fout+1.d0
               goto 1000
            elseif(xkp1.lt.0.d0)then
c               print*,'went back',xkp1
               fback=fback+1.d0
               goto 1000
            else
            endif
         endif
 1000 enddo
c      goto 2000
      pab=fab/dfloat(n)
      pout=fout/dfloat(n)
      pback=fback/dfloat(n)
      print*,'Probability of coming out=',pout
      print*,'probability of get absorped=',pab
     print*,'Probability of going back=',pback 
  200 end 
         
