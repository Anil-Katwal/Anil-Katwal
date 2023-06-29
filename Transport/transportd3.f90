program transport
implicit none
real*8 ::r, s
integer ::i, j, k, l, n, h, o, p, x
x=0
n=20
h=5
o=0
p=0
do l=1, 100000
    i=4
    j=4
    k=4
    do
        call random_number(r)
        x=ceiling(r*27)
        call random_number(s)
        if (s<0.25) then
            !no movement in k direction
            if (x==1) then
                j=j+1
            else if (x==2) then
                i=i-1
                j=j+1
            else if (x==3) then
                i=i-1
            else if (x==4) then
                i=i-1
                j=j-1
            else if (x==5) then
                j=j-1
            else if (x==6) then
                i=i+1
                j=j-1
            else if (x==7) then
                i=i+1
            else if (x==8) then
                i=i+1
                j=j+1
        else if (s>0.25 .and. s<0.75) then
            !can move in k direction
            else if (x==9) then
                k=k+1
        else if (s>0.75) then
            else if (x==10) then
                i=i+1
                k=k+1
            else if (x==11) then
                j=j+1
                k=k+1
            else if (x==12) then
                i=i+1
                j=j+1
                k=k+1
            else if (x==13) then
                i=i-1
                k=k+1
            else if (x==14) then
                j=j-1
                k=k+1
            else if (x==15) then
                i=i-1
                j=j-1
                k=k+1
            else if (x==16) then
                i=i-1
                j=j+1
                k=k+1
            else if (x==17) then
                i=i+1
                j=j-1
                k=k+1
            else if (x==18) then
                i=i-1
                j=j-1
                k=k+1
        else if (s>0.75) then
            !goes back in k direction
            else if (x==19) then
                i=i+1
                k=k-1
            else if (x==20) then
                j=j+1
                k=k-1
            else if (x==21) then
                i=i+1
                j=j+1
                k=k-1
            else if (x==22) then
                i=i-1
                k=k-1
            else if (x==23) then
                j=j-1
                k=k-1
            else if (x==24) then
                i=i-1
                j=j-1
                k=k-1
            else if (x==25) then
                k=k-1
            else if (x==26) then
                i=i+1
                j=j-1
                k=k-1
            else if (x==27) then
                i=i-1
                j=j+1
                k=k-1
            end if
        end if
            if (i<1 .or. j<1 .or. k<1 .or. i>300 .or. j>300) then
                o=o+1
                exit
            end if
            if (k>h) then
                p=p+1
                exit
            end if
    end do
end do
print*,'probability of particle escaping the lattice without going through', real(o)/real(100000)
print*, 'probability of particle going through the lattice', real(p)/real(100000)
end program transport
