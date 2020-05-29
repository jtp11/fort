program tmp2
implicit none
real :: a(40,40), b(40,40), c(40,40)
integer :: n, ising, i, j, k
a(1,1) = 2
a(1,2) = 3
a(2,1) = 4
a(2,2) = 5
b = a
n = 2
do i=1,n
    print "(6f7.3)", a(i,1:n)
enddo   
call matinv(a,n,ising)
do i=1,n
    print "(6f7.3)", a(i,1:n)
enddo
do i=1,n
   do j=1,n
      do k=1,n
         c(i,j) = c(i,j) + a(i,k)*b(k,j)
      enddo
   enddo
enddo
do i=1,n
    print "(6f7.3)", c(i,1:n)
enddo
end
