       SUBROUTINE MATINV(A,N,ISING)
       DIMENSION A(40,40)
       ISING = 0
       DO 575 ICOL=1,N
       AMAX = A(ICOL,ICOL)
       IF(AMAX.EQ.0.0) ISING = ICOL
       IF(AMAX.EQ.0.0) RETURN 
       A(ICOL,ICOL) = 1.0  
       DO 350 L = 1,N
       A(ICOL,L) = A(ICOL,L)/AMAX
 350   CONTINUE
       DO 550 LL = 1,N
       IF(LL.EQ.ICOL) GO TO 550
       SWAP = A(LL,ICOL)
       A(LL,ICOL) = 0.0
       DO 450 L = 1,N
       A(LL,L) = A(LL,L) - A(ICOL,L)*SWAP
 450   CONTINUE
 550   CONTINUE
 575   CONTINUE
       RETURN
       END  






